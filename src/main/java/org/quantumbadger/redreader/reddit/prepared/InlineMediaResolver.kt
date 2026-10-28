/*******************************************************************************
 * This file is part of RedReader.
 *
 * RedReader is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * RedReader is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with RedReader.  If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/

package org.quantumbadger.redreader.reddit.prepared

import org.quantumbadger.redreader.common.UriString
import org.quantumbadger.redreader.reddit.kthings.ImageMetadata
import org.quantumbadger.redreader.reddit.kthings.MaybeParseError
import org.quantumbadger.redreader.reddit.kthings.RedditMediaMetadata
import org.quantumbadger.redreader.reddit.kthings.UrlEncodedString
import java.util.regex.Pattern

/**
 * An image or GIF which Reddit has attached to a comment.
 */
data class InlineMedia(
	val type: Type,

	// The size of the media itself. If the size isn't known, this is a placeholder, and the
	// size of the still image is used once it has loaded.
	val width: Int,
	val height: Int,

	// Still images to display the media with, in order of increasing width. For animated
	// media, these are frames of the animation.
	val stills: List<Still>,

	val isSizeKnown: Boolean = true
) {
	enum class Type {
		IMAGE, ANIMATED
	}

	data class Still(
		val url: UriString,
		val width: Int,
		val height: Int
	)

	/**
	 * Returns the smallest still which is at least the given width, or the largest still if
	 * none are that wide.
	 */
	fun chooseStill(minWidthPx: Int): Still =
		stills.firstOrNull { it.width >= minWidthPx } ?: stills.last()
}

/**
 * Finds the media which a link in a comment points to, out of the media which Reddit has
 * attached to that comment in its "media_metadata". Reddit puts images and GIFs into the body
 * of a comment as links:
 *
 * - Uploaded images and GIFs link to "https://preview.redd.it/<id>.<extension>", where <id>
 *   is the key of the media in the metadata.
 * - GIFs from Giphy link to "https://giphy.com/gifs/<id>", and their key is "giphy|<id>" or
 *   "giphy|<id>|downsized".
 *
 * Reddit no longer gives apps the details of GIFs from Giphy: their metadata is just
 * {"status": "invalid"}. If loadGiphyDirectly is set, these are instead displayed using a
 * still image from Giphy itself, which means a request to Giphy for every one displayed.
 *
 * Links to media which isn't attached to the comment aren't resolved, as there's no way to
 * know what they point to without downloading them.
 */
class InlineMediaResolver @JvmOverloads constructor(
	mediaMetadata: Map<UrlEncodedString, MaybeParseError<RedditMediaMetadata>>?,
	private val loadGiphyDirectly: Boolean = false
) {
	private val media = HashMap<String, InlineMedia>()
	private val mediaByExternalUrl = HashMap<String, InlineMedia>()

	// Every GIF from Giphy which is attached to the comment, whether Reddit sent its details
	// or not
	private val attachedGiphyIds = HashSet<String>()

	init {
		mediaMetadata?.forEach { (key, value) ->

			giphyKeyPattern.matcher(key.decoded).let {
				if (it.matches()) {
					attachedGiphyIds.add(it.group(1)!!)
				}
			}

			val metadata = (value as? MaybeParseError.Ok)?.value ?: return@forEach
			val inlineMedia = toInlineMedia(metadata) ?: return@forEach

			media[key.decoded] = inlineMedia
			metadata.ext?.decoded?.let { mediaByExternalUrl[it] = inlineMedia }
		}
	}

	fun isEmpty() = media.isEmpty() && (!loadGiphyDirectly || attachedGiphyIds.isEmpty())

	/**
	 * Returns null if the link doesn't point to media attached to this comment.
	 */
	fun resolve(href: String): InlineMedia? {

		redditMediaPattern.matcher(href).let {
			if (it.matches()) {
				return media[it.group(1)!!]
			}
		}

		giphyPattern.matcher(href).let {
			if (it.matches()) {
				val id = it.group(1)!!

				return media["giphy|$id"]
					?: media["giphy|$id|downsized"]
					?: mediaByExternalUrl[href]
					?: if (loadGiphyDirectly && attachedGiphyIds.contains(id)) {
						giphyStill(id)
					} else {
						null
					}
			}
		}

		return null
	}

	companion object {

		private val redditMediaPattern: Pattern = Pattern.compile(
			"^https?://(?:preview|i)\\.redd\\.it/(\\w+)\\.\\w+(?:[?#].*)?$"
		)

		private val giphyKeyPattern: Pattern = Pattern.compile("^giphy\\|(\\w+)(?:\\|.*)?$")

		// Giphy's still images of this name are this many pixels wide
		private const val GIPHY_STILL_WIDTH_PX = 200

		/**
		 * The first frame of a GIF from Giphy, scaled to a fixed width. Its height isn't known
		 * until it has been downloaded.
		 */
		private fun giphyStill(id: String) = InlineMedia(
			type = InlineMedia.Type.ANIMATED,
			width = GIPHY_STILL_WIDTH_PX,
			height = GIPHY_STILL_WIDTH_PX,
			stills = listOf(
				InlineMedia.Still(
					url = UriString("https://media.giphy.com/media/$id/200w_s.gif"),
					width = GIPHY_STILL_WIDTH_PX,
					height = GIPHY_STILL_WIDTH_PX
				)
			),
			isSizeKnown = false
		)

		// Giphy page URLs may have a description before the ID, separated by dashes
		private val giphyPattern: Pattern = Pattern.compile(
			"^https?://(?:www\\.)?giphy\\.com/gifs/(?:[\\w-]*-)?(\\w+)/?(?:[?#].*)?$"
		)

		private fun Long.toPositiveIntOrNull() =
			if (this > 0 && this <= Int.MAX_VALUE) toInt() else null

		private fun toStill(image: ImageMetadata, url: UrlEncodedString?): InlineMedia.Still? {
			return InlineMedia.Still(
				url = UriString(url?.decoded ?: return null),
				width = image.x.toPositiveIntOrNull() ?: return null,
				height = image.y.toPositiveIntOrNull() ?: return null
			)
		}

		private fun toInlineMedia(metadata: RedditMediaMetadata): InlineMedia? {

			if (metadata.status != "valid") {
				return null
			}

			val type = when (metadata.e) {
				"Image" -> InlineMedia.Type.IMAGE
				"AnimatedImage" -> InlineMedia.Type.ANIMATED
				else -> return null
			}

			val source = metadata.s

			// Animated media might not have a still image at full size, in which case the GIF
			// itself is used: decoding it only produces the first frame.
			val stills = (metadata.p.orEmpty().map { toStill(it, it.u) }
					+ toStill(source, source.u ?: source.gif))
				.filterNotNull()
				.distinctBy { it.url }
				.sortedBy { it.width }

			if (stills.isEmpty()) {
				return null
			}

			return InlineMedia(
				type = type,
				width = source.x.toPositiveIntOrNull() ?: return null,
				height = source.y.toPositiveIntOrNull() ?: return null,
				stills = stills
			)
		}
	}
}
