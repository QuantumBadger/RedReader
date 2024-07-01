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

package org.quantumbadger.redreader.image

import androidx.compose.runtime.Immutable
import org.apache.commons.text.StringEscapeUtils
import org.quantumbadger.redreader.common.UriString
import org.quantumbadger.redreader.jsonwrap.JsonObject
import org.quantumbadger.redreader.reddit.kthings.ImageMetadata
import org.quantumbadger.redreader.reddit.kthings.MaybeParseError
import org.quantumbadger.redreader.reddit.kthings.RedditPost
import kotlin.math.min

@Immutable
class AlbumInfo(
	@JvmField val url: UriString,
	@JvmField val title: String?,
	val description: String?,
	images: List<ImageInfo>
) {
	@JvmField
	val images = ArrayList(images)

	companion object {
		@JvmStatic
		fun parseImgur(
			url: UriString,
			obj: JsonObject
		): AlbumInfo {
			var title = obj.getString("title")
			var description = obj.getString("description")

			if (title != null) {
				title = StringEscapeUtils.unescapeHtml4(title)
			}

			if (description != null) {
				description = StringEscapeUtils.unescapeHtml4(description)
			}

			val imagesJson = obj.getArray("images")
			val images = ArrayList<ImageInfo>()

			for (imageJson in imagesJson!!) {
				images.add(ImageInfo.parseImgur(imageJson.asObject()))
			}

			return AlbumInfo(url, title, description, images)
		}

		@JvmStatic
		fun parseImgurV3(
			url: UriString,
			obj: JsonObject
		): AlbumInfo {
			var title = obj.getString("title")
			var description = obj.getString("description")

			if (title != null) {
				title = StringEscapeUtils.unescapeHtml4(title)
			}

			if (description != null) {
				description = StringEscapeUtils.unescapeHtml4(description)
			}

			val imagesJson = obj.getArray("images")
			val images = ArrayList<ImageInfo>()

			for (imageJson in imagesJson!!) {
				images.add(ImageInfo.parseImgurV3(imageJson.asObject()))
			}

			return AlbumInfo(url, title, description, images)
		}

		private fun stringToMediaType(mediaTypeString: String?): ImageInfo.MediaType {
			return if (mediaTypeString == null) {
				ImageInfo.MediaType.IMAGE
			} else {
				when (mediaTypeString) {
					"AnimatedImage" -> ImageInfo.MediaType.GIF
					// This string doesn't seem to exist yet, but it might do in future
					"Video" -> ImageInfo.MediaType.VIDEO
					"Image" -> ImageInfo.MediaType.IMAGE
					else -> ImageInfo.MediaType.IMAGE
				}
			}
		}

		private fun getPreview(
			minSizePx: Int,
			images: List<ImageMetadata>
		): ImageUrlInfo? {

			var bestSizeMinAxis: Long? = null
			var bestUrl: String? = null
			var bestSize: ImageSize? = null

			for (image in images) {

				if (image.u == null) {
					continue
				}

				val x = image.x
				val y = image.y

				val minAxis = min(x, y)

				if (bestSizeMinAxis == null || (bestSizeMinAxis < minSizePx && minAxis > bestSizeMinAxis)
					|| (minAxis >= minSizePx && minAxis < bestSizeMinAxis)
				) {
					bestSizeMinAxis = minAxis
					bestUrl = image.u.decoded
					bestSize = ImageSize.from(image.x, image.y)
				}
			}

			return bestUrl?.let {
				ImageUrlInfo(
					UriString(bestUrl),
					size = bestSize
				)
			}
		}

		fun parseRedditGallery(post: RedditPost): AlbumInfo? {

			val galleryItems = post.gallery_data?.items ?: return null

			val images = galleryItems.mapNotNull { (it as? MaybeParseError.Ok)?.value }
				.mapNotNull { item ->

					val mediaMetadataEntry =
						(post.media_metadata?.get(item.media_id) as? MaybeParseError.Ok)?.value
							?: return@mapNotNull null

					val standardImage = mediaMetadataEntry.s

					val mediaType = stringToMediaType(mediaMetadataEntry.e)

					val urlEscaped = standardImage.u ?: standardImage.mp4 ?: standardImage.gif

					val original = urlEscaped?.let {
						ImageUrlInfo(
							url = UriString(urlEscaped.decoded),
							size = ImageSize.from(standardImage.x, standardImage.y)
						)
					}

					if (original == null) {
						throw RuntimeException("url missing from response")
					}

					var bigSquare: ImageUrlInfo? = null
					var preview: ImageUrlInfo? = null

					mediaMetadataEntry.p?.let { p ->
						val images = p + listOf(standardImage)

						bigSquare = getPreview(minSizePx = 300, images = images)
						preview = getPreview(minSizePx = 1400, images = images)
					}

					ImageInfo(
						original = original,
						preview = preview,
						bigSquare = bigSquare,
						title = item.caption?.decoded,
						outboundUrl = UriString.fromNullable(
							item.outbound_url?.decoded?.trim()?.takeUnless { it.isEmpty() }),
						type = mediaMetadataEntry.m,
						isAnimated = mediaType != ImageInfo.MediaType.IMAGE,
						mediaType = mediaType,
						hasAudio = ImageInfo.HasAudio.MAYBE_AUDIO,
					)
				}.toList()

			val title = post.title?.decoded

			return AlbumInfo(post.findUrl()!!, title, null, images)
		}
	}
}
