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
package org.quantumbadger.redreader.reddit

import androidx.annotation.StringRes
import org.quantumbadger.redreader.R
import org.quantumbadger.redreader.common.LinkHandler
import org.quantumbadger.redreader.reddit.kthings.MaybeParseError
import org.quantumbadger.redreader.reddit.kthings.RedditMediaMetadata
import org.quantumbadger.redreader.reddit.kthings.RedditPost
import java.util.Locale

enum class PostFilter(@param:StringRes val menuTitle: Int) {
	ALL(R.string.filter_posts_all),
	IMAGE(R.string.filter_posts_image),
	VIDEO(R.string.filter_posts_video),
	LINK(R.string.filter_posts_link),
	TEXT(R.string.filter_posts_text);

	fun matches(post: RedditPost): Boolean = this == ALL || classify(post) == this

	companion object {
		private val videoDomains = setOf(
			"v.redd.it",
			"gfycat.com",
			"redgifs.com",
			"streamable.com",
			"giphy.com",
			"youtube.com",
			"youtu.be",
			"vimeo.com")

		@JvmStatic
		fun classify(post: RedditPost): PostFilter {
			if (post.is_self) {
				return TEXT
			}

			if (isVideo(post)) {
				return VIDEO
			}

			if (post.gallery_data != null) {
				return if (hasAnimatedGalleryMedia(post)) VIDEO else IMAGE
			}

			if (post.post_hint.equals("image", ignoreCase = true)
				|| LinkHandler.isProbablyAnImage(post.findUrl())) {
				return IMAGE
			}

			return LINK
		}

		private fun isVideo(post: RedditPost): Boolean {
			if (post.is_video || post.media?.reddit_video != null) {
				return true
			}

			val postHint = post.post_hint?.lowercase(Locale.ROOT)
			if (postHint == "hosted:video"
				|| postHint == "rich:video"
				|| postHint == "video") {
				return true
			}

			val preview = post.preview
			if (preview?.reddit_video_preview != null
				|| preview?.images?.any { it.variants.mp4 != null } == true) {
				return true
			}

			if (isVideoDomain(post.domain?.decoded)) {
				return true
			}

			return listOfNotNull(
				post.url?.decoded,
				post.url_overridden_by_dest?.decoded,
				post.findUrl()?.value)
				.any { isVideoDomain(it) || hasVideoExtension(it) }
		}

		private fun isVideoDomain(value: String?): Boolean {
			val normalized = value?.lowercase(Locale.ROOT) ?: return false
			val domain = normalized
				.substringAfter("://", normalized)
				.removePrefix("www.")
				.substringBefore('/')
				.substringBefore(':')

			return videoDomains.any { domain == it || domain.endsWith(".$it") }
		}

		private fun hasVideoExtension(url: String): Boolean {
			val path = url.lowercase(Locale.ROOT)
				.substringBefore('?')
				.substringBefore('#')

			return path.endsWith(".gif")
				|| path.endsWith(".gifv")
				|| path.endsWith(".mp4")
				|| path.endsWith(".webm")
				|| path.endsWith(".h264")
				|| path.endsWith(".mkv")
				|| path.endsWith(".3gp")
		}

		private fun hasAnimatedGalleryMedia(post: RedditPost): Boolean {
			return post.media_metadata?.values?.any { entry ->
				val metadata = (entry as? MaybeParseError.Ok<RedditMediaMetadata>)?.value
					?: return@any false

				metadata.e.equals("AnimatedImage", ignoreCase = true)
					|| metadata.e.equals("Video", ignoreCase = true)
					|| metadata.s.mp4 != null
					|| metadata.s.gif != null
			} == true
		}
	}
}
