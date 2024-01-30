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

package org.quantumbadger.redreader.reddit.kthings

import android.os.Parcelable
import android.util.Log
import kotlinx.parcelize.Parcelize
import kotlinx.serialization.Serializable
import org.quantumbadger.redreader.image.RedditGalleryAPI
import org.quantumbadger.redreader.reddit.things.RedditThingWithIdAndType

@Suppress("PropertyName")
@Serializable
@Parcelize
data class RedditPost(
	val id: String,
	val name: RedditIdAndType,
	val url: UrlEncodedString? = null,
	val url_overridden_by_dest: UrlEncodedString? = null,
	val title: UrlEncodedString? = null,
	val author: UrlEncodedString? = null,
	val domain: UrlEncodedString? = null,
	val subreddit: UrlEncodedString,
	val num_comments: Int,
	val score: Int,
	val gilded: Int = 0,
	val crosspost_parent: String? = null,
	val upvote_ratio: Double? = null,
	val archived: Boolean = false,
	val over_18: Boolean = false,
	val hidden: Boolean = false,
	val saved: Boolean = false,
	val is_self: Boolean = false,
	val clicked: Boolean = false,
	val stickied: Boolean = false,
	val can_mod_post: Boolean = false,
	val edited: RedditFieldEdited? = null,
	val likes: Boolean? = null,
	val spoiler: Boolean = false,
	val locked: Boolean = false,
	val created_utc: RedditTimestampUTC,

	val selftext: UrlEncodedString? = null,
	val selftext_html: UrlEncodedString? = null,
	val permalink: UrlEncodedString,

	val link_flair_text: UrlEncodedString? = null,
	val author_flair_text: UrlEncodedString? = null,

	val thumbnail: UrlEncodedString? = null, // an image URL

	val media: Media? = null,

	val preview: Preview? = null,
	val is_video: Boolean = false,

	val distinguished: String? = null,
	val suggested_sort: String? = null, // TODO enum type

	@kotlinx.parcelize.IgnoredOnParcel
	val media_metadata: Map<UrlEncodedString, MaybeParseError<RedditMediaMetadata>>? = null,
	@kotlinx.parcelize.IgnoredOnParcel
	val gallery_data: GalleryData? = null,

	val removed_by_category: String? = null

) : RedditThingWithIdAndType, Parcelable {

	init {
		try {
			RedditGalleryAPI.addToCache(this)
		} catch (t: Throwable) {
			Log.e("RedditPost", "Got error when adding post to cache", t)
		}
	}

	@Serializable
	data class GalleryData(
		val items: List<MaybeParseError<GalleryItem>>
	) {
		@Serializable
		@Parcelize
		data class GalleryItem (
			val media_id: UrlEncodedString,
			val caption: UrlEncodedString? = null,
			val outbound_url: UrlEncodedString? = null,
		) : Parcelable
	}

	@Serializable
	@Parcelize
	data class Media(
		val reddit_video: RedditVideo? = null
	) : Parcelable {
		@Serializable
		@Parcelize
		data class RedditVideo (
			val fallback_url: UrlEncodedString? = null
		) : Parcelable
	}

	@Serializable
	@Parcelize
	data class Preview(
		val enabled: Boolean,
		val images: List<Image>? = null,
		val reddit_video_preview: RedditVideoPreview? = null
	) : Parcelable {
		@Serializable
		@Parcelize
		data class ImageDetails(
			val url: UrlEncodedString,
			val width: Int,
			val height: Int
		) : Parcelable

		@Serializable
		sealed class ImageBase {
			abstract val source: ImageDetails?
			abstract val resolutions: List<ImageDetails>?
		}

		@Serializable
		@Parcelize
		data class Image(
			override val source: ImageDetails? = null,
			override val resolutions: List<ImageDetails>? = null,
			val variants: ImageVariants
		) : ImageBase(), Parcelable

		@Serializable
		@Parcelize
		data class ImageVariants(
			val mp4: ImageVariant? = null
		) : Parcelable

		@Serializable
		@Parcelize
		data class ImageVariant(
			override val source: ImageDetails? = null,
			override val resolutions: List<ImageDetails>? = null
		) : ImageBase(), Parcelable

		@Serializable
		@Parcelize
		data class RedditVideoPreview(
			val fallback_url: UrlEncodedString? = null
		) : Parcelable
	}

	override fun getIdAlone(): String {
		return id
	}

	override fun getIdAndType(): RedditIdAndType {
		return name
	}

	fun findUrl(): String? {

		media?.reddit_video?.fallback_url?.decoded?.apply {
			return this
		}

		if (url?.decoded?.contains(".gif") == true) {
			preview?.images?.get(0)?.variants?.mp4?.source?.url?.decoded?.apply {
				return this
			}
		}

		url_overridden_by_dest?.decoded?.apply {
			return this
		}

		return url?.decoded
	}
}
