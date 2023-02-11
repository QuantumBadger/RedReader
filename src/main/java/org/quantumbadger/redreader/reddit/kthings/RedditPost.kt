package org.quantumbadger.redreader.reddit.kthings

import android.os.Parcelable
import kotlinx.parcelize.Parcelize
import kotlinx.serialization.Serializable
import org.quantumbadger.redreader.reddit.things.RedditThingWithIdAndType

@Suppress("PropertyName")
@Serializable
@Parcelize
data class RedditPost(
	val id: String,
	val name: RedditIdAndType,
	val url: UrlEncodedString? = null,
	val title: UrlEncodedString? = null,
	val author: UrlEncodedString? = null,
	val domain: UrlEncodedString? = null,
	val subreddit: UrlEncodedString,
	val num_comments: Int,
	val score: Int,
	val gilded: Int = 0,
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
	val suggested_sort: String? = null // TODO enum type
) : RedditThingWithIdAndType, Parcelable {

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
}
