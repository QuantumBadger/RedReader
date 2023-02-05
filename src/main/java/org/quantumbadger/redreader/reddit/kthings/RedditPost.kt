package org.quantumbadger.redreader.reddit.kthings

import kotlinx.serialization.Serializable
import org.quantumbadger.redreader.reddit.things.RedditThingWithIdAndType

@Suppress("PropertyName")
@Serializable
data class RedditPost(
	val id: String,
	val name: String,
	val url: UrlEncodedString? = null,
	val title: UrlEncodedString? = null,
	val author: UrlEncodedString? = null,
	val domain: UrlEncodedString? = null,
	val subreddit: UrlEncodedString,
	val num_comments: Int,
	val score: Int,
	val gilded: Int = 0,
	val upvote_ratio: Double? = null,
	val archived: Boolean? = null,
	val over_18: Boolean? = null,
	val hidden: Boolean? = null,
	val saved: Boolean? = null,
	val is_self: Boolean? = null,
	val clicked: Boolean? = null,
	val stickied: Boolean? = null,
	val can_mod_post: Boolean? = null,
	val edited: RedditBoolOrTimestampUTC? = null,
	val likes: Boolean? = null,
	val spoiler: Boolean? = null,
	val locked: Boolean? = null,
	val created_utc: RedditTimestampUTC,

	val selftext: UrlEncodedString? = null,
	val selftext_html: UrlEncodedString? = null,
	val permalink: UrlEncodedString,

	val link_flair_text: UrlEncodedString? = null,
	val author_flair_text: UrlEncodedString? = null,

	val thumbnail: UrlEncodedString? = null, // an image URL

	val media: Media? = null,

	val preview: Preview? = null,
	val is_video: Boolean? = null,

	val distinguished: String? = null,
	val suggested_sort: String? = null // TODO enum type
) : RedditThingWithIdAndType {

	@Serializable
	data class Media(
		val reddit_video: RedditVideo? = null
	) {
		@Serializable
		data class RedditVideo (
			val fallback_url: UrlEncodedString? = null
		)
	}

	@Serializable
	data class Preview(
		val enabled: Boolean,
		val images: List<Image>? = null
	) {
		@Serializable
		data class ImageDetails(
			val url: UrlEncodedString,
			val width: Int,
			val height: Int
		)

		@Serializable
		sealed class ImageBase {
			abstract val source: ImageDetails?
			abstract val resolutions: List<ImageDetails>?
		}

		@Serializable
		data class Image(
			override val source: ImageDetails? = null,
			override val resolutions: List<ImageDetails>? = null,
			val variants: ImageVariants
		) : ImageBase()

		@Serializable
		data class ImageVariants(
			val mp4: ImageVariant? = null
		)

		@Serializable
		data class ImageVariant(
			override val source: ImageDetails? = null,
			override val resolutions: List<ImageDetails>? = null
		) : ImageBase()
	}

	override fun getIdAlone(): String {
		return id
	}

	override fun getIdAndType(): String {
		return name
	}
}
