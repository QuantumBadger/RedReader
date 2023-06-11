package org.quantumbadger.redreader.reddit.kthings

import android.net.Uri
import android.os.Parcelable
import kotlinx.parcelize.Parcelize
import kotlinx.serialization.Serializable
import org.quantumbadger.redreader.common.LinkHandler
import org.quantumbadger.redreader.reddit.things.RedditThingWithIdAndType
import org.quantumbadger.redreader.reddit.url.PostCommentListingURL

// TODO add copyright messages to all Kotlin files

@Suppress("PropertyName")
@Serializable
@Parcelize
data class RedditComment(
	val body: UrlEncodedString? = null,
	val body_html: UrlEncodedString? = null,

	val author: UrlEncodedString? = null,
	val subreddit: UrlEncodedString? = null,
	val author_flair_text: UrlEncodedString? = null,
	val archived: Boolean = false,
	val likes: Boolean? = null,
	val score_hidden: Boolean = false,
	val locked: Boolean = false,
	val can_mod_post: Boolean = false,
	val media_metadata: Map<UrlEncodedString, MaybeParseError<EmoteMetadata>>? = null,

	val replies: RedditFieldReplies = RedditFieldReplies.None,

	val id: String,
	val subreddit_id: String? = null,
	val link_id: String? = null,
	val parent_id: String? = null,
	val name: RedditIdAndType,
	val context: UrlEncodedString? = null,

	val ups: Int = 0,
	val downs: Int = 0,
	val gilded: Int = 0,
	val controversiality: Int = 0,

	val edited: RedditFieldEdited = RedditFieldEdited.Bool(false), // TODO do same in other one

	val created_utc: RedditTimestampUTC,

	val saved: Boolean = false,

	val distinguished: String? = null, // TODO enum? Test unknown values

	val stickied: Boolean = false

) : Parcelable, RedditThingWithIdAndType {

	// TODO do this in the HTML parser instead
	fun copyWithNewBodyHtml(value: String) = copy(body_html = UrlEncodedString(value))

	@Serializable
	@Parcelize
	data class EmoteMetadata(
		val status: String,
		val e: String,
		val m: String,
		val s: ImageMetadata,
		val t: String,
		val id: String
	) : Parcelable

	@Serializable
	@Parcelize
	data class ImageMetadata(
		val x: String,
		val y: String,
		val u: String? = null
	) : Parcelable

	override fun getIdAlone() = id

	override fun getIdAndType() = name

	fun getContextUrl(): PostCommentListingURL {

		return context?.run {
			var result = decoded
			if (result.startsWith("r/")) {
				result = "/$result"
			}
			if (result.startsWith("/")) {
				result = "https://reddit.com$result"
			}
			PostCommentListingURL.parse(Uri.parse(result))

		} ?: PostCommentListingURL(
			null,
			link_id,
			idAlone,
			3,
			null,
			null,
			false
		)
	}

	fun computeAllLinks(): Set<String> {
		return body_html?.decoded?.run { LinkHandler.computeAllLinks(this) } ?: emptySet()
	}

	fun wasEdited(): Boolean = edited != RedditFieldEdited.Bool(false)

	fun isControversial(): Boolean = controversiality == 1
}
