package org.quantumbadger.redreader.reddit.kthings

import android.os.Parcelable
import kotlinx.parcelize.Parcelize
import kotlinx.serialization.Serializable
import org.quantumbadger.redreader.reddit.things.RedditThingWithIdAndType

@Suppress("PropertyName")
@Serializable
@Parcelize
data class RedditMessage(
	val id: String,
	val name: RedditIdAndType,
	val author: UrlEncodedString? = null,
	val dest: UrlEncodedString? = null,
	val body: UrlEncodedString? = null,
	val body_html: UrlEncodedString? = null,
	val context: UrlEncodedString? = null,
	val subject: UrlEncodedString? = null,
	val subreddit_name_prefixed: UrlEncodedString? = null,
	val replies: RedditFieldReplies = RedditFieldReplies.None,
	val created_utc: RedditTimestampUTC

) : Parcelable, RedditThingWithIdAndType {

	override fun getIdAlone() = id
	override fun getIdAndType() = name
}
