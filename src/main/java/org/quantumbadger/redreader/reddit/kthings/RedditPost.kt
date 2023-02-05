package org.quantumbadger.redreader.reddit.kthings

import kotlinx.serialization.Serializable
import kotlinx.serialization.json.JsonObject

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
	val archived: Boolean = false,
	var over_18: Boolean = false,
	var hidden: Boolean = false,
	var saved: Boolean = false,
	var is_self: Boolean = false,
	var clicked: Boolean = false,
	var stickied: Boolean = false,
	var can_mod_post: Boolean = false,
	var edited: RedditBoolOrTimestampUTC? = null,
	var likes: Boolean? = null,
	var spoiler: Boolean? = null,
	var locked: Boolean? = null,
	var created_utc: RedditTimestampUTC,

	var selftext: UrlEncodedString? = null,
	var selftext_html: UrlEncodedString? = null,
	var permalink: UrlEncodedString,

	var link_flair_text: UrlEncodedString? = null,
	var author_flair_text: UrlEncodedString? = null,

	var thumbnail: UrlEncodedString? = null, // an image URL

	var media: JsonObject? = null, // TODO sub-object, MaybeParseErr
	var rr_internal_dash_url: UrlEncodedString? = null,

	var preview: JsonObject? = null, // TODO sub-object, MaybeParseErr
	var is_video: Boolean = false,

	var distinguished: String? = null,
	var suggested_sort: String? = null

)
