package org.quantumbadger.redreader.reddit.kthings

import kotlinx.serialization.Serializable

@Suppress("PropertyName")
@Serializable
data class RedditComment(
	val body_html: UrlEncodedString? = null
)
