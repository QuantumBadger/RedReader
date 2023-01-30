package org.quantumbadger.redreader.reddit.kthings

import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
@SerialName("Listing")
data class RedditListing(
	val after: String?,
	val children: ArrayList<MaybeParseError<RedditThing>>
)
