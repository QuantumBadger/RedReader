package org.quantumbadger.redreader.reddit.kthings

import kotlinx.serialization.ExperimentalSerializationApi
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable
import kotlinx.serialization.json.JsonClassDiscriminator

@OptIn(ExperimentalSerializationApi::class)
@Serializable
@JsonClassDiscriminator("kind")
sealed class RedditThing {

	@Serializable
	@SerialName("t1")
	data class Comment(val data : RedditComment) : RedditThing()

	@Serializable
	@SerialName("t2")
	object User : RedditThing()

	@Serializable
	@SerialName("t3")
	data class Post(val data : RedditPost) : RedditThing()

	@Serializable
	@SerialName("t4")
	object Message : RedditThing()

	@Serializable
	@SerialName("t5")
	object Subreddit : RedditThing()

	@Serializable
	@SerialName("more")
	object More : RedditThing()

	@Serializable
	@SerialName("Listing")
	data class Listing(val data : RedditListing) : RedditThing()
}
