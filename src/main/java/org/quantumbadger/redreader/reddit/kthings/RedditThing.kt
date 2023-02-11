package org.quantumbadger.redreader.reddit.kthings

import android.os.Parcelable
import kotlinx.parcelize.Parcelize
import kotlinx.serialization.ExperimentalSerializationApi
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable
import kotlinx.serialization.json.JsonClassDiscriminator

@OptIn(ExperimentalSerializationApi::class)
@Serializable
@JsonClassDiscriminator("kind")
@Parcelize
sealed class RedditThing : Parcelable {

	// TODO test that parcelling sealed classes actually works correctly

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
	data class Message(val data : RedditMessage) : RedditThing()

	@Serializable
	@SerialName("t5")
	object Subreddit : RedditThing()

	@Serializable
	@SerialName("more")
	data class More(val data: RedditMore) : RedditThing()

	@Serializable
	@SerialName("Listing")
	data class Listing(val data : RedditListing) : RedditThing()
}
