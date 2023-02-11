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
	@Parcelize
	data class Comment(val data : RedditComment) : RedditThing()

	@Serializable
	@SerialName("t2")
	@Parcelize
	object User : RedditThing()

	@Serializable
	@SerialName("t3")
	@Parcelize
	data class Post(val data : RedditPost) : RedditThing()

	@Serializable
	@SerialName("t4")
	@Parcelize
	data class Message(val data : RedditMessage) : RedditThing()

	@Serializable
	@SerialName("t5")
	@Parcelize
	object Subreddit : RedditThing()

	@Serializable
	@SerialName("more")
	@Parcelize
	data class More(val data: RedditMore) : RedditThing()

	@Serializable
	@SerialName("Listing")
	@Parcelize
	data class Listing(val data : RedditListing) : RedditThing()
}
