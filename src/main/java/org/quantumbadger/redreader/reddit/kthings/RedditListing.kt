package org.quantumbadger.redreader.reddit.kthings

import android.os.Parcelable
import kotlinx.parcelize.Parcelize
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
@SerialName("Listing")
@Parcelize
data class RedditListing(
	val after: String? = null,
	val children: ArrayList<MaybeParseError<RedditThing>>
) : Parcelable
