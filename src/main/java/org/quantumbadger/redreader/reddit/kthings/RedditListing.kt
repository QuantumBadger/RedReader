package org.quantumbadger.redreader.reddit.kthings

import android.os.Parcelable
import kotlinx.parcelize.Parcelize
import kotlinx.serialization.Serializable

@Serializable
@Parcelize
data class RedditListing(
	val after: String? = null,
	val children: ArrayList<MaybeParseError<RedditThing>>
) : Parcelable
