/*******************************************************************************
 * This file is part of RedReader.
 *
 * RedReader is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * RedReader is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with RedReader.  If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/

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
