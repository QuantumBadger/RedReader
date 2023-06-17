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
