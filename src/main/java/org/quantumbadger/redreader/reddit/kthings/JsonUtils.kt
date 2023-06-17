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

import kotlinx.serialization.ExperimentalSerializationApi
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.decodeFromStream
import java.io.IOException
import java.io.InputStream

object JsonUtils {
	private val serializer = Json {
		ignoreUnknownKeys = true
		isLenient = true
	}

	@OptIn(ExperimentalSerializationApi::class)
	@Throws(IOException::class)
	fun decodeRedditThingFromStream(stream: InputStream)
			= serializer.decodeFromStream(RedditThing.serializer(), stream)

	@OptIn(ExperimentalSerializationApi::class)
	@Throws(IOException::class)
	fun decodeRedditThingResponseFromStream(stream: InputStream)
			= serializer.decodeFromStream(RedditThingResponse.serializer(), stream)
}
