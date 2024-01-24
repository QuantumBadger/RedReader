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

import android.os.Build
import kotlinx.serialization.ExperimentalSerializationApi
import kotlinx.serialization.KSerializer
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.decodeFromStream
import kotlinx.serialization.json.okio.decodeFromBufferedSource
import okio.BufferedSource
import okio.buffer
import okio.source
import java.io.IOException
import java.io.InputStream

object JsonUtils {
	private val json = Json {
		ignoreUnknownKeys = true
		isLenient = true
	}

	@Throws(IOException::class)
	fun decodeRedditThingFromStream(stream: InputStream): RedditThing =
		decodeFromStream(RedditThing.serializer(), stream)

	@Throws(IOException::class)
	fun decodeRedditThingResponseFromStream(stream: InputStream): RedditThingResponse =
		decodeFromStream(RedditThingResponse.serializer(), stream)

	@OptIn(ExperimentalSerializationApi::class)
	private fun <T> decodeFromStream(serializer: KSerializer<T>, stream: InputStream): T {
		/**
		 * [Json.decodeFromStream] is broken on API < 24
		 * Wrap it in [BufferedSource] and use [Json.decodeFromBufferedSource] which works
		 *
		 * https://github.com/Kotlin/kotlinx.serialization/issues/2457
		 * https://issuetracker.google.com/issues/37054036
		 */
		return if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.N) {
			json.decodeFromStream(serializer, stream)
		} else {
			json.decodeFromBufferedSource(serializer, stream.source().buffer())
		}
	}
}
