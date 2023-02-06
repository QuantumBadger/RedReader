package org.quantumbadger.redreader.reddit.kthings

import kotlinx.serialization.ExperimentalSerializationApi
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.decodeFromStream
import java.io.IOException
import java.io.InputStream

object JsonUtils {
	val serializer = Json {
		ignoreUnknownKeys = true
		isLenient = true
	}

	@OptIn(ExperimentalSerializationApi::class)
	@Throws(IOException::class)
	fun decodeRedditThingFromStream(stream: InputStream): RedditThing
			= serializer.decodeFromStream(stream)
}
