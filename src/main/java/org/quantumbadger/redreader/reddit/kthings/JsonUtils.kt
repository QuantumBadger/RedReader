package org.quantumbadger.redreader.reddit.kthings

import kotlinx.serialization.json.Json

object JsonUtils {
	val serializer = Json {
		ignoreUnknownKeys = true
		isLenient = true
	}
}
