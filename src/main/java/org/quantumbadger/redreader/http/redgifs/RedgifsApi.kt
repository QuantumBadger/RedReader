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

package org.quantumbadger.redreader.http.redgifs

import android.util.Log
import okhttp3.OkHttpClient
import okhttp3.Request
import okhttp3.Response
import okhttp3.HttpUrl.Companion.toHttpUrl
import org.json.JSONObject

class RedgifsApi(
	private val client: OkHttpClient = OkHttpClient()
) {
	companion object {
		private const val TAG = "RedgifsApi"
		private const val API_HOST = "https://api.redgifs.com"
		private const val UA =
			"Mozilla/5.0 (Android) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Mobile Safari/537.36"
		// Prefer v3, fallback to v2
		private const val V3 = "v3"
		private const val V2 = "v2"
	}

	private var bearer: String? = null
	private var tokenExpiryMs: Long = 0L

	/** Get a temporary token; cache it briefly. */
	@Synchronized
	fun ensureToken(): String {
		val now = System.currentTimeMillis()
		if (bearer != null && now < tokenExpiryMs) return bearer!!

		// Try v3 first, then v2 (both return a token in practice)
		val token = fetchTemporaryToken(V3) ?: fetchTemporaryToken(V2)
		?: error("Failed to fetch RedGifs temporary token (v3/v2)")

		bearer = token
		// Tokens are short-lived. Cache ~20 minutes to be safe.
		tokenExpiryMs = now + 20 * 60 * 1000
		return token
	}

	private fun fetchTemporaryToken(ver: String): String? {
		val url = "$API_HOST/$ver/auth/temporary".toHttpUrl()
		val req = Request.Builder()
			.url(url)
			.header("User-Agent", UA)
			.header("Accept", "application/json")
			.build()
		client.newCall(req).execute().use { resp ->
			if (!resp.isSuccessful) {
				Log.w(TAG, "temp token $ver failed: ${resp.code}")
				return null
			}
			val body = resp.body?.string() ?: return null
			return runCatching { JSONObject(body).getString("token") }.getOrNull()
		}
	}

	/** Fetch GIF JSON (id = “closeexpertracer”, etc.). Returns JSONObject of the GIF node. */
	fun getGifById(id: String): JSONObject {
		val token = ensureToken()

		// Try v3 first, fallback to v2
		val gif = getGifById(id, V3, token) ?: getGifById(id, V2, token)
		return gif ?: error("RedGifs: GIF not found (id=$id)")
	}

	private fun getGifById(id: String, ver: String, token: String): JSONObject? {
		val url = "$API_HOST/$ver/gifs/$id".toHttpUrl()
		val req = Request.Builder()
			.url(url)
			.header("User-Agent", UA)
			.header("Accept", "application/json")
			.header("Authorization", "Bearer $token")
			.build()
		client.newCall(req).execute().use { resp ->
			if (!resp.isSuccessful) {
				Log.w(TAG, "getGif $ver failed ${resp.code}")
				return null
			}
			val body = resp.body?.string() ?: return null
			val root = JSONObject(body)
			// API returns { "gif": { ... fields ... } }
			return root.optJSONObject("gif") ?: root.optJSONObject("gfyItem")
		}
	}
}
