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

package org.quantumbadger.redreader.common

import android.content.Context
import com.squareup.picasso.OkHttp3Downloader
import com.squareup.picasso.Picasso
import okhttp3.Cache
import org.quantumbadger.redreader.http.okhttp.OKHTTPBackend
import java.io.File

// Provides the project-wide Picasso instance, backed by RedReader's own
// OkHttpClient so that image downloads honour the user's proxy/Tor settings
// (the default Picasso downloader would bypass them).
object RedReaderPicasso {

	private const val CACHE_SIZE_BYTES = 50L * 1024 * 1024

	@Volatile
	private var mInstance: Picasso? = null

	@Volatile
	private var mContext: Context? = null

	fun get(context: Context): Picasso {

		mInstance?.let { return it }

		synchronized(this) {

			mInstance?.let { return it }

			val appContext = context.applicationContext
			mContext = appContext
			val picasso = build(appContext)
			mInstance = picasso
			return picasso
		}
	}

	// Rebuilds the instance with the current HTTP backend configuration
	// (e.g. after the user toggles the Tor/proxy setting).
	fun recreate() {
		synchronized(this) {
			val context = mContext ?: return
			mInstance = build(context)
		}
	}

	private fun build(context: Context): Picasso {

		// Clone the backend client (so Tor/proxy/timeout settings carry over)
		// and give it a persistent disk cache, so images are only downloaded
		// once instead of on every launch.
		val cacheDir = File(context.cacheDir, "picasso-images")
		val client = OKHTTPBackend.getClient().newBuilder()
			.cache(Cache(cacheDir, CACHE_SIZE_BYTES))
			.build()

		return Picasso.Builder(context)
			.downloader(OkHttp3Downloader(client))
			.build()
	}
}
