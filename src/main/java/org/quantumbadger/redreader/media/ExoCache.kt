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

package org.quantumbadger.redreader.media

import android.content.Context
import androidx.media3.database.StandaloneDatabaseProvider
import androidx.media3.datasource.cache.LeastRecentlyUsedCacheEvictor
import androidx.media3.datasource.cache.SimpleCache
import java.io.File

object ExoCache {
	// 256 MiB; tweak as you like
	private const val MAX_BYTES: Long = 256L * 1024L * 1024L

	@Volatile private var cache: SimpleCache? = null

	fun get(context: Context): SimpleCache {
		// double-checked locking
		val existing = cache
		if (existing != null) return existing

		synchronized(this) {
			val again = cache
			if (again != null) return again

			val dir = File(context.cacheDir, "media3-cache")
			val evictor = LeastRecentlyUsedCacheEvictor(MAX_BYTES)
			val db = StandaloneDatabaseProvider(context.applicationContext)
			val created = SimpleCache(dir, evictor, db)
			cache = created
			return created
		}
	}
}
