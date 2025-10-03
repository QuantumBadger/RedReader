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
