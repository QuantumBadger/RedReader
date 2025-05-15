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

import java.util.concurrent.atomic.AtomicReference

class CachedStringHash(private val data: () -> String) {

	private class CacheEntry(
		val key: String,
		val value: String
	)

	private var cache = AtomicReference<CacheEntry?>()

	val hash: String
		get() = cache.get().run {
			val data = data()

			if (data == this?.key) {
				return value
			}

			val result = General.sha256(data)

			cache.set(CacheEntry(data, result))
			return result
		}
}
