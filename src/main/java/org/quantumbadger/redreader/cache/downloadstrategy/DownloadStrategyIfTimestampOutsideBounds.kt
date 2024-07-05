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

package org.quantumbadger.redreader.cache.downloadstrategy

import androidx.compose.runtime.Immutable
import org.quantumbadger.redreader.cache.CacheEntry
import org.quantumbadger.redreader.common.TimestampBound

@Immutable
data class DownloadStrategyIfTimestampOutsideBounds(
	private val timestampBound: TimestampBound
) : DownloadStrategy {

	override fun shouldDownloadWithoutCheckingCache(): Boolean {
		return false
	}

	override fun shouldDownloadIfCacheEntryFound(entry: CacheEntry): Boolean {
		return !timestampBound.verifyTimestamp(entry.timestamp)
	}

	override fun shouldDownloadIfNotCached(): Boolean {
		return true
	}
}
