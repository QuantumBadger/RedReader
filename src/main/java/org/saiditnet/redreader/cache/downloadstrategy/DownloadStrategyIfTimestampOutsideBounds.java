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

package org.saiditnet.redreader.cache.downloadstrategy;

import org.saiditnet.redreader.cache.CacheEntry;
import org.saiditnet.redreader.common.TimestampBound;

public class DownloadStrategyIfTimestampOutsideBounds implements DownloadStrategy {

	private final TimestampBound mTimestampBound;

	public DownloadStrategyIfTimestampOutsideBounds(final TimestampBound timestampBound) {
		mTimestampBound = timestampBound;
	}

	@Override
	public boolean shouldDownloadWithoutCheckingCache() {
		return false;
	}

	@Override
	public boolean shouldDownloadIfCacheEntryFound(final CacheEntry entry) {
		return !mTimestampBound.verifyTimestamp(entry.timestamp);
	}

	@Override
	public boolean shouldDownloadIfNotCached() {
		return true;
	}
}
