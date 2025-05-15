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

package org.quantumbadger.redreader.common;

import android.os.SystemClock;
import androidx.annotation.NonNull;

import java.util.concurrent.atomic.AtomicReference;

public class CachedValue<E> {

	private static class CacheEntry<E> {
		@NonNull private final E mValue;
		private final long mLastUpdateMs;

		private CacheEntry(@NonNull final E value, final long lastUpdateMs) {
			mValue = value;
			mLastUpdateMs = lastUpdateMs;
		}

		@NonNull
		public E getValue() {
			return mValue;
		}

		public long getLastUpdateMs() {
			return mLastUpdateMs;
		}
	}

	@NonNull private final GenericFactory<E, RuntimeException> mFactory;
	private final long mMaxAgeMs;

	private final AtomicReference<CacheEntry<E>> mEntry = new AtomicReference<>(null);

	public CachedValue(
			@NonNull final GenericFactory<E, RuntimeException> factory,
			final long maxAgeMs) {

		mFactory = factory;
		mMaxAgeMs = maxAgeMs;
	}

	@NonNull
	public E get() {

		final long timeNow = SystemClock.uptimeMillis();

		final CacheEntry<E> entry = mEntry.get();

		if(entry != null && timeNow - entry.getLastUpdateMs() < mMaxAgeMs) {
			return entry.getValue();
		}

		final E newValue = mFactory.create();
		mEntry.set(new CacheEntry<>(newValue, timeNow));
		return newValue;
	}
}
