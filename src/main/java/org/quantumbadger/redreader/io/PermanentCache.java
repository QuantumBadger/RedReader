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

package org.quantumbadger.redreader.io;

import org.quantumbadger.redreader.common.TimestampBound;
import org.quantumbadger.redreader.common.collections.WeakReferenceListManager;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;

public final class PermanentCache<K, V extends WritableObject<K>, F>
		implements CacheDataSource<K, V, F> {

	private final HashMap<K, CacheEntry> cached = new HashMap<>();
	private final CacheDataSource<K, V, F> cacheDataSource;

	private final UpdatedVersionListenerNotifier<K, V> updatedVersionListenerNotifier
			= new UpdatedVersionListenerNotifier<>();

	public PermanentCache(final CacheDataSource<K, V, F> cacheDataSource) {
		this.cacheDataSource = cacheDataSource;
	}

	@Override
	public void performRequest(
			final K key,
			final TimestampBound timestampBound,
			final RequestResponseHandler<V, F> handler) {
		performRequest(key, timestampBound, handler, null);
	}

	@Override
	public synchronized void performRequest(
			final Collection<K> keys, final TimestampBound timestampBound,
			final RequestResponseHandler<HashMap<K, V>, F> handler) {

		final HashSet<K> keysRemaining = new HashSet<>(keys);
		final HashMap<K, V> cacheResult = new HashMap<>(keys.size());
		long oldestTimestamp = Long.MAX_VALUE;

		for(final K key : keys) {

			final CacheEntry entry = cached.get(key);
			if(entry != null) {

				final V value = entry.data;
				if(timestampBound.verifyTimestamp(value.getTimestamp())) {
					keysRemaining.remove(key);
					cacheResult.put(key, value);
					oldestTimestamp = Math.min(oldestTimestamp, value.getTimestamp());
				}
			}
		}

		if(!keysRemaining.isEmpty()) {

			final long outerOldestTimestamp = oldestTimestamp;

			cacheDataSource.performRequest(
					keysRemaining,
					timestampBound,
					new RequestResponseHandler<HashMap<K, V>, F>() {
						@Override
						public void onRequestFailed(final F failureReason) {
							handler.onRequestFailed(failureReason);
						}

						@Override
						public void onRequestSuccess(
								final HashMap<K, V> result,
								final long timeCached) {
							cacheResult.putAll(result);
							handler.onRequestSuccess(
									cacheResult,
									Math.min(timeCached, outerOldestTimestamp));
						}
					});

		} else {
			handler.onRequestSuccess(cacheResult, oldestTimestamp);
		}
	}

	@Override
	public synchronized void performWrite(final V value) {
		put(value, true);
	}

	@Override
	public void performWrite(final Collection<V> values) {
		put(values, true);
	}

	public synchronized void performRequest(
			final K key, final TimestampBound timestampBound,
			final RequestResponseHandler<V, F> handler,
			final UpdatedVersionListener<K, V> updatedVersionListener) {
		if(timestampBound != null) {
			final CacheEntry existingEntry = cached.get(key);
			if(existingEntry != null) {
				final V existing = existingEntry.data;
				if(timestampBound.verifyTimestamp(existing.getTimestamp())) {
					handler.onRequestSuccess(existing, existing.getTimestamp());
					return;
				}
			}
		}

		cacheDataSource.performRequest(
				key,
				timestampBound,
				new RequestResponseHandler<V, F>() {

					@Override
					public void onRequestFailed(final F failureReason) {
						handler.onRequestFailed(failureReason);
					}

					@Override
					public void onRequestSuccess(final V result, final long timeCached) {
						synchronized(PermanentCache.this) {
							put(result, false);
							if(updatedVersionListener != null) {
								cached.get(key).listeners.add(updatedVersionListener);
							}
							handler.onRequestSuccess(result, timeCached);
						}
					}
				});
	}

	public synchronized void forceUpdate(final K key) {
		cacheDataSource.performRequest(key, null, new RequestResponseHandler<V, F>() {

			@Override
			public void onRequestFailed(final F failureReason) {
			}

			@Override
			public void onRequestSuccess(final V result, final long timeCached) {
				put(result, false);
			}
		});
	}

	private synchronized void put(final V value, final boolean writeDown) {
		final CacheEntry oldEntry = cached.get(value.getKey());

		if(oldEntry != null) {
			cached.put(value.getKey(), new CacheEntry(value, oldEntry.listeners));
			oldEntry.listeners.map(updatedVersionListenerNotifier, value);
		} else {
			cached.put(value.getKey(), new CacheEntry(value));
		}

		if(writeDown) {
			cacheDataSource.performWrite(value);
		}
	}

	private synchronized void put(final Collection<V> values, final boolean writeDown) {

		for(final V value : values) {
			final CacheEntry oldEntry = cached.get(value.getKey());

			if(oldEntry != null) {
				cached.put(value.getKey(), new CacheEntry(value, oldEntry.listeners));
				oldEntry.listeners.map(updatedVersionListenerNotifier, value);
			} else {
				cached.put(value.getKey(), new CacheEntry(value));
			}
		}

		if(writeDown) {
			cacheDataSource.performWrite(values);
		}
	}

	private final class CacheEntry {
		public final V data;
		public final WeakReferenceListManager<UpdatedVersionListener<K, V>> listeners;

		private CacheEntry(
				final V data,
				final WeakReferenceListManager<UpdatedVersionListener<K, V>> listeners) {
			this.data = data;
			this.listeners = listeners;
		}

		private CacheEntry(final V data) {
			this(data, new WeakReferenceListManager<UpdatedVersionListener<K, V>>());
		}
	}
}
