package org.quantumbadger.redreader.io;

import org.quantumbadger.redreader.common.TimestampBound;
import org.quantumbadger.redreader.common.collections.WeakReferenceListManager;

import java.lang.ref.WeakReference;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;

public final class WeakCache<K, V extends WritableObject<K>, F> implements CacheDataSource<K, V, F> {

	private final HashMap<K, CacheEntry> cached = new HashMap<K, CacheEntry>();
	private final CacheDataSource<K, V, F> cacheDataSource;

	private final UpdatedVersionListenerNotifier<K, V> updatedVersionListenerNotifier = new UpdatedVersionListenerNotifier<K, V>();

	public WeakCache(CacheDataSource<K, V, F> cacheDataSource) {
		this.cacheDataSource = cacheDataSource;
	}

	public void performRequest(K key, TimestampBound timestampBound, RequestResponseHandler<V, F> handler) {
		performRequest(key, timestampBound, handler, null);
	}

	public synchronized void performRequest(final Collection<K> keys, final TimestampBound timestampBound,
											final RequestResponseHandler<HashMap<K, V>, F> handler) {

		final HashSet<K> keysRemaining = new HashSet<K>(keys);
		final HashMap<K, V> cacheResult = new HashMap<K, V>(keys.size());
		long oldestTimestamp = Long.MAX_VALUE;

		for(final K key : keys) {

			final CacheEntry entry = cached.get(key);
			if(entry != null) {

				final V value = entry.data.get();
				if(value != null && timestampBound.verifyTimestamp(value.getTimestamp())) {
					keysRemaining.remove(key);
					cacheResult.put(key, value);
					oldestTimestamp = Math.min(oldestTimestamp, value.getTimestamp());
				}
			}
		}

		if(keysRemaining.size() > 0) {

			final long outerOldestTimestamp = oldestTimestamp;

			cacheDataSource.performRequest(keysRemaining, timestampBound, new RequestResponseHandler<HashMap<K, V>, F>() {
				public void onRequestFailed(F failureReason) {
					handler.onRequestFailed(failureReason);
				}

				public void onRequestSuccess(HashMap<K, V> result, long timeCached) {
					cacheResult.putAll(result);
					handler.onRequestSuccess(cacheResult, Math.min(timeCached, outerOldestTimestamp));
				}
			});

		} else {
			handler.onRequestSuccess(cacheResult, oldestTimestamp);
		}
	}

	public synchronized void performWrite(V value) {
		put(value, true);
	}

	public void performWrite(Collection<V> values) {
		put(values, true);
	}

	public synchronized void performRequest(final K key, final TimestampBound timestampBound,
											final RequestResponseHandler<V, F> handler,
											final UpdatedVersionListener<K, V> updatedVersionListener) {
		if(timestampBound != null) {
			final CacheEntry existingEntry = cached.get(key);
			if(existingEntry != null) {
				final V existing = existingEntry.data.get();
				if(existing != null && timestampBound.verifyTimestamp(existing.getTimestamp())) {
					handler.onRequestSuccess(existing, existing.getTimestamp());
					return;
				}
			}
		}

		cacheDataSource.performRequest(key, timestampBound, new RequestResponseHandler<V, F>() {

			public void onRequestFailed(F failureReason) {
				handler.onRequestFailed(failureReason);
			}

			public void onRequestSuccess(V result, long timeCached) {
				synchronized(WeakCache.this) {
					put(result, false);
					if(updatedVersionListener != null) cached.get(key).listeners.add(updatedVersionListener);
					handler.onRequestSuccess(result, timeCached);
				}
			}
		});
	}

	public synchronized void forceUpdate(final K key) {
		cacheDataSource.performRequest(key, null, new RequestResponseHandler<V, F>() {

			public void onRequestFailed(F failureReason) {}

			public void onRequestSuccess(V result, long timeCached) {
				put(result, false);
			}
		});
	}

	private synchronized void put(final V value, boolean writeDown) {
		final CacheEntry oldEntry = cached.get(value.getKey());

		if(oldEntry != null) {
			cached.put(value.getKey(), new CacheEntry(new WeakReference<V>(value), oldEntry.listeners));
			oldEntry.listeners.map(updatedVersionListenerNotifier, value);
		} else {
			cached.put(value.getKey(), new CacheEntry(new WeakReference<V>(value)));
		}

		if(writeDown) cacheDataSource.performWrite(value);
	}

	private synchronized void put(final Collection<V> values, boolean writeDown) {

		for(final V value : values) {
			final CacheEntry oldEntry = cached.get(value.getKey());

			if(oldEntry != null) {
				cached.put(value.getKey(), new CacheEntry(new WeakReference<V>(value), oldEntry.listeners));
				oldEntry.listeners.map(updatedVersionListenerNotifier, value);
			} else {
				cached.put(value.getKey(), new CacheEntry(new WeakReference<V>(value)));
			}
		}

		if(writeDown) cacheDataSource.performWrite(values);
	}

	private final class CacheEntry {
		public final WeakReference<V> data;
		public final WeakReferenceListManager<UpdatedVersionListener<K, V>> listeners;

		private CacheEntry(WeakReference<V> data,
						   WeakReferenceListManager<UpdatedVersionListener<K, V>> listeners) {
			this.data = data;
			this.listeners = listeners;
		}

		private CacheEntry(WeakReference<V> data) {
			this(data, new WeakReferenceListManager<UpdatedVersionListener<K, V>>());
		}
	}

	public static interface UpdatedVersionListener<K, V extends WritableObject<K>> {
		public void onUpdatedVersion(V data);
	}

	public static class UpdatedVersionListenerNotifier<K, V extends WritableObject<K>>
			implements WeakReferenceListManager.ArgOperator<UpdatedVersionListener<K, V>, V> {

		public void operate(UpdatedVersionListener<K, V> listener, V data) {
			listener.onUpdatedVersion(data);
		}
	}
}
