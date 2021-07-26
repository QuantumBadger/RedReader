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
import org.quantumbadger.redreader.common.TriggerableThread;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.concurrent.LinkedBlockingQueue;

public class ThreadedRawObjectDB<K, V extends WritableObject<K>, F>
		implements CacheDataSource<K, V, F> {

	private final TriggerableThread writeThread
			= new TriggerableThread(this::doWrite, 1500);

	private final TriggerableThread readThread
			= new TriggerableThread(this::doRead, 0);

	private final HashMap<K, V> toWrite = new HashMap<>();
	private final LinkedBlockingQueue<ReadOperation> toRead = new LinkedBlockingQueue<>();
	private final Object ioLock = new Object();

	private final RawObjectDB<K, V> db;
	private final CacheDataSource<K, V, F> alternateSource;

	public ThreadedRawObjectDB(
			final RawObjectDB<K, V> db,
			final CacheDataSource<K, V, F> alternateSource) {
		this.db = db;
		this.alternateSource = alternateSource;
	}

	private void doWrite() {
		synchronized(ioLock) {

			final ArrayList<V> values;

			synchronized(toWrite) {
				values = new ArrayList<>(toWrite.values());
				toWrite.clear();
			}

			db.putAll(values);
		}
	}

	private void doRead() {
		synchronized(ioLock) {
			while(!toRead.isEmpty()) {
				toRead.remove().run();
			}
		}
	}

	@Override
	public void performRequest(
			final K key, final TimestampBound timestampBound,
			final RequestResponseHandler<V, F> handler) {

		toRead.offer(new SingleReadOperation(timestampBound, handler, key));
		readThread.trigger();
	}

	@Override
	public void performRequest(
			final Collection<K> keys, final TimestampBound timestampBound,
			final RequestResponseHandler<HashMap<K, V>, F> handler) {

		toRead.offer(new BulkReadOperation(timestampBound, handler, keys));
		readThread.trigger();
	}

	@Override
	public void performWrite(final V value) {

		synchronized(toWrite) {
			toWrite.put(value.getKey(), value);
		}

		writeThread.trigger();
	}

	@Override
	public void performWrite(final Collection<V> values) {

		synchronized(toWrite) {
			for(final V value : values) {
				toWrite.put(value.getKey(), value);
			}
		}

		writeThread.trigger();
	}

	private class BulkReadOperation extends ReadOperation {

		public final Collection<K> keys;
		public final RequestResponseHandler<HashMap<K, V>, F> responseHandler;

		private BulkReadOperation(
				final TimestampBound timestampBound,
				final RequestResponseHandler<HashMap<K, V>, F> responseHandler,
				final Collection<K> keys) {
			super(timestampBound);
			this.responseHandler = responseHandler;
			this.keys = keys;
		}

		@Override
		public void run() {

			final HashMap<K, V> existingResult = new HashMap<>(keys.size());
			long oldestTimestamp = Long.MAX_VALUE;

			synchronized(toWrite) {

				final Iterator<K> iter = keys.iterator();

				while(iter.hasNext()) {
					final K key = iter.next();
					final V writeCacheResult = toWrite.get(key);
					if(writeCacheResult != null && timestampBound.verifyTimestamp(
							writeCacheResult.getTimestamp())) {
						iter.remove();
						existingResult.put(key, writeCacheResult);
						oldestTimestamp = Math.min(
								oldestTimestamp,
								writeCacheResult.getTimestamp());
					}
				}
			}

			if(keys.isEmpty()) {
				responseHandler.onRequestSuccess(existingResult, oldestTimestamp);
				return;
			}

			final Iterator<K> iter = keys.iterator();

			while(iter.hasNext()) {
				final K key = iter.next();
				final V dbResult = db.getById(key); // TODO this is pretty inefficient
				if(dbResult != null
						&& timestampBound.verifyTimestamp(dbResult.getTimestamp())) {
					iter.remove();
					existingResult.put(key, dbResult);
					oldestTimestamp = Math.min(oldestTimestamp, dbResult.getTimestamp());
				}
			}

			if(keys.isEmpty()) {
				responseHandler.onRequestSuccess(existingResult, oldestTimestamp);
				return;
			}

			final long outerOldestTimestamp = oldestTimestamp;

			alternateSource.performRequest(
					keys,
					timestampBound,
					new RequestResponseHandler<HashMap<K, V>, F>() {
						@Override
						public void onRequestFailed(final F failureReason) {
							responseHandler.onRequestFailed(failureReason);
						}

						@Override
						public void onRequestSuccess(
								final HashMap<K, V> result,
								final long timeCached) {
							performWrite(result.values());
							existingResult.putAll(result);
							responseHandler.onRequestSuccess(
									existingResult,
									Math.min(timeCached, outerOldestTimestamp));
						}
					});
		}
	}

	private class SingleReadOperation extends ReadOperation {

		public final K key;
		public final RequestResponseHandler<V, F> responseHandler;

		private SingleReadOperation(
				final TimestampBound timestampBound,
				final RequestResponseHandler<V, F> responseHandler,
				final K key) {
			super(timestampBound);
			this.responseHandler = responseHandler;
			this.key = key;
		}

		@Override
		public void run() {

			synchronized(toWrite) {
				final V writeCacheResult = toWrite.get(key);
				if(writeCacheResult != null && timestampBound.verifyTimestamp(
						writeCacheResult.getTimestamp())) {
					responseHandler.onRequestSuccess(
							writeCacheResult,
							writeCacheResult.getTimestamp());
					return;
				}
			}

			final V dbResult = db.getById(key);
			if(dbResult != null
					&& timestampBound.verifyTimestamp(dbResult.getTimestamp())) {
				responseHandler.onRequestSuccess(dbResult, dbResult.getTimestamp());
				return;
			}

			alternateSource.performRequest(
					key,
					timestampBound,
					new RequestResponseHandler<V, F>() {
						@Override
						public void onRequestFailed(final F failureReason) {
							responseHandler.onRequestFailed(failureReason);
						}

						@Override
						public void onRequestSuccess(final V result, final long timeCached) {
							performWrite(result);
							responseHandler.onRequestSuccess(result, timeCached);
						}
					});
		}
	}

	private abstract static class ReadOperation {

		public final TimestampBound timestampBound;

		private ReadOperation(final TimestampBound timestampBound) {
			this.timestampBound = timestampBound;
		}

		public abstract void run();
	}
}
