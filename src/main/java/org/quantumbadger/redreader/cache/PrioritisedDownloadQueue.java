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

package org.quantumbadger.redreader.cache;

import org.apache.http.client.HttpClient;

import java.util.HashMap;
import java.util.Map;


class PrioritisedDownloadQueue {

	// TODO maintain a priority queue as well? This is probably fast enough (small no of entries)
	private final HashMap<RequestIdentifier, CacheDownload> downloadsQueued = new HashMap<RequestIdentifier, CacheDownload>();
	private final HashMap<RequestIdentifier, CacheDownload> redditDownloadsQueued = new HashMap<RequestIdentifier, CacheDownload>();
	private final HashMap<RequestIdentifier, CacheDownload> downloadsInProgress = new HashMap<RequestIdentifier, CacheDownload>();

	private final HttpClient httpClient;

	public PrioritisedDownloadQueue(final HttpClient httpClient) {
		this.httpClient = httpClient;
		new RedditQueueProcessor().start();
	}

	HttpClient getHttpClient() {
		return httpClient;
	}

	public synchronized void add(final CacheRequest request, final CacheManager manager) {

		final RequestIdentifier identifier = request.createIdentifier();

		// Is in progress? If so, add late joiner. or, cancel if requested.

		if(downloadsInProgress.containsKey(identifier)) {
			if(request.cancelExisting) {
				downloadsInProgress.get(identifier).cancel();
			} else {
				downloadsInProgress.get(identifier).addLateJoiner(request);
				return;
			}
		}

		// Is it a reddit download? Add to reddit queue

		if(request.isRedditApi) {

			if(redditDownloadsQueued.containsKey(identifier)) {
				redditDownloadsQueued.get(identifier).addLateJoiner(request);

			} else {
				redditDownloadsQueued.put(identifier, new CacheDownload(request, manager, this));
				notifyAll();
			}

			return;
		}

		// Is the priority <= 0? If so, spin up a new thread and run immediately.
		if(request.priority <= 0) {

			final CacheDownload download;

			if(downloadsQueued.containsKey(identifier)) {
				download = downloadsQueued.remove(identifier);
				download.addLateJoiner(request);
			} else {
				download = new CacheDownload(request, manager, this);
			}

			downloadsInProgress.put(identifier, download);
			new CacheDownloadThread(this, download, true);

			return;
		}

		// Is in queue? If so, add late joiner

		if(downloadsQueued.containsKey(identifier)) {
			downloadsQueued.get(identifier).addLateJoiner(request);
			return;
		}

		// Otherwise, add to queue and notify all

		downloadsQueued.put(identifier, new CacheDownload(request, manager, this));
		notifyAll();
	}

	public synchronized CacheDownload getNextInQueue() {

		while(downloadsQueued.isEmpty()) {
			try { wait(); } catch (InterruptedException e) { throw new RuntimeException(e); }
		}

		CacheDownload next = null;
		RequestIdentifier nextKey = null;

		for(final Map.Entry<RequestIdentifier, CacheDownload> entry : downloadsQueued.entrySet()) {
			if(next == null || entry.getValue().isHigherPriorityThan(next)) {
				next = entry.getValue();
				nextKey = entry.getKey();
			}
		}

		downloadsQueued.remove(nextKey);
		downloadsInProgress.put(nextKey, next);

		return next;
	}

	private synchronized CacheDownload getNextRedditInQueue() {

		while(redditDownloadsQueued.isEmpty()) {
			try { wait(); } catch (InterruptedException e) { throw new RuntimeException(e); }
		}

		CacheDownload next = null;
		RequestIdentifier nextKey = null;

		for(final Map.Entry<RequestIdentifier, CacheDownload> entry : redditDownloadsQueued.entrySet()) {
			if(next == null || entry.getValue().isHigherPriorityThan(next)) {
				next = entry.getValue();
				nextKey = entry.getKey();
			}
		}

		redditDownloadsQueued.remove(nextKey);
		downloadsInProgress.put(nextKey, next);

		return next;
	}

	public synchronized void removeDownload(final CacheDownload download) {
		downloadsInProgress.remove(download.createIdentifier());
	}

	public synchronized void exterminateDownload(final CacheDownload cacheDownload) {
		final RequestIdentifier identifier = cacheDownload.createIdentifier();
		downloadsInProgress.remove(identifier);
		redditDownloadsQueued.remove(identifier);
		downloadsQueued.remove(identifier);
	}

	private class RedditQueueProcessor extends Thread {
		@Override
		public void run() {

			android.os.Process.setThreadPriority(android.os.Process.THREAD_PRIORITY_BACKGROUND);

			while(true) {

				synchronized(this) {
					final CacheDownload download = getNextRedditInQueue();
					new CacheDownloadThread(PrioritisedDownloadQueue.this, download, true);
				}

				try {
					// TODO allow for burstiness
					sleep(2000); // Delay imposed by reddit API restrictions.
				} catch (InterruptedException e) {
					throw new RuntimeException(e);
				}
			}

		}
	}
}
