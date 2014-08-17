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
import org.quantumbadger.redreader.common.PrioritisedCachedThreadPool;

import java.util.HashMap;
import java.util.Map;


class PrioritisedDownloadQueue {

	private final HashMap<RequestIdentifier, CacheDownload> redditDownloadsQueued = new HashMap<RequestIdentifier, CacheDownload>();

	private final PrioritisedCachedThreadPool mDownloadThreadPool = new PrioritisedCachedThreadPool(5, "Download");

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

		if(request.isRedditApi) {
			redditDownloadsQueued.put(identifier, new CacheDownload(request, manager, this));
			notifyAll();

		} else {
			mDownloadThreadPool.add(new CacheDownload(request, manager, this));
		}
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

		return next;
	}

	private class RedditQueueProcessor extends Thread {

		public RedditQueueProcessor() {
			super("Reddit Queue Processor");
		}

		@Override
		public void run() {

			android.os.Process.setThreadPriority(android.os.Process.THREAD_PRIORITY_BACKGROUND);

			while(true) {

				synchronized(this) {
					final CacheDownload download = getNextRedditInQueue();
					new CacheDownloadThread(download, true, "Cache Download Thread: Reddit");
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
