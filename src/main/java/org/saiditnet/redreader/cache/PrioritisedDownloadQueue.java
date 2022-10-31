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

package org.saiditnet.redreader.cache;

import android.content.Context;
import org.saiditnet.redreader.common.PrioritisedCachedThreadPool;

import java.util.HashSet;


class PrioritisedDownloadQueue {

	private final HashSet<CacheDownload> redditDownloadsQueued = new HashSet<>();

	private final PrioritisedCachedThreadPool mDownloadThreadPool = new PrioritisedCachedThreadPool(5, "Download");

	public PrioritisedDownloadQueue(final Context context) {
		new RedditQueueProcessor().start();
	}

	public synchronized void add(final CacheRequest request, final CacheManager manager) {

		final CacheDownload download = new CacheDownload(request, manager, this);

		if(request.queueType == CacheRequest.DOWNLOAD_QUEUE_REDDIT_API) {
			redditDownloadsQueued.add(download);
			notifyAll();

		} else if(request.queueType == CacheRequest.DOWNLOAD_QUEUE_IMMEDIATE
				|| request.queueType == CacheRequest.DOWNLOAD_QUEUE_IMGUR_API) {
			new CacheDownloadThread(download, true, "Cache Download Thread: Immediate");

		} else {
			mDownloadThreadPool.add(download);
		}
	}

	private synchronized CacheDownload getNextRedditInQueue() {

		while(redditDownloadsQueued.isEmpty()) {
			try { wait(); } catch (InterruptedException e) { throw new RuntimeException(e); }
		}

		CacheDownload next = null;

		for(final CacheDownload entry : redditDownloadsQueued) {
			if(next == null || entry.isHigherPriorityThan(next)) {
				next = entry;
			}
		}

		redditDownloadsQueued.remove(next);

		return next;
	}

	private class RedditQueueProcessor extends Thread {

		public RedditQueueProcessor() {
			super("Reddit Queue Processor");
		}

		@Override
		public void run() {

			while(true) {

				synchronized(this) {
					final CacheDownload download = getNextRedditInQueue();
					new CacheDownloadThread(download, true, "Cache Download Thread: Reddit");
				}

				try {
					sleep(1200); // Delay imposed by reddit API restrictions.
				} catch (InterruptedException e) {
					throw new RuntimeException(e);
				}
			}

		}
	}
}
