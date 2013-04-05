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

import org.quantumbadger.redreader.activities.BugReportActivity;

final class CacheDownloadThread extends Thread {

	private final PrioritisedDownloadQueue queue;
	private final CacheDownload singleDownload;

	public CacheDownloadThread(final PrioritisedDownloadQueue queue, final boolean start) {
		this(queue, null, start);
	}

	public CacheDownloadThread(final PrioritisedDownloadQueue queue, final CacheDownload singleDownload, final boolean start) {
		this.queue = queue;
		this.singleDownload = singleDownload;
		if(start) start();
	}

	@Override
	public void run() {

		android.os.Process.setThreadPriority(android.os.Process.THREAD_PRIORITY_BACKGROUND);

		if(singleDownload != null) {
			singleDownload.doDownload();

		} else {

			while(true) {

				CacheDownload download = null;

				try {
					download = queue.getNextInQueue();
					download.doDownload();
				} catch(Throwable t) {
					if(download != null) {
						BugReportActivity.handleGlobalError(download.initiator.context, t);
					} else {
						t.printStackTrace();
					}
				}
			}
		}
	}
}
