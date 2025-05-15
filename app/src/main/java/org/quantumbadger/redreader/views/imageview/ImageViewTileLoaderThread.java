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

package org.quantumbadger.redreader.views.imageview;

import android.util.Log;
import org.quantumbadger.redreader.common.TriggerableThreadGroup;

import java.util.ArrayDeque;
import java.util.Deque;

public class ImageViewTileLoaderThread {

	private final TriggerableThreadGroup mThreads;
	private final Deque<ImageViewTileLoader> mQueue = new ArrayDeque<>(128);

	public ImageViewTileLoaderThread() {

		final int threadCount = Math.max(
				1,
				Runtime.getRuntime().availableProcessors() - 1);

		Log.i("IViewTileLoaderThread", "Using thread count: " + threadCount);

		mThreads = new TriggerableThreadGroup(
				threadCount,
				new InternalRunnable());
	}

	public void enqueue(final ImageViewTileLoader tile) {

		synchronized(mQueue) {
			mQueue.addLast(tile);
			mThreads.triggerOne();
		}
	}

	private class InternalRunnable implements Runnable {

		@Override
		public void run() {

			while(true) {

				final ImageViewTileLoader tile;

				synchronized(mQueue) {

					if(mQueue.isEmpty()) {
						return;
					}

					tile = mQueue.removeFirst();
				}

				tile.doPrepare();
			}

		}
	}

}
