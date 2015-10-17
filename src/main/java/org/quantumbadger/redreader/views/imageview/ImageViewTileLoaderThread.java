/*******************************************************************************
 * This file is part of Scroll.
 *
 * Scroll is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Scroll is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Scroll.  If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/

package org.quantumbadger.redreader.views.imageview;

import org.quantumbadger.redreader.common.TriggerableThread;
import org.quantumbadger.redreader.common.collections.Stack;

public class ImageViewTileLoaderThread {

	private final InternalThread mThread = new InternalThread(new InternalRunnable(), 0);
	private final Stack<ImageViewTileLoader> mStack = new Stack<ImageViewTileLoader>(128);

	public void enqueue(ImageViewTileLoader tile) {

		synchronized(mStack) {
			mStack.push(tile);
			mThread.trigger();
		}
	}

	private class InternalRunnable implements Runnable {

		@Override
		public void run() {

			while(true) {

				final ImageViewTileLoader tile;

				synchronized(mStack) {

					if(mStack.isEmpty()) {
						return;
					}

					tile = mStack.pop();
				}

				tile.doPrepare();
			}

		}
	}

	private class InternalThread extends TriggerableThread {

		public InternalThread(Runnable task, long initialDelay) {
			super(task, initialDelay);
		}
	}
}
