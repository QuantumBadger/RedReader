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

import java.util.ArrayDeque;

public class CachedThreadPool {

	private final ArrayDeque<Runnable> mTasks = new ArrayDeque<>(16);
	private final Executor mExecutor = new Executor();

	private final int mMaxThreads;
	private final String mThreadName;
	private int mThreadNameCount = 0;

	private int mRunningThreads;
	private int mIdleThreads;

	public CachedThreadPool(final int threads, final String threadName) {
		mMaxThreads = threads;
		mThreadName = threadName;
	}

	public void add(final Runnable task) {

		synchronized(mTasks) {
			mTasks.addLast(task);
			mTasks.notifyAll();

			if(mIdleThreads < 1 && mRunningThreads < mMaxThreads) {
				mRunningThreads++;
				new Thread(mExecutor, mThreadName + " " + (mThreadNameCount++)).start();
			}
		}
	}

	private final class Executor implements Runnable {

		@Override
		public void run() {

			while(true) {

				final Runnable taskToRun;

				synchronized(mTasks) {

					if(mTasks.isEmpty()) {

						mIdleThreads++;

						try {
							mTasks.wait(30_000);
						} catch(final InterruptedException e) {
							throw new RuntimeException(e);
						} finally {
							mIdleThreads--;
						}

						if(mTasks.isEmpty()) {
							mRunningThreads--;
							return;
						}
					}

					taskToRun = mTasks.removeFirst();
				}

				taskToRun.run();
			}
		}
	}
}
