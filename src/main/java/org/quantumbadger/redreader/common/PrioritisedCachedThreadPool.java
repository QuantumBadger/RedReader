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

import androidx.annotation.NonNull;

import java.util.ArrayList;

public class PrioritisedCachedThreadPool {

	private final ArrayList<Task> mTasks = new ArrayList<>(16);
	private final Executor mExecutor = new Executor();

	private final int mMaxThreads;
	private final String mThreadName;
	private int mThreadNameCount = 0;

	private int mRunningThreads;
	private int mIdleThreads;

	public PrioritisedCachedThreadPool(final int threads, final String threadName) {
		mMaxThreads = threads;
		mThreadName = threadName;
	}

	public void add(final Task task) {

		synchronized(mTasks) {
			mTasks.add(task);
			mTasks.notifyAll();

			if(mIdleThreads < 1 && mRunningThreads < mMaxThreads) {
				mRunningThreads++;
				new Thread(mExecutor, mThreadName + " " + (mThreadNameCount++)).start();
			}
		}
	}

	public static abstract class Task {

		@NonNull public abstract Priority getPriority();

		public abstract void run();
	}

	private final class Executor implements Runnable {

		@Override
		public void run() {

			while(true) {

				Task taskToRun = null;

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

					int taskIndex = -1;
					for(int i = 0; i < mTasks.size(); i++) {
						if(taskToRun == null || mTasks.get(i).getPriority()
								.isHigherPriorityThan(taskToRun.getPriority())) {
							taskToRun = mTasks.get(i);
							taskIndex = i;
						}
					}

					mTasks.remove(taskIndex);
				}

				assert taskToRun != null;
				taskToRun.run();
			}
		}
	}
}
