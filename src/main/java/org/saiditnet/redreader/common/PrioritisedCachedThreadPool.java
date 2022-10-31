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

package org.saiditnet.redreader.common;

import java.util.ArrayList;

public class PrioritisedCachedThreadPool {

	private final ArrayList<Task> mTasks = new ArrayList<>(16);
	private final Executor mExecutor = new Executor();

	private final int mMaxThreads;
	private final String mThreadName;
	private int mThreadNameCount = 0;

	private int mRunningThreads, mIdleThreads;

	public PrioritisedCachedThreadPool(int threads, String threadName) {
		mMaxThreads = threads;
		mThreadName = threadName;
	}

	public void add(Task task) {

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

		public boolean isHigherPriorityThan(Task o) {
			return getPrimaryPriority() < o.getPrimaryPriority()
					|| getSecondaryPriority() < o.getSecondaryPriority();
		}

		public abstract int getPrimaryPriority();
		public abstract int getSecondaryPriority();
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
							mTasks.wait(30000);
						} catch(InterruptedException e) {
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
						if(taskToRun == null || mTasks.get(i).isHigherPriorityThan(taskToRun)) {
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
