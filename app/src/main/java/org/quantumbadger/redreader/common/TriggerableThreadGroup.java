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

public class TriggerableThreadGroup {

	private final TriggerableThread[] mThreads;
	private int mNextThreadToTrigger = 0;

	public TriggerableThreadGroup(final int threads, final Runnable task) {

		mThreads = new TriggerableThread[threads];

		for(int i = 0; i < threads; i++) {
			mThreads[i] = new TriggerableThread(task, 0);
		}
	}

	public void triggerOne() {
		mThreads[mNextThreadToTrigger].trigger();
		mNextThreadToTrigger = (mNextThreadToTrigger + 1) % mThreads.length;
	}
}
