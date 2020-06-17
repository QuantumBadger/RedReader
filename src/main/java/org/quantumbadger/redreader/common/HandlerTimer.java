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

import android.os.Handler;
import androidx.annotation.UiThread;
import android.util.SparseBooleanArray;

public class HandlerTimer {

	private final Handler mHandler;

	private int mNextId = 0;

	private final SparseBooleanArray mTimers = new SparseBooleanArray();

	public HandlerTimer(final Handler handler) {
		mHandler = handler;
	}

	private int getNextId() {

		mNextId++;

		while(mTimers.get(mNextId, false) || mNextId == 0) {
			mNextId++;
		}

		return mNextId;
	}

	// Should never return 0
	@UiThread
	public int setTimer(final long delayMs, final Runnable runnable) {

		final int id = getNextId();
		mTimers.put(id, true);

		mHandler.postDelayed(new Runnable() {
			@Override
			public void run() {

				if(!mTimers.get(id, false)) {
					return;
				}

				mTimers.delete(id);

				runnable.run();
			}
		}, delayMs);

		return id;
	}

	public void cancelTimer(final int timerId) {
		mTimers.delete(timerId);
	}
}
