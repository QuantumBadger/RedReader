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
import android.os.Looper;
import androidx.annotation.UiThread;

public class UIThreadRepeatingTimer implements Runnable {

	public interface Listener {
		void onUIThreadRepeatingTimer(UIThreadRepeatingTimer timer);
	}

	private final Handler mHandler = new Handler(Looper.getMainLooper());

	private final long mIntervalMs;
	private final Listener mListener;

	private boolean mShouldTimerRun = false;

	public UIThreadRepeatingTimer(long mIntervalMs, Listener mListener) {
		this.mIntervalMs = mIntervalMs;
		this.mListener = mListener;
	}

	@UiThread
	public void startTimer() {

		General.checkThisIsUIThread();

		mShouldTimerRun = true;
		mHandler.postDelayed(this, mIntervalMs);
	}

	@UiThread
	public void stopTimer() {

		General.checkThisIsUIThread();

		mShouldTimerRun = false;
	}


	@Override
	public void run() {

		if(mShouldTimerRun) {

			mListener.onUIThreadRepeatingTimer(this);

			if(mShouldTimerRun) {
				mHandler.postDelayed(this, mIntervalMs);
			}
		}
	}
}
