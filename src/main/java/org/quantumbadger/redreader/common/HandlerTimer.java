package org.quantumbadger.redreader.common;

import android.os.Handler;
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

	// Must be called from the handler thread
	// Should never return 0
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
