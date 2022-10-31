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

package org.saiditnet.redreader.views;

import android.support.annotation.NonNull;

import org.saiditnet.redreader.common.AndroidCommon;

public class RRChoreographerLegacy extends RRChoreographer implements Runnable {

	static final RRChoreographerLegacy INSTANCE = new RRChoreographerLegacy();

	private final Callback[] mCallbacks = new Callback[128];
	private int mCallbackCount = 0;
	private boolean mPosted = false;

	private RRChoreographerLegacy() {}

	@Override
	public void postFrameCallback(@NonNull final Callback callback) {
		mCallbacks[mCallbackCount] = callback;
		mCallbackCount++;

		if(!mPosted) {
			AndroidCommon.UI_THREAD_HANDLER.postDelayed(this, 1000 / 60);
			mPosted = true;
		}
	}

	@Override
	public void run() {

		final long frameTimeNanos = System.nanoTime();

		final int callbackCount = mCallbackCount;
		mPosted = false;
		mCallbackCount = 0;

		for(int i = 0; i < callbackCount; i++) {
			mCallbacks[i].doFrame(frameTimeNanos);
		}
	}
}
