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

package org.quantumbadger.redreader.views;

import android.annotation.TargetApi;
import android.os.Build;
import androidx.annotation.NonNull;
import android.view.Choreographer;

@TargetApi(Build.VERSION_CODES.JELLY_BEAN)
public class RRChoreographerModern extends RRChoreographer implements Choreographer.FrameCallback {

	static final RRChoreographerModern INSTANCE = new RRChoreographerModern();

	private static final Choreographer CHOREOGRAPHER = Choreographer.getInstance();

	private final Callback[] mCallbacks = new Callback[128];
	private int mCallbackCount = 0;
	private boolean mPosted = false;

	private RRChoreographerModern() {}

	@Override
	public void postFrameCallback(@NonNull final Callback callback) {
		mCallbacks[mCallbackCount] = callback;
		mCallbackCount++;

		if(!mPosted) {
			CHOREOGRAPHER.postFrameCallback(this);
			mPosted = true;
		}
	}

	@Override
	public void doFrame(final long frameTimeNanos) {

		final int callbackCount = mCallbackCount;
		mPosted = false;
		mCallbackCount = 0;

		for(int i = 0; i < callbackCount; i++) {
			mCallbacks[i].doFrame(frameTimeNanos);
		}


	}
}
