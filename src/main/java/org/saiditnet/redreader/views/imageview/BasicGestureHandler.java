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

package org.saiditnet.redreader.views.imageview;

import android.view.MotionEvent;
import android.view.View;

public class BasicGestureHandler implements View.OnTouchListener, FingerTracker.FingerListener {

	public BasicGestureHandler(final Listener listener) {
		mListener = listener;
	}

	public interface Listener {
		void onSingleTap();

		void onHorizontalSwipe(float pixels);

		void onHorizontalSwipeEnd();
	}

	private final FingerTracker mFingerTracker = new FingerTracker(this);

	private final Listener mListener;

	private FingerTracker.Finger mFirstFinger;
	private int mCurrentFingerCount;

	@Override
	public boolean onTouch(final View v, final MotionEvent event) {
		mFingerTracker.onTouchEvent(event);
		return true;
	}

	@Override
	public void onFingerDown(final FingerTracker.Finger finger) {

		mCurrentFingerCount++;

		if(mCurrentFingerCount > 1) {
			mFirstFinger = null;
		} else {
			mFirstFinger = finger;
		}
	}

	@Override
	public void onFingersMoved() {

		if(mFirstFinger != null) {
			mListener.onHorizontalSwipe(mFirstFinger.mTotalPosDifference.x);
		}
	}

	@Override
	public void onFingerUp(final FingerTracker.Finger finger) {

		mCurrentFingerCount--;

		if(mFirstFinger != null) {

			mListener.onHorizontalSwipeEnd();

			// TODO
			if(mFirstFinger.mDownDuration < 300
					&& mFirstFinger.mPosDifference.x < 20
					&& mFirstFinger.mPosDifference.y < 20) {

				mListener.onSingleTap();
			}

			mFirstFinger = null;
		}
	}
}
