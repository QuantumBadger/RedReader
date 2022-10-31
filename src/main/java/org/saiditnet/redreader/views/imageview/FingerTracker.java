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

import org.saiditnet.redreader.common.MutableFloatPoint2D;

public class FingerTracker {

	public interface FingerListener {
		void onFingerDown(Finger finger);

		void onFingersMoved();

		void onFingerUp(Finger finger);
	}

	private final Finger[] mFingers = new Finger[10];
	private final FingerListener mListener;

	public FingerTracker(FingerListener mListener) {

		this.mListener = mListener;

		for(int i = 0; i < mFingers.length; i++) {
			mFingers[i] = new Finger();
		}
	}

	public void onTouchEvent(MotionEvent event) {

		switch(event.getActionMasked()) {

			case MotionEvent.ACTION_DOWN:
			case MotionEvent.ACTION_POINTER_DOWN:

				for(final Finger f : mFingers) {
					if(!f.mActive) {
						f.onDown(event);
						mListener.onFingerDown(f);
						break;
					}
				}

				break;

			case MotionEvent.ACTION_MOVE:

				for(Finger finger : mFingers) {
					if(finger.mActive) {
						finger.onMove(event);
					}
				}

				mListener.onFingersMoved();

				break;

			case MotionEvent.ACTION_POINTER_UP:
			case MotionEvent.ACTION_UP:

				final int id = event.getPointerId(event.getActionIndex());

				for(final Finger f : mFingers) {
					if(f.mActive && f.mAndroidId == id) {
						f.onUp(event);
						mListener.onFingerUp(f);
						break;
					}
				}

				break;
		}
	}

	public class Finger {

		boolean mActive = false;

		int mAndroidId;

		final MutableFloatPoint2D
				mStartPos = new MutableFloatPoint2D(),
				mCurrentPos = new MutableFloatPoint2D(),
				mLastPos = new MutableFloatPoint2D(),
				mPosDifference = new MutableFloatPoint2D(),
				mTotalPosDifference = new MutableFloatPoint2D();

		long mDownStartTime, mDownDuration;

		public void onDown(final MotionEvent event) {
			final int index = event.getActionIndex();
			mActive = true;
			mAndroidId = event.getPointerId(index);
			mCurrentPos.set(event, index);
			mLastPos.set(mCurrentPos);
			mStartPos.set(mCurrentPos);
			mPosDifference.reset();
			mTotalPosDifference.reset();
			mDownStartTime = event.getDownTime();
			mDownDuration = 0;
		}

		public void onMove(final MotionEvent event) {
			final int index = event.findPointerIndex(mAndroidId);
			if(index >= 0) {
				mLastPos.set(mCurrentPos);
				mCurrentPos.set(event, index);
				mCurrentPos.sub(mLastPos, mPosDifference);
				mCurrentPos.sub(mStartPos, mTotalPosDifference);
				mDownDuration = event.getEventTime() - mDownStartTime;
			}
		}

		public void onUp(final MotionEvent event) {

			mLastPos.set(mCurrentPos);
			mCurrentPos.set(event, event.getActionIndex());
			mCurrentPos.sub(mLastPos, mPosDifference);
			mCurrentPos.sub(mStartPos, mTotalPosDifference);
			mDownDuration = event.getEventTime() - mDownStartTime;

			mActive = false;
		}
	}
}
