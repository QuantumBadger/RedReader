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

package org.quantumbadger.redreader.views.video;

import android.annotation.SuppressLint;
import android.view.MotionEvent;
import android.view.View;

import androidx.annotation.NonNull;

import org.quantumbadger.redreader.common.MutableFloatPoint2D;
import org.quantumbadger.redreader.common.collections.Stack;
import org.quantumbadger.redreader.views.imageview.BasicGestureHandler;
import org.quantumbadger.redreader.views.imageview.FingerTracker;

/**
 * Touch handler for the video player. Behaves like
 * {@link BasicGestureHandler} for taps and single-finger horizontal swipes,
 * but additionally supports pinch-to-zoom and (while zoomed in)
 * single-finger panning, applied to an {@link ExoPlayerWrapperView}.
 */
public class VideoGestureHandler
		implements View.OnTouchListener, FingerTracker.FingerListener {

	private static final long TAP_MAX_DURATION_MS = 300;

	private enum TouchState {
		ONE_FINGER_DOWN,
		ONE_FINGER_DRAG,
		TWO_FINGER_PINCH
	}

	@NonNull private final BasicGestureHandler.Listener mListener;
	@NonNull private final ExoPlayerWrapperView mPlayerView;

	private final FingerTracker mFingerTracker = new FingerTracker(this);

	private final float mScreenDensity;

	private TouchState mCurrentTouchState;

	private FingerTracker.Finger mDragFinger;
	private FingerTracker.Finger mPinchFinger1;
	private FingerTracker.Finger mPinchFinger2;
	private final Stack<FingerTracker.Finger> mSpareFingers = new Stack<>(8);

	private final MutableFloatPoint2D mTmpPoint1 = new MutableFloatPoint2D();
	private final MutableFloatPoint2D mTmpPoint2 = new MutableFloatPoint2D();

	public VideoGestureHandler(
			@NonNull final BasicGestureHandler.Listener listener,
			@NonNull final ExoPlayerWrapperView playerView) {

		mListener = listener;
		mPlayerView = playerView;
		mScreenDensity = playerView.getResources().getDisplayMetrics().density;
	}

	@SuppressLint("ClickableViewAccessibility")
	@Override
	public boolean onTouch(final View v, final MotionEvent event) {
		mFingerTracker.onTouchEvent(event);
		return true;
	}

	@Override
	public void onFingerDown(final FingerTracker.Finger finger) {

		if(mCurrentTouchState == null) {
			mCurrentTouchState = TouchState.ONE_FINGER_DOWN;
			mDragFinger = finger;

		} else {
			switch(mCurrentTouchState) {

				case ONE_FINGER_DRAG:
					mListener.onHorizontalSwipeEnd();

					// Deliberate fallthrough

				case ONE_FINGER_DOWN:
					mCurrentTouchState = TouchState.TWO_FINGER_PINCH;
					mPinchFinger1 = mDragFinger;
					mPinchFinger2 = finger;
					mDragFinger = null;
					break;

				default:
					mSpareFingers.push(finger);
					break;
			}
		}
	}

	@Override
	public void onFingersMoved() {

		if(mCurrentTouchState == null) {
			return;
		}

		switch(mCurrentTouchState) {

			case ONE_FINGER_DOWN: {

				if(mDragFinger.mTotalPosDifference.distanceSquared()
						>= 100f * mScreenDensity * mScreenDensity) {
					mCurrentTouchState = TouchState.ONE_FINGER_DRAG;
				}

				// Deliberate fall-through
			}

			case ONE_FINGER_DRAG:
				if(mPlayerView.isZoomedIn()) {
					mPlayerView.panBy(
							mDragFinger.mPosDifference.x,
							mDragFinger.mPosDifference.y);
				} else {
					mListener.onHorizontalSwipe(mDragFinger.mTotalPosDifference.x);
				}
				break;

			case TWO_FINGER_PINCH: {

				final double oldDistance =
						mPinchFinger1.mLastPos.euclideanDistanceTo(mPinchFinger2.mLastPos);
				final double newDistance =
						mPinchFinger1.mCurrentPos.euclideanDistanceTo(
								mPinchFinger2.mCurrentPos);

				final MutableFloatPoint2D oldCentre = mTmpPoint1;
				mPinchFinger1.mLastPos.add(mPinchFinger2.mLastPos, oldCentre);
				oldCentre.scale(0.5);

				final MutableFloatPoint2D newCentre = mTmpPoint2;
				mPinchFinger1.mCurrentPos.add(mPinchFinger2.mCurrentPos, newCentre);
				newCentre.scale(0.5);

				if(oldDistance > 0) {
					mPlayerView.scaleBy(
							(float)(newDistance / oldDistance),
							newCentre.x,
							newCentre.y);
				}

				mPlayerView.panBy(
						newCentre.x - oldCentre.x,
						newCentre.y - oldCentre.y);

				break;
			}
		}
	}

	@Override
	public void onFingerUp(final FingerTracker.Finger finger) {

		if(mSpareFingers.remove(finger)) {
			return;
		}

		if(mCurrentTouchState == null) {
			return;
		}

		switch(mCurrentTouchState) {

			case ONE_FINGER_DOWN:

				mListener.onHorizontalSwipeEnd();

				if(finger.mDownDuration < TAP_MAX_DURATION_MS) {
					mListener.onSingleTap();
				}

				mCurrentTouchState = null;
				mDragFinger = null;
				break;

			case ONE_FINGER_DRAG:

				mListener.onHorizontalSwipeEnd();

				if(mSpareFingers.isEmpty()) {
					mCurrentTouchState = null;
					mDragFinger = null;
				} else {
					mDragFinger = mSpareFingers.pop();
				}

				break;

			case TWO_FINGER_PINCH:

				if(mSpareFingers.isEmpty()) {
					mCurrentTouchState = TouchState.ONE_FINGER_DRAG;
					mDragFinger =
							(mPinchFinger1 == finger) ? mPinchFinger2 : mPinchFinger1;
					mPinchFinger1 = null;
					mPinchFinger2 = null;

				} else {
					if(mPinchFinger1 == finger) {
						mPinchFinger1 = mSpareFingers.pop();
					} else {
						mPinchFinger2 = mSpareFingers.pop();
					}
				}
				break;
		}
	}
}
