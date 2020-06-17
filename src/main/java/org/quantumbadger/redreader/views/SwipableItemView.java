/*******************************************************************************
 * This file is part of RedReader.
 * <p>
 * RedReader is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * <p>
 * RedReader is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * <p>
 * You should have received a copy of the GNU General Public License
 * along with RedReader.  If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/

package org.quantumbadger.redreader.views;

import android.content.Context;
import androidx.annotation.NonNull;
import android.view.MotionEvent;
import android.widget.FrameLayout;
import org.quantumbadger.redreader.common.General;

public abstract class SwipableItemView extends FrameLayout {

	private MotionEvent mSwipeStart;
	private int mSwipeStartPointerId = -1;

	private boolean mSwipingEnabled = true;

	private boolean mSwipeInProgress = false;

	private float mCurrentSwipeDelta = 0;
	private float mOverallSwipeDelta = 0;

	private final SwipeHistory mSwipeHistory = new SwipeHistory(30);
	private float mVelocity;

	private SwipeAnimation mCurrentSwipeAnimation;

	public SwipableItemView(@NonNull final Context context) {
		super(context);
	}

	protected abstract void onSwipeFingerDown(int x, int y, final float xOffsetPixels, boolean wasOldSwipeInterrupted);
	protected abstract void onSwipeDeltaChanged(float dx);

	protected abstract boolean allowSwipingLeft();
	protected abstract boolean allowSwipingRight();

	public void setSwipingEnabled(final boolean swipingEnabled) {
		mSwipingEnabled = swipingEnabled;
	}

	protected void resetSwipeState() {
		mSwipeHistory.clear();
		mSwipeStart = null;
		mSwipeStartPointerId = -1;
		mSwipeInProgress = false;
		mCurrentSwipeDelta = 0;
		mOverallSwipeDelta = 0;
		cancelSwipeAnimation();

		updateOffset();
	}

	private void updateOffset() {

		final float overallPos = mOverallSwipeDelta + mCurrentSwipeDelta;

		if((overallPos > 0 && !allowSwipingRight()) || (overallPos < 0 && !allowSwipingLeft())) {
			mOverallSwipeDelta = -mCurrentSwipeDelta;
		}

		onSwipeDeltaChanged(mOverallSwipeDelta + mCurrentSwipeDelta);
	}

	private void onFingerDown(int x, int y) {

		final boolean wasOldSwipeInterrupted = (mCurrentSwipeAnimation != null) || (mOverallSwipeDelta != 0);

		cancelSwipeAnimation();
		mSwipeHistory.clear();
		mVelocity = 0;
		mOverallSwipeDelta += mCurrentSwipeDelta;
		mCurrentSwipeDelta = 0;
		onSwipeFingerDown(x, y, mOverallSwipeDelta, wasOldSwipeInterrupted);
	}

	private void onFingerSwipeMove() {
		mSwipeHistory.add(mCurrentSwipeDelta, System.currentTimeMillis());
		updateOffset();
	}

	private void onSwipeEnd() {

		if(mSwipeHistory.size() >= 2) {
			mVelocity = (mSwipeHistory.getMostRecent() - mSwipeHistory.getAtTimeAgoMs(100)) * 10;
		} else {
			mVelocity = 0;
		}

		mOverallSwipeDelta += mCurrentSwipeDelta;
		mCurrentSwipeDelta = 0;

		animateSwipeToRestPosition();
	}

	private void onSwipeCancelled() {

		mVelocity = 0;

		mOverallSwipeDelta += mCurrentSwipeDelta;
		mCurrentSwipeDelta = 0;

		animateSwipeToRestPosition();
	}

	private void animateSwipeToRestPosition() {
		final LiveDHM.Params params = new LiveDHM.Params(); // TODO account for screen dpi!
		params.startPosition = mOverallSwipeDelta;
		params.startVelocity = mVelocity;
		startSwipeAnimation(new SwipeAnimation(params));
	}

	private void startSwipeAnimation(final SwipeAnimation animation) {

		if(mCurrentSwipeAnimation != null) {
			mCurrentSwipeAnimation.stop();
		}

		mCurrentSwipeAnimation = animation;
		mCurrentSwipeAnimation.start();
	}

	private void cancelSwipeAnimation() {

		if(mCurrentSwipeAnimation != null) {
			mCurrentSwipeAnimation.stop();
			mCurrentSwipeAnimation = null;
		}
	}

	private class SwipeAnimation extends RRDHMAnimation {

		private SwipeAnimation(final LiveDHM.Params params) {
			super(params);
		}

		@Override
		protected void onUpdatedPosition(final float position) {
			mOverallSwipeDelta = position;
			updateOffset();
		}

		@Override
		protected void onEndPosition(final float endPosition) {
			mOverallSwipeDelta = endPosition;
			updateOffset();
			mCurrentSwipeAnimation = null;
		}

	}

	@Override
	public boolean onInterceptTouchEvent(final MotionEvent ev) {

		if(mSwipeInProgress) {
			return true;
		}

		if(swipeStartLogic(ev)) {
			return true;
		}

		return super.onInterceptTouchEvent(ev);
	}

	private boolean swipeStartLogic(final MotionEvent ev) {

		if(mSwipeInProgress) {
			throw new RuntimeException();
		}

		if(!mSwipingEnabled) {
			return false;
		}

		final int action = ev.getAction() & MotionEvent.ACTION_MASK;
		final int pointerId = ev.getPointerId(ev.getActionIndex());

		if(action == MotionEvent.ACTION_DOWN
			   || action == MotionEvent.ACTION_POINTER_DOWN) {

			if(mSwipeStart != null) {
				// We can receive duplicate DOWN events because we're visited in both
				// the onInterceptTouchEvent AND onTouchEvent methods
				return false;
			}

			mSwipeStart = MotionEvent.obtain(ev);
			mSwipeStartPointerId = pointerId;
			onFingerDown((int)ev.getX(), (int)ev.getY());

		} else if(action == MotionEvent.ACTION_MOVE) {

			if(mSwipeStart == null) {
				return false;
			}

			if(pointerId != mSwipeStartPointerId) {
				return false;
			}

			final float xDelta = ev.getX() - mSwipeStart.getX();
			final float yDelta = ev.getY() - mSwipeStart.getY();

			final int minXDelta = General.dpToPixels(getContext(), 20);
			final int maxYDelta = General.dpToPixels(getContext(), 10);

			if(Math.abs(xDelta) >= minXDelta && Math.abs(yDelta) <= maxYDelta) {
				mSwipeInProgress = true;
				mCurrentSwipeDelta = 0;
				requestDisallowInterceptTouchEvent(true);
				cancelLongPress();
				return true;
			}

		} else if(action == MotionEvent.ACTION_CANCEL
					  || action == MotionEvent.ACTION_UP
					  || action == MotionEvent.ACTION_POINTER_UP
					  || action == MotionEvent.ACTION_OUTSIDE) {

			if(pointerId != mSwipeStartPointerId) {
				return false;
			}

			mSwipeStart = null;

			onSwipeCancelled();
		}

		return false;
	}

	@Override
	public boolean onTouchEvent(MotionEvent ev) {

		if(!mSwipeInProgress) {

			if(swipeStartLogic(ev)) {
				return true;
			}

			return super.onTouchEvent(ev);
		}

		if(mSwipeStart == null) {
			throw new RuntimeException();
		}

		final int action = ev.getAction() & MotionEvent.ACTION_MASK;
		final int pointerId = ev.getPointerId(ev.getActionIndex());

		if(pointerId != mSwipeStartPointerId) {
			return false;
		}

		if(action == MotionEvent.ACTION_MOVE) {

			mCurrentSwipeDelta = ev.getX() - mSwipeStart.getX();
			onFingerSwipeMove();

		} else if(action == MotionEvent.ACTION_CANCEL
					  || action == MotionEvent.ACTION_UP
					  || action == MotionEvent.ACTION_POINTER_UP
					  || action == MotionEvent.ACTION_OUTSIDE) {

			mSwipeStart = null;
			mSwipeInProgress = false;
			requestDisallowInterceptTouchEvent(false);

			onSwipeEnd();
		}

		return true;
	}
}
