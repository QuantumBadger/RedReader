package org.quantumbadger.redreader.views.imageview;

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
