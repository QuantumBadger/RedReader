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

package org.quantumbadger.redreader.views.list;

import android.content.Context;
import android.util.Log;
import android.view.MotionEvent;
import android.view.View;
import android.widget.FrameLayout;
import android.widget.ListView;
import org.quantumbadger.redreader.common.AndroidApi;
import org.quantumbadger.redreader.common.HandlerTimer;

// TODO add short highlight timer, add "onHighlight" and "onHighlightEnd" callbacks
public class ListOverlayView extends FrameLayout {

	private final ListView child;

	public ListOverlayView(final Context context, final ListView child) {
		super(context);
		addView(child);
		this.child = child;

		setClickable(false);
		setLongClickable(false);
	}

	private enum TouchEventType {
		CLICK, VERTICAL, HORIZONTAL, LONGCLICK, UNKNOWN
	}

	private MotionEvent downStart = null;
	private RRTouchable mDownItem = null;
	private TouchEventType touchEventType = null;
	private int mDownPointerId = -1;

	private final HandlerTimer mTimer = new HandlerTimer(AndroidApi.UI_THREAD_HANDLER);

	private int mTimerHighlightStart = 0;
	private int mTimerLongClick = 0;

	private boolean mLongHighlightNotified = false;

	@Override
	public boolean onInterceptTouchEvent(final MotionEvent ev) {

		final int action = ev.getAction() & MotionEvent.ACTION_MASK;

		if(action == MotionEvent.ACTION_DOWN) {

			downStart = MotionEvent.obtain(ev);

			final int itemIndex = child.pointToPosition((int)downStart.getX(), (int)downStart.getY());

			if(itemIndex < 0) return true;

			final View downItemView = child.getChildAt(itemIndex - child.getFirstVisiblePosition());
			if(!(downItemView instanceof RRTouchable)) return false;

			mDownItem = (RRTouchable)downItemView;
			mDownPointerId = ev.getPointerId(0);

			mDownItem.rrOnFingerDown();

			if(mTimerLongClick != 0 || mTimerHighlightStart != 0) {
				Log.e("clickTimer", "Timer still exists on intercept entry");
				cancelTimers();
				notifyHighlightEnd();
			}

			mTimerHighlightStart = mTimer.setTimer(75, new Runnable() {
				@Override
				public void run() {
					mTimerHighlightStart = 0;
					mDownItem.rrOnHighlightStart((int)downStart.getRawX(), (int)downStart.getRawY());
					mLongHighlightNotified = true;
				}
			});

			if(mDownItem.rrAllowLongClick()) {

				mTimerLongClick = mTimer.setTimer(300, new Runnable() {
					@Override
					public void run() {
						Log.e("LOV-DEBUG", "Long click timer!");
						mTimerLongClick = 0;
						touchEventType = TouchEventType.LONGCLICK;
						mDownItem.rrOnLongClick();
					}
				});
			}

			touchEventType = TouchEventType.CLICK;

			return false;

		} else if(action == MotionEvent.ACTION_UP
				|| action == MotionEvent.ACTION_OUTSIDE
				|| action == MotionEvent.ACTION_CANCEL
				|| action == MotionEvent.ACTION_POINTER_UP) {

			if(ev.getPointerId(ev.getActionIndex()) != mDownPointerId) return false;
			mDownPointerId = -1;

			Log.e("LOV-DEBUG", "Pointer up");
			cancelTimers();

			if(mDownItem != null) {
				if(touchEventType == null) Log.i("Item selected", mDownItem.toString());

				if(touchEventType == TouchEventType.CLICK) {

					final RRTouchable downItem = mDownItem;

					if(!mLongHighlightNotified) {
						downItem.rrOnHighlightStart((int)downStart.getRawX(), (int)downStart.getRawY());
					}

					mLongHighlightNotified = false;

					mTimer.setTimer(250, new Runnable() {
						@Override
						public void run() {
							downItem.rrOnHighlightEnd();
						}
					});

					mDownItem.rrOnClick((int)downStart.getRawX(), (int)downStart.getRawY());

				} else {
					notifyHighlightEnd();
				}

				mDownItem.rrOnFingerUp();

			} else {
				Log.e("LOV", "mDownItem was null...");
			}

			touchEventType = null;

			return false;

		} else if(action == MotionEvent.ACTION_MOVE) {

			if(downStart == null) return false;

			if(ev.getPointerId(ev.getActionIndex()) != mDownPointerId) return false;

			final float xDelta = ev.getX() - downStart.getX(), yDelta = ev.getY() - downStart.getY();

			if(touchEventType == null || touchEventType == TouchEventType.CLICK) {

				if(Math.abs(yDelta) > 20 || (Math.abs(yDelta) > 3 * Math.abs(xDelta) && yDelta > 10)) {
					touchEventType = TouchEventType.VERTICAL;
					mDownItem.rrOnFingerUp();

				} else if (Math.abs(xDelta) > 30 || (Math.abs(xDelta) > 3 * Math.abs(yDelta) && xDelta > 15)) {
					touchEventType = TouchEventType.HORIZONTAL;

				} else {
					return false;
				}
			}

			cancelTimers();
			notifyHighlightEnd();

			switch(touchEventType) {

				case HORIZONTAL:
					mDownItem.rrOnSwipeDelta(xDelta);
					return true;

				case VERTICAL:
					return false;

				case LONGCLICK:
				case CLICK:
					return true;

				default:
					return false;
			}

		} else {

			// Unknown action
			Log.i("LOV", String.format("ACTION unknown (%d): index %d, id %d", action, ev.getActionIndex(), mDownPointerId));
			return false;
		}
	}

	private void cancelTimers() {

		if(mTimerLongClick != 0) {
			Log.e("LOV-DEBUG", "Deleting long click timer");
			mTimer.cancelTimer(mTimerLongClick);
			mTimerLongClick = 0;
		}

		if(mTimerHighlightStart != 0) {
			mTimer.cancelTimer(mTimerHighlightStart);
			mTimerHighlightStart = 0;
		}
	}

	private void notifyHighlightEnd() {
		if(mLongHighlightNotified) {
			mDownItem.rrOnHighlightEnd();
			mLongHighlightNotified = false;
		}
	}

	@Override
	public void requestDisallowInterceptTouchEvent(final boolean disallowIntercept) {

		super.requestDisallowInterceptTouchEvent(disallowIntercept);

			touchEventType = TouchEventType.UNKNOWN;
			cancelTimers();
	}

	@Override
	public boolean onTouchEvent(final MotionEvent ev) {
		return onInterceptTouchEvent(ev);
	}
}
