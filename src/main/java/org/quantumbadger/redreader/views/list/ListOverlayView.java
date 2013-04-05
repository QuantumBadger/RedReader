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
import android.os.Handler;
import android.os.Looper;
import android.os.Message;
import android.util.Log;
import android.view.MotionEvent;
import android.view.View;
import org.holoeverywhere.widget.FrameLayout;
import org.holoeverywhere.widget.ListView;

import java.util.Timer;
import java.util.TimerTask;

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

	private final Object touchEventLock = new Object();

	private MotionEvent downStart = null;
	private RRTouchable downItem = null;
	private TouchEventType touchEventType = null;
	private int downId = -1;

	private Timer clickTimer = null;

	@Override
	public boolean onInterceptTouchEvent(final MotionEvent ev) {

		synchronized(touchEventLock) {

			final int action = ev.getAction() & MotionEvent.ACTION_MASK;

			// TODO change back to switch/case
			if(action == MotionEvent.ACTION_DOWN) {

				downStart = MotionEvent.obtain(ev);

				final int itemIndex = child.pointToPosition((int)downStart.getX(), (int)downStart.getY());

				if(itemIndex < 0) return true;

				final View downItemView = child.getChildAt(itemIndex - child.getFirstVisiblePosition());
				if(!(downItemView instanceof RRTouchable)) return false;

				downItem = (RRTouchable)downItemView;
				downId = ev.getPointerId(0);

				downItem.rrOnFingerDown();

				if(clickTimer != null) {
					Log.e("clickTimer", "Timer still exists on intercept entry");
					cancelClickTimer();
				}

				if(downItem.rrAllowLongClick()) {

					final Handler longClickHandler = new Handler(Looper.getMainLooper()) {
						@Override
						public void handleMessage(final Message msg) {
							downItem.rrOnLongClick();
						}
					};

					clickTimer = new Timer("Post list long click timer");
					clickTimer.schedule(new TimerTask() {

						@Override
						public void run() {
							synchronized(touchEventLock) {
								touchEventType = TouchEventType.LONGCLICK;
								longClickHandler.sendEmptyMessage(0);
							}
						}
					}, 300);
				}

				touchEventType = TouchEventType.CLICK;

				return false;

			} else if(action == MotionEvent.ACTION_UP
					|| action == MotionEvent.ACTION_OUTSIDE
					|| action == MotionEvent.ACTION_CANCEL
					|| action == MotionEvent.ACTION_POINTER_UP) {

				if(ev.getPointerId(ev.getActionIndex()) != downId) return false;
				downId = -1;

				cancelClickTimer();

				if(downItem != null) {
					if(touchEventType == null) Log.i("Item selected", downItem.toString());
					if(touchEventType == TouchEventType.CLICK) downItem.rrOnClick((int)downStart.getRawX(), (int)downStart.getRawY());
					downItem.rrOnFingerUp();
				} else {
					Log.e("LOV", "downItem was null...");
				}

				touchEventType = null;

				return false;

			} else if(action == MotionEvent.ACTION_MOVE) {

				if(downStart == null) return false;

				if(ev.getPointerId(ev.getActionIndex()) != downId) return false;

				final float xDelta = ev.getX() - downStart.getX(), yDelta = ev.getY() - downStart.getY();

				if(touchEventType == null || touchEventType == TouchEventType.CLICK) {

					if(Math.abs(yDelta) > 20 || (Math.abs(yDelta) > 3 * Math.abs(xDelta) && yDelta > 10)) {
						touchEventType = TouchEventType.VERTICAL;
						downItem.rrOnFingerUp();

					} else if (Math.abs(xDelta) > 30 || (Math.abs(xDelta) > 3 * Math.abs(yDelta) && xDelta > 15)) {
						touchEventType = TouchEventType.HORIZONTAL;

					} else {
						return false;
					}
				}

				cancelClickTimer();

				switch(touchEventType) {

					case HORIZONTAL:
						downItem.rrOnSwipeDelta(xDelta);
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
				Log.i("LOV", String.format("ACTION unknown (%d): index %d, id %d", action, ev.getActionIndex(), downId));
				return false;
			}
		}
	}

	private void cancelClickTimer() {
		synchronized(touchEventLock) {
			if(clickTimer != null) {
				clickTimer.cancel();
				clickTimer = null;
			}
		}
	}

	@Override
	public void requestDisallowInterceptTouchEvent(final boolean disallowIntercept) {

		super.requestDisallowInterceptTouchEvent(disallowIntercept);

		synchronized (touchEventLock) {
			touchEventType = TouchEventType.UNKNOWN;
			cancelClickTimer();
		}
	}

	@Override
	public boolean onTouchEvent(final MotionEvent ev) {
		return onInterceptTouchEvent(ev);
	}
}
