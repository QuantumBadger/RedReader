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

package org.quantumbadger.redreader.ui.frag;

import android.content.Context;
import android.util.AttributeSet;
import android.util.Log;
import android.view.MotionEvent;
import android.view.View;

public abstract class RRViewWrapper extends View {

	private final TouchEvent reusableTouchEvent = new TouchEvent();

	public RRViewWrapper(Context context) {
		super(context);
	}

	public RRViewWrapper(Context context, AttributeSet attrs) {
		super(context, attrs);
	}

	public RRViewWrapper(Context context, AttributeSet attrs, int defStyle) {
		super(context, attrs, defStyle);
	}

	protected abstract void onTouchEvent(TouchEvent e);

	public static enum TouchEventType {
		START, MOVE, FINISH, CANCEL
	}

	public final class TouchEvent {

		public int pointerCount = 0;

		public TouchEventType type;

		public float[] xStart, yStart;
		public float[] xPos, yPos;
		public int[] pointerId;

		public long startTime, currentTime;

		public boolean isFollowingPointerUp = false;

		private void start(float[] xPos, float[] yPos, int[] pointerId) {
			this.xPos = xPos;
			this.yPos = yPos;
			this.xStart = xPos.clone();
			this.yStart = yPos.clone();
			this.pointerId = pointerId;
		}

		private void updatePositions(MotionEvent rawEvent) {
			for(int i = 0; i < xPos.length; i++) {
				xPos[i] = rawEvent.getX(i);
				yPos[i] = rawEvent.getY(i);
			}
		}

		private void cancel(MotionEvent rawEvent, boolean finish) {

			Log.i("RRTouch", String.format("Pointers: %d, time: %d ms", pointerCount, rawEvent.getEventTime() - startTime));

			type = finish ? TouchEventType.FINISH : TouchEventType.CANCEL;
			updatePositions(rawEvent);
			onTouchEvent(this);
			type = null;
		}

		private void onTouchEventBegin(long timeMs, MotionEvent rawEvent) {

			pointerCount = rawEvent.getPointerCount();
			this.isFollowingPointerUp = false;

			final float[] xPos = new float[pointerCount];
			final float[] yPos = new float[pointerCount];
			final int[] pointerId = new int[pointerCount];

			for(int i = 0; i < pointerCount; i++) {
				xPos[i] = rawEvent.getX(i);
				yPos[i] = rawEvent.getY(i);
				pointerId[i] = rawEvent.getPointerId(i);
			}

			type = TouchEventType.START;
			start(xPos, yPos, pointerId);
			startTime = timeMs;
			onTouchEvent(this);
		}

		private void onTouchEventBeginFollowingPointerUp(long timeMs, MotionEvent rawEvent) {

			pointerCount = rawEvent.getPointerCount() - 1;
			this.isFollowingPointerUp = true;

			final float[] xPos = new float[pointerCount];
			final float[] yPos = new float[pointerCount];
			final int[] pointerId = new int[pointerCount];

			final int pointerUpIndex = rawEvent.getActionIndex();

			int i = 0, j = 0;

			while(j < pointerCount) {

				if(i != pointerUpIndex) {
					xPos[j] = rawEvent.getX(i);
					yPos[j] = rawEvent.getY(i);
					pointerId[j] = rawEvent.getPointerId(i);
					j++;
				}

				i++;
			}

			type = TouchEventType.START;
			start(xPos, yPos, pointerId);
			startTime = timeMs;
			onTouchEvent(this);
		}
	}

	@Override
	public final boolean onTouchEvent(final MotionEvent rawEvent) {

		final int action = rawEvent.getActionMasked();
		final TouchEvent event = this.reusableTouchEvent;

		final long cancelIfDurationLessThanMs = 100;

		final long rawEventTimeMs = rawEvent.getEventTime();
		final long eventTimeMs = event.startTime;

		event.currentTime = rawEventTimeMs;

		switch(action) {

			case MotionEvent.ACTION_POINTER_DOWN:
				if(event.type != null) {
					event.cancel(rawEvent, rawEventTimeMs - eventTimeMs > cancelIfDurationLessThanMs);
				}

			case MotionEvent.ACTION_DOWN:
				event.onTouchEventBegin(rawEventTimeMs, rawEvent);
				break;

			case MotionEvent.ACTION_POINTER_UP:
			case MotionEvent.ACTION_UP:
				if(event.type != null) event.cancel(rawEvent, true);
				if(event.pointerCount > 1) event.onTouchEventBeginFollowingPointerUp(rawEventTimeMs, rawEvent);
				break;

			case MotionEvent.ACTION_MOVE:
				if(event.type != null) {
					event.updatePositions(rawEvent);
					event.type = TouchEventType.MOVE;
					onTouchEvent(event);
				}
				break;

			case MotionEvent.ACTION_CANCEL:
				event.cancel(rawEvent, false);
				break;
		}

		return true;
	}
}
