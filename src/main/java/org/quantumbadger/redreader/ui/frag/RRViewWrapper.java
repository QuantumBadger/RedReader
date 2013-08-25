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
import android.view.MotionEvent;
import android.view.VelocityTracker;
import android.view.View;
import org.quantumbadger.redreader.common.General;

public abstract class RRViewWrapper extends View {

	private final TouchEvent reusableTouchEvent = new TouchEvent();
	private final VelocityTracker velocityTracker = VelocityTracker.obtain();

	public RRViewWrapper(Context context) {
		super(context);
	}

	protected abstract void onTouchEvent(TouchEvent e);

	public static enum TouchEventType {
		BEGIN, MOVE, FINISH, CANCEL
	}

	public final class TouchEvent {

		public int pointerCount = 0;

		public TouchEventType type;

		public float[] xStart, yStart;
		public float[] xPos, yPos;
		public float[] xLastPos, yLastPos;
		public int[] pointerId;

		public long startTime, currentTime;

		public boolean isFollowingPointerUp = false;

		private void start(float[] xPos, float[] yPos, int[] pointerId) {
			type = TouchEventType.BEGIN;
			this.xPos = xPos;
			this.yPos = yPos;
			this.xStart = xPos.clone();
			this.yStart = yPos.clone();
			this.xLastPos = xPos.clone();
			this.yLastPos = yPos.clone();
			this.pointerId = pointerId;
		}

		private void updatePositions(MotionEvent rawEvent) {

			System.arraycopy(xPos, 0, xLastPos, 0, xPos.length);
			System.arraycopy(yPos, 0, yLastPos, 0, yPos.length);

			for(int i = 0; i < xPos.length; i++) {
				xPos[i] = rawEvent.getX(i);
				yPos[i] = rawEvent.getY(i);
			}
		}

		private void cancel(MotionEvent rawEvent, boolean finish) {
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

			start(xPos, yPos, pointerId);
			startTime = timeMs;
			onTouchEvent(this);
		}

		public double totalDistanceMovedSquared(final int pointerIndex) {
			return General.euclideanDistanceSquared(
					xPos[pointerIndex] - xStart[pointerIndex],
					yPos[pointerIndex] - yStart[pointerIndex]);
		}

		public float totalDistanceMovedX(final int pointerIndex) {
			return xPos[pointerIndex] - xStart[pointerIndex];
		}

		public float totalDistanceMovedY(final int pointerIndex) {
			return yPos[pointerIndex] - yStart[pointerIndex];
		}

		public float xDelta(int i) {
			return xPos[i] - xLastPos[i];
		}

		public float yDelta(int i) {
			return yPos[i] - yLastPos[i];
		}
	}

	public final float getVVelocity() {
		velocityTracker.computeCurrentVelocity(1000);
		return velocityTracker.getYVelocity();
	}

	@Override
	public final boolean onTouchEvent(final MotionEvent rawEvent) {

		velocityTracker.addMovement(rawEvent);

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
