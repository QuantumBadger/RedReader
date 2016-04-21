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
import android.view.View;
import android.view.animation.Animation;
import android.view.animation.LinearInterpolator;
import android.view.animation.Transformation;
import android.widget.FrameLayout;
import android.widget.LinearLayout;
import android.widget.ListView;

public abstract class SwipableListItemView extends FrameLayout implements RRTouchable {

	private LinearLayout child;
	private final ListView listParent;

	private final SwipeHistory swipeHistory = new SwipeHistory(30);
	private float velocity;
	private int xOffset = 0, swipeStartXOffset = 0;

	protected SwipableListItemView(final Context context, final ListView listParent) {
		super(context);
		this.listParent = listParent;
	}

	protected void setSwipableView(final LinearLayout child) {
		if(this.child != null) throw new RuntimeException("Attempt to set child twice");
		this.child = child;
	}

	protected void setVisibleView(final View view) {
		addView(view);
	}

	protected abstract void onSwipeBegin(int xOffsetPixels);
	protected abstract void onSwipePositionChange(int xOffsetPixels);

	// Called when the view is reused
	protected void reset() {
		clearAnimation();
		swipeHistory.clear();
		velocity = 0;
		xOffset = 0;
		swipeStartXOffset = 0;
		updateOffset();
	}

	protected abstract boolean leftFlingEnabled();
	protected abstract boolean rightFlingEnabled();

	private void updateOffset() {
		int xOffset = getXOffset();

		if(!leftFlingEnabled() && xOffset < 0 || !rightFlingEnabled() && xOffset > 0) {
			xOffset = 0;
			this.swipeStartXOffset = -this.xOffset;
			clearAnimation();
		}

		child.setTranslationX(xOffset);
		onSwipePositionChange(xOffset);
	}

	private int getXOffset() {
		return xOffset + swipeStartXOffset;
	}

	public final void rrOnFingerDown() {
		clearAnimation();
		swipeHistory.clear();
		velocity = 0;
		swipeStartXOffset = xOffset;
		xOffset = 0;
	}

	private boolean fingerSwiping = false;

	public final void rrOnSwipeDelta(final float dx) {

		swipeHistory.add(dx, System.currentTimeMillis());

		xOffset = Math.round(dx);

		if(!fingerSwiping) {
			fingerSwiping = true;
			onSwipeBegin(getXOffset());
		}

		updateOffset();
		invalidate();
		listParent.invalidate();
	}

	public final void rrOnFingerUp() {

		fingerSwiping = false;

		if(swipeHistory.size() >= 2) {
			velocity = (swipeHistory.getMostRecent() - swipeHistory.get(100)) * 10;
		} else {
			velocity = 0;
		}

		xOffset += swipeStartXOffset;
		swipeStartXOffset = 0;

		final DHM dhm = new DHM(xOffset, velocity, 30, 1.0f / 60.0f); // TODO account for screen dpi!
		final DHMAnimation anim = new DHMAnimation(dhm);
		anim.setDuration((long)(anim.getSteps() / 60.0f * 1000.0f));
		startAnimation(anim);
		invalidate();
		listParent.invalidate();
	}

	public boolean rrAllowLongClick() {
		return true;
	}

	@Override
	protected final void onAnimationEnd() {
		super.onAnimationEnd();
		xOffset = 0;
		swipeStartXOffset = 0;
		updateOffset();
		invalidate();
		listParent.invalidateViews();
	}

	private class DHMAnimation extends Animation {

		private final DHM dhm;
		private final float steps;

		public DHMAnimation(final DHM dhm) {
			setInterpolator(new LinearInterpolator());
			this.dhm = dhm;
			steps = dhm.calculate(0.49f, 15);
		}

		public float getSteps() {
			return steps;
		}

		@Override
		protected void applyTransformation(final float interpolatedTime, final Transformation t) {
			xOffset = Math.round(dhm.getPositionAt((int) (interpolatedTime * steps)));
			updateOffset();
			invalidate();
			listParent.invalidate();
		}
	}

	private static final class SwipeHistory {

		private final float[] positions;
		private final long[] timestamps;
		private int start = 0, len = 0;

		public SwipeHistory(int len) {
			positions = new float[len];
			timestamps = new long[len];
		}

		public void add(float position, long timestamp) {

			if(len >= positions.length) {
				positions[start] = position;
				timestamps[start] = timestamp;
				start = (start + 1) % positions.length;

			} else {
				positions[(start + len) % positions.length] = position;
				timestamps[(start + len) % timestamps.length] = timestamp;
				len++;
			}
		}

		public float getMostRecent() {
			return positions[getNthMostRecentIndex(0)];
		}

		public float get(long timeAgo) {

			final long timestamp = timestamps[getNthMostRecentIndex(0)] - timeAgo;
			float result = getMostRecent();

			for(int i = 0; i < len; i++) {

				final int index = getNthMostRecentIndex(i);

				if(timestamp > timestamps[index]) {
					return result;
				} else {
					result = positions[index];
				}
			}

			return result;
		}

		private int getNthMostRecentIndex(int n) {
			if(n >= len || n < 0) throw new ArrayIndexOutOfBoundsException(n);
			return (start + len - n - 1) % positions.length;
		}

		public void clear() {
			len = 0;
			start = 0;
		}

		public int size() {
			return len;
		}
	}
}
