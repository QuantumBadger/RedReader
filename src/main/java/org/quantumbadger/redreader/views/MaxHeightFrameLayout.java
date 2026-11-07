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

package org.quantumbadger.redreader.views;

import android.content.Context;
import android.util.AttributeSet;
import android.widget.FrameLayout;

import androidx.annotation.Nullable;

/**
 * A FrameLayout which can optionally clip its content to a maximum height.
 * The content is always measured at its full size, so that callers can find out
 * whether it would overflow the limit and offer a "show more" control.
 */
public class MaxHeightFrameLayout extends FrameLayout {

	public interface OverflowListener {
		void onOverflowChanged(boolean contentExceedsMaxHeight);
	}

	private int mMaxHeightPx = Integer.MAX_VALUE;
	private boolean mCollapsed = false;
	private boolean mContentExceedsMaxHeight = false;

	@Nullable private OverflowListener mOverflowListener;

	public MaxHeightFrameLayout(final Context context) {
		super(context);
	}

	public MaxHeightFrameLayout(final Context context, @Nullable final AttributeSet attrs) {
		super(context, attrs);
	}

	public MaxHeightFrameLayout(
			final Context context,
			@Nullable final AttributeSet attrs,
			final int defStyleAttr) {
		super(context, attrs, defStyleAttr);
	}

	public void setMaxHeightPx(final int maxHeightPx) {
		if(mMaxHeightPx != maxHeightPx) {
			mMaxHeightPx = maxHeightPx;
			requestLayout();
		}
	}

	public void setCollapsed(final boolean collapsed) {
		if(mCollapsed != collapsed) {
			mCollapsed = collapsed;
			requestLayout();
		}
	}

	public boolean isCollapsed() {
		return mCollapsed;
	}

	public boolean contentExceedsMaxHeight() {
		return mContentExceedsMaxHeight;
	}

	public void setOverflowListener(@Nullable final OverflowListener listener) {
		mOverflowListener = listener;
	}

	@Override
	protected void onMeasure(final int widthMeasureSpec, final int heightMeasureSpec) {

		// Measure the content at its full height, regardless of the limit
		super.onMeasure(
				widthMeasureSpec,
				MeasureSpec.makeMeasureSpec(0, MeasureSpec.UNSPECIFIED));

		final int fullHeight = getMeasuredHeight();
		final boolean exceeds = fullHeight > mMaxHeightPx;

		final int height = (mCollapsed && exceeds) ? mMaxHeightPx : fullHeight;

		setMeasuredDimension(
				getMeasuredWidthAndState(),
				resolveSizeAndState(height, heightMeasureSpec, 0));

		if(exceeds != mContentExceedsMaxHeight) {
			mContentExceedsMaxHeight = exceeds;

			if(mOverflowListener != null) {
				// Listeners may change visibility of other views, which isn't
				// permitted during a measure pass
				final OverflowListener listener = mOverflowListener;
				post(() -> listener.onOverflowChanged(exceeds));
			}
		}
	}
}
