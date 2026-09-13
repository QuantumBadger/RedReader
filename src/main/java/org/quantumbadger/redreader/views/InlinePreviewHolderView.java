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
import android.graphics.Rect;
import android.util.AttributeSet;
import android.view.ViewParent;
import android.widget.FrameLayout;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.recyclerview.widget.RecyclerView;

import org.quantumbadger.redreader.reddit.prepared.InlinePreviewLoader;

/**
 * Displays an inline image preview at the aspect ratio of the image itself, except where
 * that would make it taller than {@link InlinePreviewLoader#getMaxPreviewHeightPx(int)},
 * in which case it is limited to that height and the image is letterboxed within it.
 *
 * <p>The height is calculated during measurement, rather than when the preview is attached,
 * so that it is always based on the space this view has actually been given -- both the
 * width it is being measured against, and the height of the list it is in. Neither can be
 * known in advance: the list may take up only part of the screen, as it does in the two
 * pane tablet layout, and the screen can be rotated without the posts on it being rebound.
 */
public final class InlinePreviewHolderView extends FrameLayout {

	private final Rect mWindowVisibleDisplayFrame = new Rect();

	private int mImageWidthPx;
	private int mImageHeightPx;

	public InlinePreviewHolderView(@NonNull final Context context) {
		super(context);
	}

	public InlinePreviewHolderView(
			@NonNull final Context context,
			@Nullable final AttributeSet attrs) {
		super(context, attrs);
	}

	public InlinePreviewHolderView(
			@NonNull final Context context,
			@Nullable final AttributeSet attrs,
			final int defStyleAttr) {
		super(context, attrs, defStyleAttr);
	}

	/**
	 * Sets the size of the image to be displayed. Only the ratio between the two values is
	 * used -- the image is displayed at the width of this view, whatever that turns out to
	 * be. Pass zero for both when there is no image to display.
	 */
	public void setImageSize(final int widthPx, final int heightPx) {

		if(widthPx == mImageWidthPx && heightPx == mImageHeightPx) {
			return;
		}

		mImageWidthPx = widthPx;
		mImageHeightPx = heightPx;

		requestLayout();
	}

	/**
	 * The height of the list this view is in, which is the space actually available to
	 * display a post. Falls back to the height of the window if the list hasn't been laid
	 * out yet, or if this view isn't in one.
	 */
	private int getDisplayAreaHeightPx() {

		ViewParent parent = getParent();

		while(parent != null) {

			if(parent instanceof RecyclerView) {

				final int height = ((RecyclerView)parent).getHeight();

				if(height > 0) {
					return height;
				}

				break;
			}

			parent = parent.getParent();
		}

		getWindowVisibleDisplayFrame(mWindowVisibleDisplayFrame);

		return mWindowVisibleDisplayFrame.height();
	}

	@Override
	protected void onMeasure(final int widthMeasureSpec, final int heightMeasureSpec) {

		if(mImageWidthPx < 1 || mImageHeightPx < 1) {
			super.onMeasure(widthMeasureSpec, heightMeasureSpec);
			return;
		}

		final int width = MeasureSpec.getSize(widthMeasureSpec);

		final int height = Math.max(1, Math.min(
				InlinePreviewLoader.getMaxPreviewHeightPx(getDisplayAreaHeightPx()),
				(int)(((long)width * mImageHeightPx) / mImageWidthPx)));

		super.onMeasure(
				MeasureSpec.makeMeasureSpec(width, MeasureSpec.EXACTLY),
				MeasureSpec.makeMeasureSpec(height, MeasureSpec.EXACTLY));
	}
}
