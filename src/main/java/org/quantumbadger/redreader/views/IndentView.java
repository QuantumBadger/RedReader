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
import android.content.res.TypedArray;
import android.graphics.Canvas;
import android.graphics.Paint;
import android.util.AttributeSet;
import android.view.View;
import androidx.annotation.Nullable;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.PrefsUtility;

/**
 * Draws the left margin for comments based on the RedditPreparedComment#indentation number
 */
class IndentView extends View {

	private final Paint mPaint = new Paint();
	private int mIndent;

	private final int mPixelsPerIndent;
	private final int mHalfALine;

	private final boolean mPrefDrawLines;

	private float[] mLineBuffer;

	public IndentView(final Context context) {
		this(context, null);
	}

	public IndentView(final Context context, @Nullable final AttributeSet attrs) {
		this(context, attrs, 0);
	}

	public IndentView(
			final Context context,
			@Nullable final AttributeSet attrs,
			final int defStyleAttr) {

		super(context, attrs, defStyleAttr);

		mPixelsPerIndent = General.dpToPixels(context, 10.0f);
		final int mPixelsPerLine = General.dpToPixels(context, 2);
		mHalfALine = mPixelsPerLine / 2;

		final int rrIndentBackgroundCol;
		final int rrIndentLineCol;

		{
			final TypedArray attr = context.obtainStyledAttributes(new int[] {
					R.attr.rrIndentBackgroundCol,
					R.attr.rrIndentLineCol
			});

			rrIndentBackgroundCol = attr.getColor(0, General.COLOR_INVALID);
			rrIndentLineCol = attr.getColor(1, General.COLOR_INVALID);

			attr.recycle();
		}

		this.setBackgroundColor(rrIndentBackgroundCol);
		mPaint.setColor(rrIndentLineCol);
		mPaint.setStrokeWidth(mPixelsPerLine);

		mPrefDrawLines = PrefsUtility.pref_appearance_indentlines();
	}

	@Override
	protected void onDraw(final Canvas canvas) {

		super.onDraw(canvas);

		final int height = getMeasuredHeight();

		if(mPrefDrawLines) {
			// i keeps track of indentation, and
			// l is to populate the float[] with line co-ordinates
			int l = 0;
			int i = 0;
			while(i < mIndent) {
				final float x = (mPixelsPerIndent * ++i) - mHalfALine;
				mLineBuffer[l++] = x;      // start-x
				mLineBuffer[l++] = 0;      // start-y
				mLineBuffer[l++] = x;      // stop-x
				mLineBuffer[l++] = height; // stop-y
			}
			canvas.drawLines(mLineBuffer, mPaint);

		} else {
			final float rightLine = getWidth() - mHalfALine;
			canvas.drawLine(rightLine, 0, rightLine, getHeight(), mPaint);
		}
	}

	/**
	 * Sets the indentation for the View
	 *
	 * @param indent comment indentation number
	 */
	public void setIndentation(final int indent) {
		getLayoutParams().width = (mPixelsPerIndent * indent);
		mIndent = indent;

		if(mPrefDrawLines) {
			mLineBuffer = new float[mIndent * 4];
		}

		invalidate();
		requestLayout();
	}
}
