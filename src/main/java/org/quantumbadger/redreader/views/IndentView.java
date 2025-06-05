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
import org.quantumbadger.redreader.common.RRThemeAttributes;

/**
 * Draws the left margin for comments based on the RedditPreparedComment#indentation number
 */
class IndentView extends View {

	private final Paint mPaint = new Paint();
	private int mIndent;

	private final int mPixelsPerIndent;
	private final int mHalfALine;

	private final boolean mPrefDrawLines;

	private final int[] mIndentColors; // Array to hold indentation colors
	private final int mOriginalIndentLineCol; // Original single line color

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

		{
			final TypedArray attr = context.obtainStyledAttributes(new int[] {
					R.attr.rrIndentBackgroundCol,
					R.attr.rrIndentLineCol
			});

			rrIndentBackgroundCol = attr.getColor(0, General.COLOR_INVALID);
			mOriginalIndentLineCol = attr.getColor(1, General.COLOR_INVALID);
			attr.recycle();
		}

		// Load indentation colors from RRThemeAttributes
		final RRThemeAttributes themeAttributes = new RRThemeAttributes(context);
		mIndentColors = new int[5];
		mIndentColors[0] = themeAttributes.rrIndentLineCol1;
		mIndentColors[1] = themeAttributes.rrIndentLineCol2;
		mIndentColors[2] = themeAttributes.rrIndentLineCol3;
		mIndentColors[3] = themeAttributes.rrIndentLineCol4;
		mIndentColors[4] = themeAttributes.rrIndentLineCol5;

		this.setBackgroundColor(rrIndentBackgroundCol);
		mPaint.setStrokeWidth(mPixelsPerLine);

		mPrefDrawLines = PrefsUtility.pref_appearance_indentlines();
	}

	@Override
	protected void onDraw(final Canvas canvas) {

		super.onDraw(canvas);

		final int height = getMeasuredHeight();

		if(mPrefDrawLines) {
			// Draw each line individually with its own color
			for(int i = 0; i < mIndent; i++) {
				final float x = (mPixelsPerIndent * (i + 1)) - mHalfALine;
				// Set color based on indentation level, cycling through the color array
				mPaint.setColor(mIndentColors[i % mIndentColors.length]);
				// Draw each line individually
				canvas.drawLine(x, 0, x, height, mPaint);
			}

		} else {
			// Use the original theme color when colorful bars are disabled
			mPaint.setColor(mOriginalIndentLineCol);
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

		invalidate();
		requestLayout();
	}
}
