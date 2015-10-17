/*******************************************************************************
 * This file is part of Scroll.
 *
 * Scroll is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Scroll is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Scroll.  If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/

package com.konneh.scroll.views;

import android.content.Context;
import android.graphics.Canvas;
import android.graphics.Color;
import android.graphics.Paint;
import android.preference.PreferenceManager;
import android.view.View;
import com.konneh.scroll.common.General;
import com.konneh.scroll.common.PrefsUtility;

/**
 * Draws the left margin for comments based on the
 * RedditPreparedComment#indentation number
 *
 * @author Gabriel Castro &lt;dev@GabrielCastro.ca&gt;
 */
class IndentView extends View {

	private final Paint mPaint = new Paint();
	private int mIndent;

	private final int mPixelsPerIndent;
	private final int mPixelsPerLine;
	private final float mHalfALine;

	private final boolean mPrefDrawLines;

	public IndentView(Context context) {
		super(context);

		mPixelsPerIndent = General.dpToPixels(context, 10.0f);
		mPixelsPerLine = General.dpToPixels(context, 2);
		mHalfALine = mPixelsPerLine / 2;

		this.setBackgroundColor(Color.argb(20, 128, 128, 128));
		mPaint.setColor(Color.argb(75, 128, 128, 128));
		mPaint.setStrokeWidth(mPixelsPerLine);

		mPrefDrawLines = PrefsUtility.pref_appearance_indentlines(context, PreferenceManager.getDefaultSharedPreferences(context));
	}

	@Override
	protected void onDraw(final Canvas canvas) {

		super.onDraw(canvas);

		final int height = getMeasuredHeight();

		if(mPrefDrawLines) {
			final float[] lines = new float[mIndent * 4];
			float x;
			// i keeps track of indentation, and
			// l is to populate the float[] with line co-ordinates
			for (int i = 0, l = 0; i < mIndent; ++l) {
				x = (mPixelsPerIndent * ++i) - mHalfALine;
				lines[l]   = x;      // start-x
				lines[++l] = 0;      // start-y
				lines[++l] = x;      // stop-x
				lines[++l] = height; // stop-y
			}
			canvas.drawLines(lines, mPaint);

		} else {
			final float rightLine = getWidth() - mHalfALine;
			canvas.drawLine(rightLine, 0, rightLine, getHeight(), mPaint);
		}
	}

	/**
	 * Sets the indentation for the View
	 * @param indent comment indentation number
	 */
	public void setIndentation(int indent) {
		this.getLayoutParams().width = (mPixelsPerIndent * indent);
		this.mIndent = indent;
		this.invalidate();
		this.requestLayout();
	}
}
