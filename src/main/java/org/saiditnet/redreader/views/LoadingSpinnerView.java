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

package org.saiditnet.redreader.views;

import android.content.Context;
import android.content.res.TypedArray;
import android.graphics.Color;
import android.widget.RelativeLayout;
import com.github.lzyzsd.circleprogress.DonutProgress;
import org.saiditnet.redreader.R;
import org.saiditnet.redreader.common.General;

public class LoadingSpinnerView extends RelativeLayout {

	final DonutProgress mProgressView;

	public LoadingSpinnerView(final Context context) {

		super(context);

		final TypedArray typedArray = context.obtainStyledAttributes(new int[]{
				R.attr.rrLoadingRingForegroundCol,
				R.attr.rrLoadingRingBackgroundCol
		});

		final int foreground = typedArray.getColor(0, Color.MAGENTA);
		final int background = typedArray.getColor(1, Color.GREEN);

		typedArray.recycle();

		mProgressView = new DonutProgress(context);
		mProgressView.setIndeterminate(true);
		mProgressView.setFinishedStrokeColor(foreground);
		mProgressView.setUnfinishedStrokeColor(background);
		final int progressStrokeWidthPx = General.dpToPixels(context, 10);
		mProgressView.setUnfinishedStrokeWidth(progressStrokeWidthPx);
		mProgressView.setFinishedStrokeWidth(progressStrokeWidthPx);
		mProgressView.setStartingDegree(-90);
		mProgressView.initPainters();

		addView(mProgressView);
		final int progressDimensionsPx = General.dpToPixels(context, 100);
		mProgressView.getLayoutParams().width = progressDimensionsPx;
		mProgressView.getLayoutParams().height = progressDimensionsPx;
		((LayoutParams)mProgressView.getLayoutParams()).addRule(CENTER_IN_PARENT);
	}
}
