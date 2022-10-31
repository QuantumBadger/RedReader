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
import android.graphics.Color;
import android.view.View;
import android.widget.ImageView;
import android.widget.RelativeLayout;
import com.github.lzyzsd.circleprogress.DonutProgress;
import org.saiditnet.redreader.R;
import org.saiditnet.redreader.common.General;

public class HorizontalSwipeProgressOverlay extends RelativeLayout {

	private final ImageView mIcon;
	private final DonutProgress mProgress;
	private int mCurrentIconResource = 0;

	public HorizontalSwipeProgressOverlay(final Context context) {
		super(context);

		final View background = new View(context);
		final int backgroundDimensionsPx = General.dpToPixels(context, 200);
		background.setBackgroundColor(Color.argb(127, 0, 0, 0));
		addView(background);
		background.getLayoutParams().width = backgroundDimensionsPx;
		background.getLayoutParams().height = backgroundDimensionsPx;
		((LayoutParams)background.getLayoutParams()).addRule(RelativeLayout.CENTER_IN_PARENT);

		mIcon = new ImageView(context);
		mIcon.setImageResource(R.drawable.ic_action_forward_dark);
		mCurrentIconResource = R.drawable.ic_action_forward_dark;
		addView(mIcon);
		((LayoutParams)mIcon.getLayoutParams()).addRule(RelativeLayout.CENTER_IN_PARENT);

		mProgress = new DonutProgress(context);

		addView(mProgress);
		((LayoutParams)mProgress.getLayoutParams()).addRule(RelativeLayout.CENTER_IN_PARENT);
		final int progressDimensionsPx = General.dpToPixels(context, 150);
		mProgress.getLayoutParams().width = progressDimensionsPx;
		mProgress.getLayoutParams().height = progressDimensionsPx;

		mProgress.setFinishedStrokeColor(Color.RED);
		mProgress.setUnfinishedStrokeColor(Color.argb(127, 0, 0, 0));
		final int progressStrokeWidthPx = General.dpToPixels(context, 15);
		mProgress.setUnfinishedStrokeWidth(progressStrokeWidthPx);
		mProgress.setFinishedStrokeWidth(progressStrokeWidthPx);
		mProgress.setStartingDegree(-90);
		mProgress.initPainters();

		setVisibility(GONE);
	}

	private void setIconResource(final int resource) {
		if(resource != mCurrentIconResource) {
			mCurrentIconResource = resource;
			mIcon.setImageResource(resource);
		}
	}

	public void onSwipeUpdate(float px, float maxPx) {

		mProgress.setProgress(-(px / maxPx));

		if(Math.abs(px) > 20) {
			setVisibility(VISIBLE);
		}

		if(px < 0) {
			setIconResource(R.drawable.ic_action_forward_dark);
		} else {
			setIconResource(R.drawable.ic_action_back_dark);
		}
	}

	public void onSwipeEnd() {
		setVisibility(GONE);
	}
}
