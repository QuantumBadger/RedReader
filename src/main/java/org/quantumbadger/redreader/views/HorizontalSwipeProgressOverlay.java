package com.konneh.scroll.views;

import android.content.Context;
import android.graphics.Color;
import android.view.View;
import android.widget.ImageView;
import android.widget.RelativeLayout;
import com.github.lzyzsd.circleprogress.DonutProgress;
import com.konneh.scroll.R;
import com.konneh.scroll.common.General;

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
