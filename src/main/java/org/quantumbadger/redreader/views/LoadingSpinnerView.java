package org.quantumbadger.redreader.views;

import android.content.Context;
import android.content.res.TypedArray;
import android.graphics.Color;
import android.widget.RelativeLayout;
import com.github.lzyzsd.circleprogress.DonutProgress;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.common.General;

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
