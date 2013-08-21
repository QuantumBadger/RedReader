package org.quantumbadger.redreader.ui.frag;

import android.util.DisplayMetrics;
import org.holoeverywhere.app.Activity;
import org.quantumbadger.redreader.common.RRSchedulerManager;

public class RRContext {

	public final Activity activity;
	public final RRFragmentLayout fragmentLayout;
	public final RRSchedulerManager scheduler;

	// TODO spScale may change at runtime?
	public final float dpScale, spScale;

	RRContext(RRContext context) {
		this(context.activity, context.fragmentLayout, context.scheduler);
	}

	RRContext(Activity activity, RRFragmentLayout fragmentLayout, RRSchedulerManager scheduler) {

		this.activity = activity;
		this.fragmentLayout = fragmentLayout;
		this.scheduler = scheduler;

		final DisplayMetrics displayMetrics = activity.getResources().getDisplayMetrics();
		dpScale = displayMetrics.density;
		spScale = displayMetrics.scaledDensity;
	}

	public RRFragmentContext forFragment(RRFragment fragment) {
		return new RRFragmentContext(this, fragment);
	}

	final void onResume() {
		scheduler.onResume();
	}

	final void onPause() {
		scheduler.onPause();
	}
}
