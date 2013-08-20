package org.quantumbadger.redreader.ui.frag;

import org.holoeverywhere.app.Activity;
import org.quantumbadger.redreader.common.RRSchedulerManager;

public class RRContext {

	public final Activity activity;
	public final RRFragmentLayout fragmentLayout;
	public final RRSchedulerManager scheduler;

	public RRContext(Activity activity, RRFragmentLayout fragmentLayout, RRSchedulerManager scheduler) {
		this.activity = activity;
		this.fragmentLayout = fragmentLayout;
		this.scheduler = scheduler;
	}

	public RRFragmentContext forFragment(RRFragment fragment) {
		return new RRFragmentContext(activity, fragmentLayout, scheduler, fragment);
	}

	void onResume() {
		scheduler.onResume();
	}

	void onPause() {
		scheduler.onPause();
	}
}
