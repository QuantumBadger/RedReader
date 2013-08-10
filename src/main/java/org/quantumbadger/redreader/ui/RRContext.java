package org.quantumbadger.redreader.ui;

import org.holoeverywhere.app.Activity;
import org.quantumbadger.redreader.ui.frag.RRFragment;
import org.quantumbadger.redreader.ui.frag.RRFragmentLayout;

public class RRContext {

	public final Activity activity;
	public final RRFragmentLayout fragmentLayout;

	public RRContext(Activity activity, RRFragmentLayout fragmentLayout) {
		this.activity = activity;
		this.fragmentLayout = fragmentLayout;
	}

	public RRFragmentContext forFragment(RRFragment fragment) {
		return new RRFragmentContext(activity, fragmentLayout, fragment);
	}
}
