package org.quantumbadger.redreader.ui;

import org.holoeverywhere.app.Activity;
import org.quantumbadger.redreader.ui.frag.RRFragment;
import org.quantumbadger.redreader.ui.frag.RRFragmentLayout;

public final class RRContext {

	public final Activity activity;
	public final RRFragmentLayout fragmentLayout;
	public final RRFragment fragment;

	public RRContext(Activity activity, RRFragmentLayout fragmentLayout, RRFragment fragment) {
		this.activity = activity;
		this.fragmentLayout = fragmentLayout;
		this.fragment = fragment;
	}

	public RRContext(Activity activity, RRFragmentLayout fragmentLayout) {
		this(activity, fragmentLayout, null);
	}

	public RRContext forFragment(RRFragment fragment) {
		return new RRContext(activity, fragmentLayout, fragment);
	}
}
