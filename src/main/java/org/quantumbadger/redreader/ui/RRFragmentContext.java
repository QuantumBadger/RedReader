package org.quantumbadger.redreader.ui;

import org.holoeverywhere.app.Activity;
import org.quantumbadger.redreader.ui.frag.RRFragment;
import org.quantumbadger.redreader.ui.frag.RRFragmentLayout;

public final class RRFragmentContext extends RRContext {

	public final RRFragment fragment;

	public RRFragmentContext(Activity activity, RRFragmentLayout fragmentLayout, RRFragment fragment) {
		super(activity, fragmentLayout);
		this.fragment = fragment;
	}
}
