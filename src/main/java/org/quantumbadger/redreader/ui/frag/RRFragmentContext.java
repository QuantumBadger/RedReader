package org.quantumbadger.redreader.ui.frag;

import org.holoeverywhere.app.Activity;
import org.quantumbadger.redreader.common.RRSchedulerManager;

public final class RRFragmentContext extends RRContext {

	public final RRFragment fragment;

	public RRFragmentContext(Activity activity, RRFragmentLayout fragmentLayout, RRSchedulerManager scheduler,
							 RRFragment fragment) {
		super(activity, fragmentLayout, scheduler);
		this.fragment = fragment;
	}
}
