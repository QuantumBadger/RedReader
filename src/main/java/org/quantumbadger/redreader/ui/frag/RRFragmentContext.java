package org.quantumbadger.redreader.ui.frag;

import org.quantumbadger.redreader.common.UnexpectedInternalStateException;

public final class RRFragmentContext extends RRContext {

	public final RRFragment fragment;

	public RRFragmentContext(RRContext context, RRFragment fragment) {
		super(context);
		this.fragment = fragment;
	}

	@Override
	public RRFragmentContext forFragment(RRFragment fragment) {
		throw new UnexpectedInternalStateException();
	}
}
