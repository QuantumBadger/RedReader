package org.quantumbadger.redreader.reddit.prepared.html;

import androidx.annotation.NonNull;

import java.util.ArrayList;

public class HtmlRawElementTagPassthrough extends HtmlRawElementTagAttributeChange {

	public HtmlRawElementTagPassthrough(final ArrayList<HtmlRawElement> children) {
		super(children);
	}

	@Override
	protected void onStart(@NonNull final HtmlTextAttributes activeAttributes) {
		// Do nothing
	}

	@Override
	protected void onEnd(@NonNull final HtmlTextAttributes activeAttributes) {
		// Do nothing
	}
}
