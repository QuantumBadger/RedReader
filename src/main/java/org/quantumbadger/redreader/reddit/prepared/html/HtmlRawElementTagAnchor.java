package org.quantumbadger.redreader.reddit.prepared.html;

import android.support.annotation.NonNull;

import java.util.ArrayList;

public class HtmlRawElementTagAnchor extends HtmlRawElementTagAttributeChange {

	@NonNull private final String mHref;

	public HtmlRawElementTagAnchor(
			final ArrayList<HtmlRawElement> children,
			@NonNull final String href) {
		super(children);
		mHref = href;
	}

	protected void onStart(@NonNull HtmlTextAttributes activeAttributes) {
		activeAttributes.href = mHref;
	}

	protected void onEnd(@NonNull HtmlTextAttributes activeAttributes) {
		activeAttributes.href = null;
	}
}
