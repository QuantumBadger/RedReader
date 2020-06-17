package org.quantumbadger.redreader.reddit.prepared.html;

import androidx.annotation.NonNull;

import java.util.ArrayList;

public class HtmlRawElementTagH4 extends HtmlRawElementTagAttributeChange {

	public HtmlRawElementTagH4(final ArrayList<HtmlRawElement> children) {
		super(children);
	}

	@Override
    protected void onStart(@NonNull HtmlTextAttributes activeAttributes) {
		activeAttributes.large++;
	}

	@Override
	protected void onEnd(@NonNull HtmlTextAttributes activeAttributes) {
		activeAttributes.large--;
	}
}
