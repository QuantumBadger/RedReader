package org.quantumbadger.redreader.reddit.prepared.html;

import androidx.annotation.NonNull;

import java.util.ArrayList;

public class HtmlRawElementTagCode extends HtmlRawElementTagAttributeChange {

	public HtmlRawElementTagCode(final ArrayList<HtmlRawElement> children) {
		super(children);
	}

	@Override
    protected void onStart(@NonNull HtmlTextAttributes activeAttributes) {
		activeAttributes.monospace++;
	}

	@Override
	protected void onEnd(@NonNull HtmlTextAttributes activeAttributes) {
		activeAttributes.monospace--;
	}
}
