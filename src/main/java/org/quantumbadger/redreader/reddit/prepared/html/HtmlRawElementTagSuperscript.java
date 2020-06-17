package org.quantumbadger.redreader.reddit.prepared.html;

import androidx.annotation.NonNull;

import java.util.ArrayList;

public class HtmlRawElementTagSuperscript extends HtmlRawElementTagAttributeChange {

	public HtmlRawElementTagSuperscript(final ArrayList<HtmlRawElement> children) {
		super(children);
	}

	@Override
    protected void onStart(@NonNull HtmlTextAttributes activeAttributes) {
		activeAttributes.superscript++;
	}

	@Override
	protected void onEnd(@NonNull HtmlTextAttributes activeAttributes) {
		activeAttributes.superscript--;
	}
}
