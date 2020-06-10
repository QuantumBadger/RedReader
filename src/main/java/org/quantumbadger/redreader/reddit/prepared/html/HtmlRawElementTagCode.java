package org.quantumbadger.redreader.reddit.prepared.html;

import android.support.annotation.NonNull;

import java.util.ArrayList;

public class HtmlRawElementTagCode extends HtmlRawElementTagAttributeChange {

	public HtmlRawElementTagCode(final ArrayList<HtmlRawElement> children) {
		super(children);
	}

	protected void onStart(@NonNull HtmlTextAttributes activeAttributes) {
		activeAttributes.monospace++;
	}

	protected void onEnd(@NonNull HtmlTextAttributes activeAttributes) {
		activeAttributes.monospace--;
	}
}
