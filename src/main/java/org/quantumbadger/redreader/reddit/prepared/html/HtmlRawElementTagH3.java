package org.quantumbadger.redreader.reddit.prepared.html;

import android.support.annotation.NonNull;

import java.util.ArrayList;

public class HtmlRawElementTagH3 extends HtmlRawElementTagAttributeChange {

	public HtmlRawElementTagH3(final ArrayList<HtmlRawElement> children) {
		super(children);
	}

	protected void onStart(@NonNull HtmlTextAttributes activeAttributes) {
		activeAttributes.bold++;
		activeAttributes.large++;
	}

	protected void onEnd(@NonNull HtmlTextAttributes activeAttributes) {
		activeAttributes.bold--;
		activeAttributes.large--;
	}
}
