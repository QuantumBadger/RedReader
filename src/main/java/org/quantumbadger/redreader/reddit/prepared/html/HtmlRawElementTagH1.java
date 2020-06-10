package org.quantumbadger.redreader.reddit.prepared.html;

import android.support.annotation.NonNull;

import java.util.ArrayList;

public class HtmlRawElementTagH1 extends HtmlRawElementTagAttributeChange {

	public HtmlRawElementTagH1(final ArrayList<HtmlRawElement> children) {
		super(children);
	}

	protected void onStart(@NonNull HtmlTextAttributes activeAttributes) {
		activeAttributes.bold++;
		activeAttributes.extraLarge++;
	}

	protected void onEnd(@NonNull HtmlTextAttributes activeAttributes) {
		activeAttributes.bold--;
		activeAttributes.extraLarge--;
	}
}
