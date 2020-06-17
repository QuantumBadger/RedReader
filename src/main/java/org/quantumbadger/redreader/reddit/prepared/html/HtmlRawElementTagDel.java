package org.quantumbadger.redreader.reddit.prepared.html;

import androidx.annotation.NonNull;

import java.util.ArrayList;

public class HtmlRawElementTagDel extends HtmlRawElementTagAttributeChange {

	public HtmlRawElementTagDel(final ArrayList<HtmlRawElement> children) {
		super(children);
	}

	@Override
    protected void onStart(@NonNull HtmlTextAttributes activeAttributes) {
		activeAttributes.strikethrough++;
	}

	@Override
	protected void onEnd(@NonNull HtmlTextAttributes activeAttributes) {
		activeAttributes.strikethrough--;
	}
}
