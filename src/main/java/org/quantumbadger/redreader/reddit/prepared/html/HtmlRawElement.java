package org.quantumbadger.redreader.reddit.prepared.html;

import android.support.annotation.NonNull;

import java.util.ArrayList;

public abstract class HtmlRawElement {

	@NonNull
	public abstract ArrayList<HtmlReducedElement> reduce(
			@NonNull HtmlTextAttributes activeAttributes);
}
