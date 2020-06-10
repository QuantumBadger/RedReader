package org.quantumbadger.redreader.reddit.prepared.html;

import android.support.annotation.NonNull;

import java.util.ArrayList;

public abstract class HtmlRawElement {
	// div, a, p, table/td/tr, hr, strong, em, "code", "headings", "underline", "strikethrough", ul/li etc

	@NonNull
	public abstract ArrayList<HtmlReducedElement> reduce(
			@NonNull HtmlTextAttributes activeAttributes);
}
