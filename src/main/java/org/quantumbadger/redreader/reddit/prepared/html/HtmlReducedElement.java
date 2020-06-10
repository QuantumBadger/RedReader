package org.quantumbadger.redreader.reddit.prepared.html;

import android.content.Context;
import android.support.annotation.NonNull;
import android.view.View;

public abstract class HtmlReducedElement {

	@NonNull
	public abstract View generateViews(@NonNull Context context);
}
