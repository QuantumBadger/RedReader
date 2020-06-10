package org.quantumbadger.redreader.reddit.prepared.html;

import android.support.annotation.NonNull;
import android.support.v7.app.AppCompatActivity;
import android.text.SpannableStringBuilder;

public abstract class HtmlRawElementText extends HtmlRawElement {

	public abstract void writeTo(
			@NonNull final SpannableStringBuilder ssb,
			@NonNull final HtmlTextAttributes attributes,
			@NonNull final AppCompatActivity activity);
}
