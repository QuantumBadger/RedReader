package org.quantumbadger.redreader.reddit.prepared.html;

import android.graphics.Color;
import android.support.annotation.NonNull;
import android.support.v7.app.AppCompatActivity;
import android.text.SpannableStringBuilder;
import android.text.Spanned;
import android.text.style.BackgroundColorSpan;
import android.text.style.ForegroundColorSpan;

import java.util.ArrayList;

public class HtmlRawElementInlineErrorMessage extends HtmlRawElementText {

	@NonNull private final String mText;

	public HtmlRawElementInlineErrorMessage(@NonNull final String text) {
		mText = text;
	}

	@Override
	public final void writeTo(
			@NonNull final SpannableStringBuilder ssb,
			@NonNull final HtmlTextAttributes attributes,
			@NonNull final AppCompatActivity activity) {

		final int textStart = ssb.length();
		ssb.append("  ");
		ssb.append(mText);
		ssb.append("  ");
		final int textEnd = ssb.length();

		ssb.setSpan(new BackgroundColorSpan(Color.RED), textStart, textEnd, Spanned.SPAN_INCLUSIVE_EXCLUSIVE);
		ssb.setSpan(new ForegroundColorSpan(Color.WHITE), textStart, textEnd, Spanned.SPAN_INCLUSIVE_EXCLUSIVE);
	}

	@NonNull
	public static HtmlRawElementTagPassthrough appendError(
			@NonNull final String text,
			@NonNull final HtmlRawElement element) {

		final ArrayList<HtmlRawElement> children = new ArrayList<>();

		children.add(element);
		children.add(new HtmlRawElementInlineErrorMessage(text));

		return new HtmlRawElementTagPassthrough(children);
	}
}
