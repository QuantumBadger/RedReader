package org.quantumbadger.redreader.reddit.prepared.html;

import android.graphics.Typeface;
import android.support.annotation.NonNull;
import android.support.v7.app.AppCompatActivity;
import android.text.SpannableStringBuilder;
import android.text.Spanned;
import android.text.style.ClickableSpan;
import android.text.style.RelativeSizeSpan;
import android.text.style.StrikethroughSpan;
import android.text.style.StyleSpan;
import android.text.style.SuperscriptSpan;
import android.text.style.TypefaceSpan;
import android.view.View;
import org.quantumbadger.redreader.common.LinkHandler;

public class HtmlRawElementPlainText extends HtmlRawElementText {

	@NonNull private final String mText;

	public HtmlRawElementPlainText(@NonNull final String text) {
		mText = text;
	}

	@Override
	public final void writeTo(
			@NonNull final SpannableStringBuilder ssb,
			@NonNull final HtmlTextAttributes attributes,
			@NonNull final AppCompatActivity activity) {

		final int textStart = ssb.length();
		ssb.append(mText);
		final int textEnd = ssb.length();

		if(attributes.bold > 0) {
			ssb.setSpan(
					new StyleSpan(Typeface.BOLD),
					textStart,
					textEnd,
					Spanned.SPAN_INCLUSIVE_EXCLUSIVE);
		}

		if(attributes.italic > 0) {
			ssb.setSpan(
					new StyleSpan(Typeface.ITALIC),
					textStart,
					textEnd,
					Spanned.SPAN_INCLUSIVE_EXCLUSIVE);
		}

		if(attributes.strikethrough > 0) {
			ssb.setSpan(
					new StrikethroughSpan(),
					textStart,
					textEnd,
					Spanned.SPAN_INCLUSIVE_EXCLUSIVE);
		}

		if(attributes.monospace > 0) {
			ssb.setSpan(
					new TypefaceSpan("monospace"),
					textStart,
					textEnd,
					Spanned.SPAN_INCLUSIVE_EXCLUSIVE);
		}

		if(attributes.superscript > 0) {
			ssb.setSpan(
					new SuperscriptSpan(),
					textStart,
					textEnd,
					Spanned.SPAN_INCLUSIVE_EXCLUSIVE);
		}

		if(attributes.extraLarge > 0) {
			ssb.setSpan(
					new RelativeSizeSpan(1.6f),
					textStart,
					textEnd,
					Spanned.SPAN_INCLUSIVE_EXCLUSIVE);

		} else if(attributes.large > 0) {
			ssb.setSpan(
					new RelativeSizeSpan(1.3f),
					textStart,
					textEnd,
					Spanned.SPAN_INCLUSIVE_EXCLUSIVE);
		}

		if(attributes.href != null) {

			final String url = attributes.href;

			ssb.setSpan(
					new ClickableSpan() {
						@Override
						public void onClick(@NonNull final View widget) {
							LinkHandler.onLinkClicked(activity, url);
						}
					},
					textStart,
					textEnd,
					Spanned.SPAN_INCLUSIVE_EXCLUSIVE);
		}
	}
}
