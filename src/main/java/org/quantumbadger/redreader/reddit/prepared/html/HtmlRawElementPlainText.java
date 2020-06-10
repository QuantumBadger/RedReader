package org.quantumbadger.redreader.reddit.prepared.html;

import android.graphics.Typeface;
import android.support.annotation.NonNull;
import android.support.v7.app.AppCompatActivity;
import android.text.style.CharacterStyle;
import android.text.style.ClickableSpan;
import android.text.style.RelativeSizeSpan;
import android.text.style.StrikethroughSpan;
import android.text.style.StyleSpan;
import android.text.style.SuperscriptSpan;
import android.text.style.TypefaceSpan;
import android.view.View;
import org.quantumbadger.redreader.common.LinkHandler;
import org.quantumbadger.redreader.reddit.prepared.bodytext.BodyTextElement;

import java.util.ArrayList;

public class HtmlRawElementPlainText extends HtmlRawElement {

	@NonNull private final String mText;

	public HtmlRawElementPlainText(@NonNull final String text) {
		mText = text;
	}

	@Override
	public void reduce(
			@NonNull final HtmlTextAttributes attributes,
			@NonNull final AppCompatActivity activity,
			@NonNull final ArrayList<HtmlRawElement> destination) {

		ArrayList<CharacterStyle> spans = null;

		if(attributes.bold > 0) {
			//noinspection ConstantConditions
			if(spans == null) spans = new ArrayList<>();
			spans.add(new StyleSpan(Typeface.BOLD));
		}

		if(attributes.italic > 0) {
			if(spans == null) spans = new ArrayList<>();
			spans.add(new StyleSpan(Typeface.ITALIC));
		}

		if(attributes.strikethrough > 0) {
			if(spans == null) spans = new ArrayList<>();
			spans.add(new StrikethroughSpan());
		}

		if(attributes.monospace > 0) {
			if(spans == null) spans = new ArrayList<>();
			spans.add(new TypefaceSpan("monospace"));
		}

		if(attributes.superscript > 0) {
			if(spans == null) spans = new ArrayList<>();
			spans.add(new SuperscriptSpan());
		}

		if(attributes.extraLarge > 0) {
			if(spans == null) spans = new ArrayList<>();
			spans.add(new RelativeSizeSpan(1.6f));

		} else if(attributes.large > 0) {
			if(spans == null) spans = new ArrayList<>();
			spans.add(new RelativeSizeSpan(1.3f));
		}

		if(attributes.href != null) {

			if(spans == null) spans = new ArrayList<>();

			final String url = attributes.href;

			spans.add(new ClickableSpan() {
				@Override
				public void onClick(@NonNull final View widget) {
					LinkHandler.onLinkClicked(activity, url);
				}
			});
		}

		destination.add(new HtmlRawElementStyledText(mText, spans));
	}

	@Override
	public void generate(
			@NonNull final AppCompatActivity activity,
			@NonNull final ArrayList<BodyTextElement> destination) {

		throw new RuntimeException("Attempt to call generate() on reducible element");
	}
}
