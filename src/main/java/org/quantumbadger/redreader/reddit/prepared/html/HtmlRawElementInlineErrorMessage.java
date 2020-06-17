package org.quantumbadger.redreader.reddit.prepared.html;

import android.graphics.Color;
import androidx.annotation.NonNull;
import android.text.style.BackgroundColorSpan;
import android.text.style.CharacterStyle;
import android.text.style.ForegroundColorSpan;

import java.util.ArrayList;

public abstract class HtmlRawElementInlineErrorMessage extends HtmlRawElement {

	private HtmlRawElementInlineErrorMessage() {}

	public static HtmlRawElementStyledText create(@NonNull final String text) {

		final ArrayList<CharacterStyle> spans = new ArrayList<>();
		spans.add(new BackgroundColorSpan(Color.RED));
		spans.add(new ForegroundColorSpan(Color.WHITE));

		return new HtmlRawElementStyledText(text, spans);
	}

	@NonNull
	public static HtmlRawElementTagPassthrough appendError(
			@NonNull final String text,
			@NonNull final HtmlRawElement element) {

		final ArrayList<HtmlRawElement> children = new ArrayList<>();

		children.add(element);
		children.add(HtmlRawElementInlineErrorMessage.create(text));

		return new HtmlRawElementTagPassthrough(children);
	}
}
