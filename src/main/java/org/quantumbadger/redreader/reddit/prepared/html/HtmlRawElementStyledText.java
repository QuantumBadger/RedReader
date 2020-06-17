package org.quantumbadger.redreader.reddit.prepared.html;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;
import android.text.SpannableStringBuilder;
import android.text.Spanned;
import android.text.style.CharacterStyle;
import org.quantumbadger.redreader.reddit.prepared.bodytext.BodyElement;

import java.util.ArrayList;

public class HtmlRawElementStyledText extends HtmlRawElement {

	@NonNull private final String mText;
	@Nullable private final ArrayList<CharacterStyle> mSpans;

	public HtmlRawElementStyledText(
			@NonNull final String text,
			@Nullable final ArrayList<CharacterStyle> spans) {
		mText = text;
		mSpans = spans;
	}

	@Override
	public void getPlainText(@NonNull final StringBuilder stringBuilder) {
		stringBuilder.append(mText);
	}

	public final void writeTo(@NonNull final SpannableStringBuilder ssb) {

		final int textStart = ssb.length();
		ssb.append(mText);
		final int textEnd = ssb.length();

		if(mSpans != null) {
			for(final CharacterStyle span : mSpans) {
				ssb.setSpan(span, textStart, textEnd, Spanned.SPAN_INCLUSIVE_EXCLUSIVE);
			}
		}
	}

	@Override
	public void reduce(
			@NonNull final HtmlTextAttributes activeAttributes,
			@NonNull final AppCompatActivity activity,
			@NonNull final ArrayList<HtmlRawElement> destination,
			@NonNull final ArrayList<LinkButtonDetails> linkButtons) {

		destination.add(this);
	}

	@Override
	public void generate(
			@NonNull final AppCompatActivity activity,
			@NonNull final ArrayList<BodyElement> destination) {

		throw new RuntimeException("Attempt to call generate() on styled text: should be inside a block");
	}
}
