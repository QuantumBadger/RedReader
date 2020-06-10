package org.quantumbadger.redreader.reddit.prepared.html;

import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.support.v7.app.AppCompatActivity;
import android.text.SpannableStringBuilder;
import org.quantumbadger.redreader.reddit.prepared.bodytext.BodyTextElement;
import org.quantumbadger.redreader.reddit.prepared.bodytext.BodyTextElementTextSpanned;

import java.util.ArrayList;

public abstract class HtmlRawElementTagAttributeChange extends HtmlRawElementTag {

	private final ArrayList<HtmlRawElement> mChildren;

	public HtmlRawElementTagAttributeChange(final ArrayList<HtmlRawElement> children) {
		mChildren = children;
	}

	protected abstract void onStart(@NonNull HtmlTextAttributes activeAttributes);

	protected abstract void onEnd(@NonNull HtmlTextAttributes activeAttributes);

	@Override
	public final void reduce(
			@NonNull final HtmlTextAttributes activeAttributes,
			@NonNull final AppCompatActivity activity,
			@NonNull final ArrayList<BodyTextElement> destination) {

		onStart(activeAttributes);

		try {

			@Nullable SpannableStringBuilder currentSsb = null;

			for(final HtmlRawElement child : mChildren) {

				if(child instanceof HtmlRawElementTag) {

					if(currentSsb != null) {
						destination.add(new BodyTextElementTextSpanned(currentSsb));
						currentSsb = null;
					}

					((HtmlRawElementTag)child).reduce(activeAttributes, activity, destination);

				} else if(child instanceof HtmlRawElementText) {

					if(currentSsb == null) {
						currentSsb = new SpannableStringBuilder();
					}

					((HtmlRawElementText)child).writeTo(currentSsb, activeAttributes, activity);
				}

			}

			if(currentSsb != null) {
				destination.add(new BodyTextElementTextSpanned(currentSsb));
				currentSsb = null;
			}

		} finally {
			onEnd(activeAttributes);
		}
	}
}
