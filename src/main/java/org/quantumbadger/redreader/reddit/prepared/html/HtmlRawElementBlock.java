package org.quantumbadger.redreader.reddit.prepared.html;

import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.support.v7.app.AppCompatActivity;
import android.text.SpannableStringBuilder;
import org.quantumbadger.redreader.reddit.prepared.bodytext.BodyTextElement;
import org.quantumbadger.redreader.reddit.prepared.bodytext.BodyTextElementTextSpanned;

import java.util.ArrayList;

public class HtmlRawElementBlock extends HtmlRawElement {

	private final ArrayList<HtmlRawElement> mChildren;

	public HtmlRawElementBlock(final ArrayList<HtmlRawElement> children) {
		mChildren = children;
	}

	@Override
	public void reduce(
			@NonNull final HtmlTextAttributes activeAttributes,
			@NonNull final AppCompatActivity activity,
			@NonNull final ArrayList<HtmlRawElement> destination) {

		final ArrayList<HtmlRawElement> reduced = new ArrayList<>();

		for(final HtmlRawElement child : mChildren) {
			child.reduce(activeAttributes, activity, reduced);
		}

		destination.add(new HtmlRawElementBlock(reduced));
	}

	@Override
	public void generate(
			@NonNull final AppCompatActivity activity,
			@NonNull final ArrayList<BodyTextElement> destination) {

		@Nullable SpannableStringBuilder currentSsb = null;

		for(final HtmlRawElement child : mChildren) {

			if(child instanceof HtmlRawElementStyledText) {

				if(currentSsb == null) {
					currentSsb = new SpannableStringBuilder();
				}

				((HtmlRawElementStyledText)child).writeTo(currentSsb);

			} else {

				if(currentSsb != null) {
					destination.add(new BodyTextElementTextSpanned(currentSsb));
					currentSsb = null;
				}

				child.generate(activity, destination);
			}
		}

		if(currentSsb != null) {
			destination.add(new BodyTextElementTextSpanned(currentSsb));
			currentSsb = null;
		}
	}
}
