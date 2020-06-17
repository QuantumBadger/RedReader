package org.quantumbadger.redreader.reddit.prepared.html;

import androidx.annotation.NonNull;
import androidx.appcompat.app.AppCompatActivity;
import org.quantumbadger.redreader.reddit.prepared.bodytext.BodyElement;
import org.quantumbadger.redreader.reddit.prepared.bodytext.BodyElementSpoilerButton;
import org.quantumbadger.redreader.reddit.prepared.bodytext.BodyElementVerticalSequence;

import java.util.ArrayList;

public class HtmlRawElementSpoiler extends HtmlRawElement {

	@NonNull private final HtmlRawElementBlock mChild;

	public HtmlRawElementSpoiler(@NonNull final HtmlRawElementBlock child) {
		mChild = child;
	}

	@Override
	public void getPlainText(@NonNull final StringBuilder stringBuilder) {
		mChild.getPlainText(stringBuilder);
	}

	@Override
	public void reduce(
			@NonNull final HtmlTextAttributes activeAttributes,
			@NonNull final AppCompatActivity activity,
			@NonNull final ArrayList<HtmlRawElement> destination,
			@NonNull final ArrayList<LinkButtonDetails> linkButtons) {

		destination.add(new HtmlRawElementSpoiler(mChild.reduce(activeAttributes, activity)));
	}

	@Override
	public void generate(
			@NonNull final AppCompatActivity activity,
			@NonNull final ArrayList<BodyElement> destination) {

		final ArrayList<BodyElement> elements = new ArrayList<>();
		mChild.generate(activity, elements);

		destination.add(new BodyElementSpoilerButton(
				activity,
				new BodyElementVerticalSequence(elements)));
	}
}
