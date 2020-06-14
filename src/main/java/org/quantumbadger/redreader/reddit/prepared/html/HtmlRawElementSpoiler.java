package org.quantumbadger.redreader.reddit.prepared.html;

import android.support.annotation.NonNull;
import android.support.v7.app.AppCompatActivity;
import org.quantumbadger.redreader.reddit.prepared.bodytext.BodyElement;
import org.quantumbadger.redreader.reddit.prepared.bodytext.BodyElementSpoilerButton;
import org.quantumbadger.redreader.reddit.prepared.bodytext.BodyElementVerticalSequence;

import java.util.ArrayList;

public class HtmlRawElementSpoiler extends HtmlRawElement {

	@NonNull private final HtmlRawElementBlock mChild;

	public HtmlRawElementSpoiler(
			@NonNull final HtmlRawElementBlock child) {

		mChild = child;
	}

	@Override
	public void reduce(
			@NonNull final HtmlTextAttributes activeAttributes,
			@NonNull final AppCompatActivity activity,
			@NonNull final ArrayList<HtmlRawElement> destination) {

		destination.add(new HtmlRawElementSpoiler(mChild.reduce(activeAttributes, activity)));
	}

	@Override
	public void generate(@NonNull final ArrayList<BodyElement> destination) {

		final ArrayList<BodyElement> elements = new ArrayList<>();
		mChild.generate(elements);

		destination.add(new BodyElementSpoilerButton(new BodyElementVerticalSequence(elements)));
	}
}
