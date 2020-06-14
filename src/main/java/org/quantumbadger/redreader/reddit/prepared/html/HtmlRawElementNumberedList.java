package org.quantumbadger.redreader.reddit.prepared.html;

import android.support.annotation.NonNull;
import android.support.v7.app.AppCompatActivity;
import org.quantumbadger.redreader.reddit.prepared.bodytext.BodyElement;
import org.quantumbadger.redreader.reddit.prepared.bodytext.BodyElementNumberedListElement;

import java.util.ArrayList;

public class HtmlRawElementNumberedList extends HtmlRawElement {

	@NonNull private final ArrayList<HtmlRawElement> mChildren;

	public HtmlRawElementNumberedList(@NonNull final ArrayList<HtmlRawElement> children) {
		mChildren = children;
	}


	public HtmlRawElementNumberedList reduce(
			@NonNull final HtmlTextAttributes activeAttributes,
			@NonNull final AppCompatActivity activity) {

		final ArrayList<HtmlRawElement> reduced = new ArrayList<>();

		for(final HtmlRawElement child : mChildren) {
			child.reduce(activeAttributes, activity, reduced);
		}

		return new HtmlRawElementNumberedList(reduced);
	}

	@Override
	public void reduce(
			@NonNull final HtmlTextAttributes activeAttributes,
			@NonNull final AppCompatActivity activity,
			@NonNull final ArrayList<HtmlRawElement> destination) {

		destination.add(reduce(activeAttributes, activity));
	}

	@Override
	public void generate(
			@NonNull final AppCompatActivity activity,
			@NonNull final ArrayList<BodyElement> destination) {

		int number = 1;

		for(final HtmlRawElement child : mChildren) {

			final ArrayList<BodyElement> thisElement = new ArrayList<>();
			child.generate(activity, thisElement);

			destination.add(new BodyElementNumberedListElement(number, thisElement));

			number++;
		}
	}
}
