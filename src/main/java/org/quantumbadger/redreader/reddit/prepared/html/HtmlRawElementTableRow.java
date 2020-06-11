package org.quantumbadger.redreader.reddit.prepared.html;

import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.support.v7.app.AppCompatActivity;
import org.quantumbadger.redreader.reddit.prepared.bodytext.BodyElement;
import org.quantumbadger.redreader.reddit.prepared.bodytext.BodyElementTableRow;

import java.util.ArrayList;

public class HtmlRawElementTableRow extends HtmlRawElement {

	@NonNull private final ArrayList<HtmlRawElement> mChildren;

	public HtmlRawElementTableRow(@NonNull final ArrayList<HtmlRawElement> children) {
		mChildren = children;
	}


	public HtmlRawElementTableRow reduce(
			@NonNull final HtmlTextAttributes activeAttributes,
			@NonNull final AppCompatActivity activity) {

		final ArrayList<HtmlRawElement> reduced = new ArrayList<>();

		for(final HtmlRawElement child : mChildren) {
			child.reduce(activeAttributes, activity, reduced);
		}

		return new HtmlRawElementTableRow(reduced);
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
			@Nullable final Integer textColor,
			@Nullable final Float textSize,
			@NonNull final ArrayList<BodyElement> destination) {

		final ArrayList<BodyElement> cols = new ArrayList<>(mChildren.size());

		for(final HtmlRawElement child : mChildren) {
			child.generate(activity, textColor, textSize, cols);
		}

		destination.add(new BodyElementTableRow(cols));
	}
}
