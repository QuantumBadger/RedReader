package org.quantumbadger.redreader.reddit.prepared.html;

import androidx.annotation.NonNull;
import androidx.appcompat.app.AppCompatActivity;
import org.quantumbadger.redreader.reddit.prepared.bodytext.BodyElement;
import org.quantumbadger.redreader.reddit.prepared.bodytext.BodyElementTable;

import java.util.ArrayList;

public class HtmlRawElementTable extends HtmlRawElement {

	@NonNull private final ArrayList<HtmlRawElement> mChildren;

	public HtmlRawElementTable(@NonNull final ArrayList<HtmlRawElement> children) {
		mChildren = children;
	}

	@Override
	public void getPlainText(@NonNull final StringBuilder stringBuilder) {
		for(final HtmlRawElement element : mChildren) {
			element.getPlainText(stringBuilder);
		}
	}

	public HtmlRawElementTable reduce(
			@NonNull final HtmlTextAttributes activeAttributes,
			@NonNull final AppCompatActivity activity,
			@NonNull final ArrayList<LinkButtonDetails> linkButtons) {

		final ArrayList<HtmlRawElement> reduced = new ArrayList<>();

		for(final HtmlRawElement child : mChildren) {
			child.reduce(activeAttributes, activity, reduced, linkButtons);
		}

		return new HtmlRawElementTable(reduced);
	}

	@Override
	public void reduce(
			@NonNull final HtmlTextAttributes activeAttributes,
			@NonNull final AppCompatActivity activity,
			@NonNull final ArrayList<HtmlRawElement> destination,
			@NonNull final ArrayList<LinkButtonDetails> linkButtons) {

		destination.add(reduce(activeAttributes, activity, linkButtons));
	}

	@Override
	public void generate(
			@NonNull final AppCompatActivity activity,
			@NonNull final ArrayList<BodyElement> destination) {

		final ArrayList<BodyElement> rows = new ArrayList<>(mChildren.size());

		for(final HtmlRawElement child : mChildren) {
			child.generate(activity, rows);
		}

		destination.add(new BodyElementTable(rows));
	}
}
