package org.quantumbadger.redreader.reddit.prepared.html;

import android.support.annotation.NonNull;
import android.support.v7.app.AppCompatActivity;
import org.quantumbadger.redreader.reddit.prepared.bodytext.BodyElement;
import org.quantumbadger.redreader.reddit.prepared.bodytext.BodyElementBullet;

import java.util.ArrayList;

public class HtmlRawElementBulletList extends HtmlRawElement {

	@NonNull private final ArrayList<HtmlRawElement> mChildren;

	public HtmlRawElementBulletList(@NonNull final ArrayList<HtmlRawElement> children) {
		mChildren = children;
	}


	public HtmlRawElementBulletList reduce(
			@NonNull final HtmlTextAttributes activeAttributes,
			@NonNull final AppCompatActivity activity) {

		final ArrayList<HtmlRawElement> reduced = new ArrayList<>();

		for(final HtmlRawElement child : mChildren) {
			child.reduce(activeAttributes, activity, reduced);
		}

		return new HtmlRawElementBulletList(reduced);
	}

	@Override
	public void reduce(
			@NonNull final HtmlTextAttributes activeAttributes,
			@NonNull final AppCompatActivity activity,
			@NonNull final ArrayList<HtmlRawElement> destination) {

		destination.add(reduce(activeAttributes, activity));
	}

	@Override
	public void generate(@NonNull final ArrayList<BodyElement> destination) {

		for(final HtmlRawElement child : mChildren) {

			final ArrayList<BodyElement> thisBullet = new ArrayList<>();
			child.generate(thisBullet);

			destination.add(new BodyElementBullet(thisBullet));
		}
	}
}
