package org.quantumbadger.redreader.reddit.prepared.html;

import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.support.v7.app.AppCompatActivity;
import org.quantumbadger.redreader.reddit.prepared.bodytext.BodyElement;

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
			@NonNull final ArrayList<HtmlRawElement> destination) {

		onStart(activeAttributes);

		try {
			for(final HtmlRawElement child : mChildren) {
				child.reduce(activeAttributes, activity, destination);
			}

		} finally {
			onEnd(activeAttributes);
		}
	}

	@Override
	public final void generate(
			@NonNull final AppCompatActivity activity,
			@Nullable final Integer textColor,
			@Nullable final Float textSize,
			@NonNull final ArrayList<BodyElement> destination) {

		throw new RuntimeException("Attempt to call generate() on reducible element");
	}
}
