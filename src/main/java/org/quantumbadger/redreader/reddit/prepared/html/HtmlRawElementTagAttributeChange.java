package org.quantumbadger.redreader.reddit.prepared.html;

import androidx.annotation.NonNull;
import androidx.appcompat.app.AppCompatActivity;
import org.quantumbadger.redreader.reddit.prepared.bodytext.BodyElement;

import java.util.ArrayList;

public abstract class HtmlRawElementTagAttributeChange extends HtmlRawElementTag {

	private final ArrayList<HtmlRawElement> mChildren;

	public HtmlRawElementTagAttributeChange(final ArrayList<HtmlRawElement> children) {
		mChildren = children;
	}

	protected void onLinkButtons(@NonNull final ArrayList<LinkButtonDetails> linkButtons) {
		// Add nothing by default
	}

	protected abstract void onStart(@NonNull HtmlTextAttributes activeAttributes);

	protected abstract void onEnd(@NonNull HtmlTextAttributes activeAttributes);

	@Override
	public void getPlainText(@NonNull final StringBuilder stringBuilder) {
		for(final HtmlRawElement element : mChildren) {
			element.getPlainText(stringBuilder);
		}
	}

	@Override
	public final void reduce(
			@NonNull final HtmlTextAttributes activeAttributes,
			@NonNull final AppCompatActivity activity,
			@NonNull final ArrayList<HtmlRawElement> destination,
			@NonNull final ArrayList<LinkButtonDetails> linkButtons) {

		onStart(activeAttributes);

		try {
			for(final HtmlRawElement child : mChildren) {
				child.reduce(activeAttributes, activity, destination, linkButtons);
			}

		} finally {
			onEnd(activeAttributes);
		}

		onLinkButtons(linkButtons);
	}

	@Override
	public final void generate(
			@NonNull final AppCompatActivity activity,
			@NonNull final ArrayList<BodyElement> destination) {

		throw new RuntimeException("Attempt to call generate() on reducible element");
	}
}
