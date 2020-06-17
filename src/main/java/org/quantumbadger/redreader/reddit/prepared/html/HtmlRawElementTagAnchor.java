package org.quantumbadger.redreader.reddit.prepared.html;

import androidx.annotation.NonNull;

import java.util.ArrayList;

public class HtmlRawElementTagAnchor extends HtmlRawElementTagAttributeChange {

	@NonNull private final String mHref;

	public HtmlRawElementTagAnchor(
			final ArrayList<HtmlRawElement> children,
			@NonNull final String href) {
		super(children);
		mHref = href;
	}

	@Override
	protected void onLinkButtons(@NonNull final ArrayList<LinkButtonDetails> linkButtons) {

		final String text = getPlainText().trim();

		linkButtons.add(new LinkButtonDetails(
				text.isEmpty() ? null : text,
				mHref));
	}

	@Override
	protected void onStart(@NonNull HtmlTextAttributes activeAttributes) {
		activeAttributes.href = mHref;
	}

	@Override
	protected void onEnd(@NonNull HtmlTextAttributes activeAttributes) {
		activeAttributes.href = null;
	}
}
