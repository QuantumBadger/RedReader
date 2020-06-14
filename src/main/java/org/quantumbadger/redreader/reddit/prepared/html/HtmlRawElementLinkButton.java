package org.quantumbadger.redreader.reddit.prepared.html;

import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.support.v7.app.AppCompatActivity;
import org.quantumbadger.redreader.common.LinkHandler;
import org.quantumbadger.redreader.reddit.prepared.bodytext.BodyElement;
import org.quantumbadger.redreader.reddit.prepared.bodytext.BodyElementButton;

import java.util.ArrayList;

public class HtmlRawElementLinkButton extends HtmlRawElement {

	@NonNull private final LinkButtonDetails mDetails;

	public HtmlRawElementLinkButton(@NonNull final LinkButtonDetails details) {
		mDetails = details;
	}

	@Override
	public void getPlainText(@NonNull final StringBuilder stringBuilder) {
		// Nothing to do
	}

	@Override
	public void reduce(
			@NonNull final HtmlTextAttributes activeAttributes,
			@NonNull final AppCompatActivity activity,
			@NonNull final ArrayList<HtmlRawElement> destination,
			@NonNull final ArrayList<LinkButtonDetails> linkButtons) {

		destination.add(this);
	}

	@Override
	public void generate(
			@NonNull final AppCompatActivity activity,
			@NonNull final ArrayList<BodyElement> destination) {

		@NonNull final String title;
		@Nullable final String subtitle;

		if(mDetails.name == null) {
			title = mDetails.url;
			subtitle = null;

		} else {
			title = mDetails.name;
			subtitle = mDetails.url;
		}

		// TODO BodyElementLinkButton, and hide them if needed
		destination.add(new BodyElementButton(
				title,
				subtitle,
				(button) -> LinkHandler.onLinkClicked(activity, mDetails.url, false),
				(button) -> {
					LinkHandler.onLinkLongClicked(activity, mDetails.url);
					return true;
				}));
	}
}
