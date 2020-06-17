package org.quantumbadger.redreader.reddit.prepared.html;

import androidx.annotation.NonNull;
import androidx.appcompat.app.AppCompatActivity;
import org.quantumbadger.redreader.reddit.prepared.bodytext.BodyElement;
import org.quantumbadger.redreader.reddit.prepared.bodytext.BodyElementLinkButton;

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

		destination.add(new BodyElementLinkButton(mDetails));
	}
}
