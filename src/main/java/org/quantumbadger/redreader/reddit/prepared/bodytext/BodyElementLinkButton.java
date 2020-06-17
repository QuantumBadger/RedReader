package org.quantumbadger.redreader.reddit.prepared.bodytext;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;
import android.view.View;
import org.quantumbadger.redreader.common.LinkHandler;
import org.quantumbadger.redreader.reddit.prepared.html.HtmlRawElement;

public class BodyElementLinkButton extends BodyElementBaseButton {

	@NonNull private final HtmlRawElement.LinkButtonDetails mDetails;

	public BodyElementLinkButton(
			@NonNull final HtmlRawElement.LinkButtonDetails details) {
		super(details.getButtonTitle(), details.getButtonSubtitle(), true);
		mDetails = details;
	}

	@NonNull
	@Override
	protected View.OnClickListener generateOnClickListener(
			@NonNull final AppCompatActivity activity,
			@Nullable final Integer textColor,
			@Nullable final Float textSize,
			final boolean showLinkButtons) {

		return (button) -> LinkHandler.onLinkClicked(activity, mDetails.url, false);
	}

	@Nullable
	@Override
	protected View.OnLongClickListener generateOnLongClickListener(
			@NonNull final AppCompatActivity activity,
			@Nullable final Integer textColor,
			@Nullable final Float textSize,
			final boolean showLinkButtons) {

		return (button) -> {
			LinkHandler.onLinkLongClicked(activity, mDetails.url);
			return true;
		};
	}
}
