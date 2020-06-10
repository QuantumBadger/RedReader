package org.quantumbadger.redreader.reddit.prepared.bodytext;

import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.support.v7.app.AppCompatActivity;
import android.text.Spanned;
import android.view.View;
import android.widget.TextView;

public class BodyTextElementTextSpanned extends BodyTextElement {

	@NonNull private final Spanned mSpanned;

	public BodyTextElementTextSpanned(@NonNull final Spanned spanned) {
		mSpanned = spanned;
	}

	@Override
	public View generateView(
			@NonNull final AppCompatActivity activity,
			@Nullable final Integer textColor,
			@Nullable final Float textSize,
			final boolean showLinkButtons) {

		final TextView tv = new TextView(activity);

		if(textColor != null) tv.setTextColor(textColor);
		if(textSize != null) tv.setTextSize(textSize);

		tv.setText(mSpanned);

		return tv;
	}
}
