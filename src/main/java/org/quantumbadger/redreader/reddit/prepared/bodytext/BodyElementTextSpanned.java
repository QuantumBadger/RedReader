package org.quantumbadger.redreader.reddit.prepared.bodytext;

import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.support.v7.app.AppCompatActivity;
import android.text.Spanned;
import android.view.View;
import android.widget.TextView;
import org.quantumbadger.redreader.views.LinkifiedTextView;

public class BodyElementTextSpanned extends BodyElement {

	@NonNull private final Spanned mSpanned;

	public BodyElementTextSpanned(
			@NonNull final BlockType blockType,
			@NonNull final Spanned spanned) {
		super(blockType);
		mSpanned = spanned;
	}

	@Override
	public View generateView(
			@NonNull final AppCompatActivity activity,
			@Nullable final Integer textColor,
			@Nullable final Float textSize,
			final boolean showLinkButtons) {

		final LinkifiedTextView tv = new LinkifiedTextView(activity);

		if(textColor != null) tv.setTextColor(textColor);
		if(textSize != null) tv.setTextSize(textSize);

		tv.setText(mSpanned, TextView.BufferType.SPANNABLE);

		return tv;
	}
}
