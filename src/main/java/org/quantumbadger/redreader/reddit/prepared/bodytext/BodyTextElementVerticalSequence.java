package org.quantumbadger.redreader.reddit.prepared.bodytext;

import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.support.v7.app.AppCompatActivity;
import android.view.View;
import android.widget.LinearLayout;

import java.util.ArrayList;

public class BodyTextElementVerticalSequence extends BodyTextElement {

	@NonNull private final ArrayList<BodyTextElement> mElements;

	public BodyTextElementVerticalSequence(@NonNull final ArrayList<BodyTextElement> elements) {
		mElements = elements;
	}


	@Override
	public View generateView(
			@NonNull final AppCompatActivity activity,
			@Nullable final Integer textColor,
			@Nullable final Float textSize,
			final boolean showLinkButtons) {

		final LinearLayout result = new LinearLayout(activity);
		result.setOrientation(LinearLayout.VERTICAL);

		for(final BodyTextElement element : mElements) {
			result.addView(element.generateView(activity, textColor, textSize, showLinkButtons));
		}

		return result;
	}
}
