package org.quantumbadger.redreader.reddit.prepared.bodytext;

import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.support.v7.app.AppCompatActivity;
import android.view.View;
import android.widget.LinearLayout;

import java.util.ArrayList;

public class BodyElementVerticalSequence extends BodyElement {

	@NonNull private final ArrayList<BodyElement> mElements;

	public BodyElementVerticalSequence(@NonNull final ArrayList<BodyElement> elements) {
		super(BlockType.VERTICAL_SEQUENCE);
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

		for(final BodyElement element : mElements) {
			result.addView(element.generateView(activity, textColor, textSize, showLinkButtons));
		}

		return result;
	}
}
