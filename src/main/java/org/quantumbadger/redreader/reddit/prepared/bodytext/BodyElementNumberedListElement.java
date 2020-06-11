package org.quantumbadger.redreader.reddit.prepared.bodytext;

import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.support.v7.app.AppCompatActivity;
import android.view.View;
import android.widget.LinearLayout;
import android.widget.TextView;
import org.quantumbadger.redreader.common.General;

import java.util.ArrayList;

public class BodyElementNumberedListElement extends BodyElement {

	private final int mListIndex;
	@NonNull private final ArrayList<BodyElement> mElements;

	public BodyElementNumberedListElement(
			final int listIndex,
			@NonNull final ArrayList<BodyElement> elements) {

		super(BlockType.LIST_ELEMENT);
		mListIndex = listIndex;
		mElements = elements;
	}

	@Override
	public View generateView(
			@NonNull final AppCompatActivity activity,
			@Nullable final Integer textColor,
			@Nullable final Float textSize,
			final boolean showLinkButtons) {

		final LinearLayout outerLayout = new LinearLayout(activity);
		final int paddingPx = General.dpToPixels(activity, 6);
		outerLayout.setPadding(paddingPx, 0, paddingPx, 0);

		final TextView number = new TextView(activity);
		number.setText(mListIndex + ".  ");
		if(textSize != null) number.setTextSize(textSize);

		outerLayout.addView(number);

		if(mElements.size() == 1) {
			outerLayout.addView(mElements.get(0)
					.generateView(activity, textColor, textSize, showLinkButtons));

		} else {
			final LinearLayout subItems = new LinearLayout(activity);
			subItems.setOrientation(LinearLayout.VERTICAL);

			for(final BodyElement element : mElements) {
				subItems.addView(element.generateView(activity, textColor, textSize, showLinkButtons));
			}

			outerLayout.addView(subItems);
		}

		return outerLayout;
	}
}
