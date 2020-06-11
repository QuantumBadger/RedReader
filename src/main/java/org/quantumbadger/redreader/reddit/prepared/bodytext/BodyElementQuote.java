package org.quantumbadger.redreader.reddit.prepared.bodytext;

import android.graphics.Color;
import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.support.v7.app.AppCompatActivity;
import android.view.View;
import android.view.ViewGroup;
import android.widget.LinearLayout;
import org.quantumbadger.redreader.common.General;

import java.util.ArrayList;

public class BodyElementQuote extends BodyElement {

	@NonNull private final ArrayList<BodyElement> mElements;

	public BodyElementQuote(@NonNull final ArrayList<BodyElement> elements) {
		super(BlockType.QUOTE);
		mElements = elements;
	}


	@Override
	public View generateView(
			@NonNull final AppCompatActivity activity,
			@Nullable final Integer textColor,
			@Nullable final Float textSize,
			final boolean showLinkButtons) {

		final LinearLayout quoteLayout = new LinearLayout(activity);

		final int paddingPx = General.dpToPixels(activity, 6);
		final int paragraphSpacingPx = General.dpToPixels(activity, 6);
		quoteLayout.setPadding(paddingPx, paddingPx, paddingPx, 0);

		final int quoteBarWidth = General.dpToPixels(activity, 3);

		final View quoteIndent = new View(activity);
		quoteLayout.addView(quoteIndent);
		quoteIndent.setBackgroundColor(Color.rgb(128, 128, 128));
		quoteIndent.getLayoutParams().width = quoteBarWidth;
		quoteIndent.getLayoutParams().height = ViewGroup.LayoutParams.MATCH_PARENT;
		((ViewGroup.MarginLayoutParams)quoteIndent.getLayoutParams()).rightMargin = quoteBarWidth;

		if(mElements.size() == 1) {
			quoteLayout.addView(mElements.get(0)
					.generateView(activity, textColor, textSize, showLinkButtons));

		} else {
			final LinearLayout subItems = new LinearLayout(activity);
			subItems.setOrientation(LinearLayout.VERTICAL);

			boolean first = true;

			for(final BodyElement element : mElements) {

				final View view = element.generateView(activity, textColor, textSize, showLinkButtons);
				subItems.addView(view);

				if(!first) {
					((LinearLayout.LayoutParams)view.getLayoutParams()).topMargin = paragraphSpacingPx;
					view.setLayoutParams(view.getLayoutParams());
				}

				first = false;
			}

			quoteLayout.addView(subItems);
		}

		return quoteLayout;
	}
}
