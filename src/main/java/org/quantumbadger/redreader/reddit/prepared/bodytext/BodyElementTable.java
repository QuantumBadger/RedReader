package org.quantumbadger.redreader.reddit.prepared.bodytext;

import android.graphics.Color;
import android.graphics.drawable.ColorDrawable;
import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.support.v7.app.AppCompatActivity;
import android.view.View;
import android.view.ViewGroup;
import android.widget.LinearLayout;
import android.widget.TableLayout;

import java.util.ArrayList;

public class BodyElementTable extends BodyElement {

	@NonNull private final ArrayList<BodyElement> mElements;

	public BodyElementTable(@NonNull final ArrayList<BodyElement> elements) {
		super(BlockType.TABLE);
		mElements = elements;
	}

	@Override
	public View generateView(
			@NonNull final AppCompatActivity activity,
			@Nullable final Integer textColor,
			@Nullable final Float textSize,
			final boolean showLinkButtons) {

		final TableLayout result = new TableLayout(activity);

		for(final BodyElement element : mElements) {

			final View view = element.generateView(activity, textColor, textSize, showLinkButtons);
			result.addView(view);
		}

		result.setShowDividers(LinearLayout.SHOW_DIVIDER_MIDDLE);
		result.setDividerDrawable(new ColorDrawable(Color.GRAY));

		result.setShrinkAllColumns(true);

		result.setLayoutParams(new ViewGroup.LayoutParams(
				ViewGroup.LayoutParams.WRAP_CONTENT,
				ViewGroup.LayoutParams.WRAP_CONTENT));

		return result;
	}
}
