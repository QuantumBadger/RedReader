package org.quantumbadger.redreader.reddit.prepared.bodytext;

import android.graphics.Color;
import android.graphics.drawable.ColorDrawable;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;
import android.view.View;
import android.view.ViewGroup;
import android.widget.HorizontalScrollView;
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

		final TableLayout table = new TableLayout(activity);

		for(final BodyElement element : mElements) {

			final View view = element.generateView(activity, textColor, textSize, showLinkButtons);
			table.addView(view);
		}

		table.setShowDividers(LinearLayout.SHOW_DIVIDER_MIDDLE);
		table.setDividerDrawable(new ColorDrawable(Color.GRAY));

		table.setLayoutParams(new ViewGroup.LayoutParams(
				ViewGroup.LayoutParams.WRAP_CONTENT,
				ViewGroup.LayoutParams.WRAP_CONTENT));

		final HorizontalScrollView scrollView = new HorizontalScrollView(activity);

		scrollView.addView(table);

		return scrollView;
	}
}
