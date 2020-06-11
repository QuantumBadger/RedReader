package org.quantumbadger.redreader.reddit.prepared.bodytext;

import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.support.v7.app.AppCompatActivity;
import android.view.View;
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

		return result;
	}
}
