package org.quantumbadger.redreader.reddit.prepared.bodytext;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TableRow;

import java.util.ArrayList;

public class BodyElementTableRow extends BodyElement {

	@NonNull private final ArrayList<BodyElement> mElements;

	public BodyElementTableRow(@NonNull final ArrayList<BodyElement> elements) {
		super(BlockType.TABLE_ROW);
		mElements = elements;
	}

	@Override
	public View generateView(
			@NonNull final AppCompatActivity activity,
			@Nullable final Integer textColor,
			@Nullable final Float textSize,
			final boolean showLinkButtons) {

		final TableRow result = new TableRow(activity);

		for(final BodyElement element : mElements) {

			final View view = element.generateView(activity, textColor, textSize, showLinkButtons);
			result.addView(view);

			final TableRow.LayoutParams layoutParams = (TableRow.LayoutParams)view.getLayoutParams();

			layoutParams.width = ViewGroup.LayoutParams.WRAP_CONTENT;
			layoutParams.height = ViewGroup.LayoutParams.WRAP_CONTENT;

			view.setLayoutParams(layoutParams);
		}

		return result;
	}
}
