package org.quantumbadger.redreader.reddit.prepared.bodytext;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;
import android.view.View;
import android.widget.FrameLayout;
import org.quantumbadger.redreader.common.General;

import java.util.ArrayList;

public class BodyElementTableCell extends BodyElement {

	@NonNull private final ArrayList<BodyElement> mElements;

	public BodyElementTableCell(@NonNull final ArrayList<BodyElement> elements) {
		super(BlockType.TABLE_CELL);
		mElements = elements;
	}

	@Override
	public View generateView(
			@NonNull final AppCompatActivity activity,
			@Nullable final Integer textColor,
			@Nullable final Float textSize,
			final boolean showLinkButtons) {

		// Don't show link buttons inside tables

		final View inner = new BodyElementVerticalSequence(mElements)
				.generateView(activity, textColor, textSize, false);

		final FrameLayout padding = new FrameLayout(activity);
		padding.addView(inner);

		final int verticalPaddingPx = General.dpToPixels(activity, 2);
		final int horizontalPaddingPx = General.dpToPixels(activity, 5);
		padding.setPadding(horizontalPaddingPx, verticalPaddingPx, horizontalPaddingPx, verticalPaddingPx);

		return padding;
	}
}
