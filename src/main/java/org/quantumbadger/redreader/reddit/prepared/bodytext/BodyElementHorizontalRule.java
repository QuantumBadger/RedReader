package org.quantumbadger.redreader.reddit.prepared.bodytext;

import android.graphics.Color;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;
import android.view.View;
import android.view.ViewGroup;
import org.quantumbadger.redreader.common.General;

public class BodyElementHorizontalRule extends BodyElement {

	public BodyElementHorizontalRule() {
		super(BlockType.HORIZONTAL_RULE);
	}

	@Override
	public View generateView(
			@NonNull final AppCompatActivity activity,
			@Nullable final Integer textColor,
			@Nullable final Float textSize,
			final boolean showLinkButtons) {

		final int paddingPx = General.dpToPixels(activity, 3);
		final int thicknessPx = General.dpToPixels(activity, 1);

		final View divider = new View(activity);

		final ViewGroup.MarginLayoutParams layoutParams
				= new ViewGroup.MarginLayoutParams(
						ViewGroup.LayoutParams.MATCH_PARENT,
						thicknessPx);

		layoutParams.leftMargin = paddingPx;
		layoutParams.rightMargin = paddingPx;

		divider.setBackgroundColor(Color.GRAY);
		divider.setLayoutParams(layoutParams);

		return divider;
	}
}
