package org.quantumbadger.redreader.reddit.prepared.bodytext;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;
import android.view.View;
import android.widget.LinearLayout;
import org.quantumbadger.redreader.common.General;

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

		final float dpScale = activity.getResources().getDisplayMetrics().density;
		final int paragraphSpacing = (int) (dpScale * 6);

		@Nullable BlockType lastBlock = null;

		for(final BodyElement element : mElements) {

			final View view = element.generateView(activity, textColor, textSize, showLinkButtons);
			result.addView(view);

			final LinearLayout.LayoutParams layoutParams
					= (LinearLayout.LayoutParams)view.getLayoutParams();

			if(lastBlock != null) {

				if(element.getType() == BlockType.LIST_ELEMENT
						&& lastBlock == BlockType.LIST_ELEMENT) {
					// No spacing

				} else {
					layoutParams.topMargin = paragraphSpacing;
				}

			}

			view.setLayoutParams(layoutParams);

			lastBlock = element.getType();
		}

		General.setLayoutMatchWidthWrapHeight(result);

		return result;
	}
}
