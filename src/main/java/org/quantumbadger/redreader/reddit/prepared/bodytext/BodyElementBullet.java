package org.quantumbadger.redreader.reddit.prepared.bodytext;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;
import android.view.View;
import android.widget.LinearLayout;
import android.widget.TextView;
import org.quantumbadger.redreader.common.General;

import java.util.ArrayList;

public class BodyElementBullet extends BodyElement {

	@NonNull private final ArrayList<BodyElement> mElements;

	public BodyElementBullet(@NonNull final ArrayList<BodyElement> elements) {
		super(BlockType.LIST_ELEMENT);
		mElements = elements;
	}


	@Override
	public View generateView(
			@NonNull final AppCompatActivity activity,
			@Nullable final Integer textColor,
			@Nullable final Float textSize,
			final boolean showLinkButtons) {

		final LinearLayout bulletItem = new LinearLayout(activity);
		final int paddingPx = General.dpToPixels(activity, 6);
		bulletItem.setPadding(paddingPx, 0, paddingPx, 0);

		final TextView bullet = new TextView(activity);
		bullet.setText("•   ");
		if(textSize != null) bullet.setTextSize(textSize);

		bulletItem.addView(bullet);

		if(mElements.size() == 1) {
			bulletItem.addView(mElements.get(0)
					.generateView(activity, textColor, textSize, showLinkButtons));

		} else {
			bulletItem.addView(new BodyElementVerticalSequence(mElements)
					.generateView(activity, textColor, textSize, showLinkButtons));
		}

		General.setLayoutMatchWidthWrapHeight(bulletItem);

		return bulletItem;
	}
}
