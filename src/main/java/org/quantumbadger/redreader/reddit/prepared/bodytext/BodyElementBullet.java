/*******************************************************************************
 * This file is part of RedReader.
 *
 * RedReader is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * RedReader is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with RedReader.  If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/

package org.quantumbadger.redreader.reddit.prepared.bodytext;

import android.view.View;
import android.widget.LinearLayout;
import android.widget.TextView;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import org.quantumbadger.redreader.activities.BaseActivity;
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
			@NonNull final BaseActivity activity,
			@Nullable final Integer textColor,
			@Nullable final Float textSize,
			final boolean showLinkButtons) {

		final LinearLayout bulletItem = new LinearLayout(activity);
		final int paddingPx = General.dpToPixels(activity, 6);
		bulletItem.setPadding(paddingPx, 0, paddingPx, 0);

		final TextView bullet = new TextView(activity);
		bullet.setText("•   ");
		if(textSize != null) {
			bullet.setTextSize(textSize);
		}

		bulletItem.addView(bullet);

		if(mElements.size() == 1) {
			bulletItem.addView(mElements.get(0)
					.generateView(
							activity,
							textColor,
							textSize,
							showLinkButtons));

		} else {
			bulletItem.addView(new BodyElementVerticalSequence(mElements)
					.generateView(
							activity,
							textColor,
							textSize,
							showLinkButtons));
		}

		General.setLayoutMatchWidthWrapHeight(bulletItem);

		return bulletItem;
	}
}
