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
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import org.quantumbadger.redreader.activities.BaseActivity;
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
			@NonNull final BaseActivity activity,
			@Nullable final Integer textColor,
			@Nullable final Float textSize,
			final boolean showLinkButtons) {

		final LinearLayout result = new LinearLayout(activity);
		result.setOrientation(LinearLayout.VERTICAL);

		final float dpScale = activity.getResources().getDisplayMetrics().density;
		final int paragraphSpacing = (int)(dpScale * 6);

		@Nullable BlockType lastBlock = null;

		for(final BodyElement element : mElements) {

			final View view = element.generateView(
					activity,
					textColor,
					textSize,
					showLinkButtons);
			result.addView(view);

			final LinearLayout.LayoutParams layoutParams
					= (LinearLayout.LayoutParams)view.getLayoutParams();

			if(lastBlock != null) {

				if(!(element.getType() == BlockType.LIST_ELEMENT
						&& lastBlock == BlockType.LIST_ELEMENT)) {
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
