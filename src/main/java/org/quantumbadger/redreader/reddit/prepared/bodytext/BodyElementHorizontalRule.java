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

import android.graphics.Color;
import android.view.View;
import android.view.ViewGroup;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import org.quantumbadger.redreader.activities.BaseActivity;
import org.quantumbadger.redreader.common.General;

public class BodyElementHorizontalRule extends BodyElement {

	public BodyElementHorizontalRule() {
		super(BlockType.HORIZONTAL_RULE);
	}

	@Override
	public View generateView(
			@NonNull final BaseActivity activity,
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
