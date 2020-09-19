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
import android.widget.FrameLayout;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import org.quantumbadger.redreader.activities.BaseActivity;
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
			@NonNull final BaseActivity activity,
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
		padding.setPadding(
				horizontalPaddingPx,
				verticalPaddingPx,
				horizontalPaddingPx,
				verticalPaddingPx);

		return padding;
	}
}
