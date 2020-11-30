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
import android.widget.LinearLayout;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import org.quantumbadger.redreader.activities.BaseActivity;
import org.quantumbadger.redreader.common.General;

import java.util.ArrayList;

public class BodyElementQuote extends BodyElement {

	@NonNull private final ArrayList<BodyElement> mElements;

	public BodyElementQuote(@NonNull final ArrayList<BodyElement> elements) {
		super(BlockType.QUOTE);
		mElements = elements;
	}


	@Override
	public View generateView(
			@NonNull final BaseActivity activity,
			@Nullable final Integer textColor,
			@Nullable final Float textSize,
			final boolean showLinkButtons) {

		final LinearLayout quoteLayout = new LinearLayout(activity);

		final int paddingPx = General.dpToPixels(activity, 6);
		quoteLayout.setPadding(paddingPx, paddingPx, paddingPx, 0);

		final int quoteBarWidth = General.dpToPixels(activity, 3);

		final View quoteIndent = new View(activity);
		quoteLayout.addView(quoteIndent);
		quoteIndent.setBackgroundColor(Color.rgb(128, 128, 128));

		{
			final ViewGroup.LayoutParams quoteIndentLayoutParams = quoteIndent.getLayoutParams();
			quoteIndentLayoutParams.width = quoteBarWidth;
			quoteIndentLayoutParams.height = ViewGroup.LayoutParams.MATCH_PARENT;
			((ViewGroup.MarginLayoutParams)quoteIndentLayoutParams).rightMargin
					= quoteBarWidth;
			quoteIndent.setLayoutParams(quoteIndentLayoutParams);
		}

		if(mElements.size() == 1) {
			quoteLayout.addView(mElements.get(0)
					.generateView(
							activity,
							textColor,
							textSize,
							showLinkButtons));

		} else {
			quoteLayout.addView(new BodyElementVerticalSequence(mElements)
					.generateView(
							activity,
							textColor,
							textSize,
							showLinkButtons));
		}

		General.setLayoutMatchWidthWrapHeight(quoteLayout);

		return quoteLayout;
	}
}
