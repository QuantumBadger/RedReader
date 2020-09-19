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
import android.graphics.drawable.ColorDrawable;
import android.view.View;
import android.view.ViewGroup;
import android.widget.HorizontalScrollView;
import android.widget.LinearLayout;
import android.widget.TableLayout;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import org.quantumbadger.redreader.activities.BaseActivity;

import java.util.ArrayList;

public class BodyElementTable extends BodyElement {

	@NonNull private final ArrayList<BodyElement> mElements;

	public BodyElementTable(@NonNull final ArrayList<BodyElement> elements) {
		super(BlockType.TABLE);
		mElements = elements;
	}

	@Override
	public View generateView(
			@NonNull final BaseActivity activity,
			@Nullable final Integer textColor,
			@Nullable final Float textSize,
			final boolean showLinkButtons) {

		final TableLayout table = new TableLayout(activity);

		for(final BodyElement element : mElements) {

			final View view = element.generateView(
					activity,
					textColor,
					textSize,
					showLinkButtons);
			table.addView(view);
		}

		table.setShowDividers(LinearLayout.SHOW_DIVIDER_MIDDLE);
		table.setDividerDrawable(new ColorDrawable(Color.GRAY));

		table.setLayoutParams(new ViewGroup.LayoutParams(
				ViewGroup.LayoutParams.WRAP_CONTENT,
				ViewGroup.LayoutParams.WRAP_CONTENT));

		final HorizontalScrollView scrollView = new HorizontalScrollView(activity);

		scrollView.addView(table);

		return scrollView;
	}
}
