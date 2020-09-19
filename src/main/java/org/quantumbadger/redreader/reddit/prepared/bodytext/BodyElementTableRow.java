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
import android.view.ViewGroup;
import android.widget.TableRow;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import org.quantumbadger.redreader.activities.BaseActivity;

import java.util.ArrayList;

public class BodyElementTableRow extends BodyElement {

	@NonNull private final ArrayList<BodyElement> mElements;

	public BodyElementTableRow(@NonNull final ArrayList<BodyElement> elements) {
		super(BlockType.TABLE_ROW);
		mElements = elements;
	}

	@Override
	public View generateView(
			@NonNull final BaseActivity activity,
			@Nullable final Integer textColor,
			@Nullable final Float textSize,
			final boolean showLinkButtons) {

		final TableRow result = new TableRow(activity);

		for(final BodyElement element : mElements) {

			final View view = element.generateView(
					activity,
					textColor,
					textSize,
					showLinkButtons);
			result.addView(view);

			final TableRow.LayoutParams layoutParams
					= (TableRow.LayoutParams)view.getLayoutParams();

			layoutParams.width = ViewGroup.LayoutParams.WRAP_CONTENT;
			layoutParams.height = ViewGroup.LayoutParams.WRAP_CONTENT;

			view.setLayoutParams(layoutParams);
		}

		return result;
	}
}
