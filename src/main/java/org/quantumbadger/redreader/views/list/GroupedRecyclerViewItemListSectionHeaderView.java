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

package org.quantumbadger.redreader.views.list;

import androidx.annotation.NonNull;
import androidx.core.view.AccessibilityDelegateCompat;
import androidx.core.view.ViewCompat;
import androidx.core.view.accessibility.AccessibilityNodeInfoCompat;
import androidx.recyclerview.widget.RecyclerView;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.adapters.GroupedRecyclerViewAdapter;

public class GroupedRecyclerViewItemListSectionHeaderView extends GroupedRecyclerViewAdapter.Item {

	@NonNull private final CharSequence mText;

	public GroupedRecyclerViewItemListSectionHeaderView(
			@NonNull final CharSequence text) {

		mText = text;
	}

	@Override
	public Class getViewType() {
		// There's no wrapper class for this view, so just use the item class
		return GroupedRecyclerViewItemListSectionHeaderView.class;
	}

	@Override
	public RecyclerView.ViewHolder onCreateViewHolder(final ViewGroup viewGroup) {
		return new RecyclerView.ViewHolder(
				LayoutInflater.from(viewGroup.getContext()).inflate(
						R.layout.list_sectionheader,
						viewGroup,
						false)) {};
	}

	@Override
	public void onBindViewHolder(final RecyclerView.ViewHolder viewHolder) {

		final TextView view = (TextView)viewHolder.itemView;
		view.setText(mText);

		//Use less top padding for the Shortcuts heading (at the very top of the main menu)
		final float TOP_PADDING = 10; //Measured in dps
		if (mText.toString().equals(view.getContext().getString(R.string.mainmenu_header_shortcuts))) {
			view.setPadding(
					view.getPaddingLeft(),
					(int) (TOP_PADDING * view.getContext().getResources().getDisplayMetrics().density),
					view.getPaddingRight(),
					view.getPaddingBottom()
			);
		}

		//From https://stackoverflow.com/a/54082384
		ViewCompat.setAccessibilityDelegate(view, new AccessibilityDelegateCompat() {
			@Override
			public void onInitializeAccessibilityNodeInfo(View host, AccessibilityNodeInfoCompat info) {
				super.onInitializeAccessibilityNodeInfo(host, info);
				info.setHeading(true);
			}
		});
	}

	@Override
	public boolean isHidden() {
		return false;
	}
}
