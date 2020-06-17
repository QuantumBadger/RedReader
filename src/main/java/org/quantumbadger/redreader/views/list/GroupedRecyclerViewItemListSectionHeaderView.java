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
import androidx.recyclerview.widget.RecyclerView;
import android.view.LayoutInflater;
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
	}

	@Override
	public boolean isHidden() {
		return false;
	}
}
