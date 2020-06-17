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

import android.graphics.drawable.Drawable;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.recyclerview.widget.RecyclerView;
import android.view.View;
import android.view.ViewGroup;
import org.quantumbadger.redreader.adapters.GroupedRecyclerViewAdapter;

public class GroupedRecyclerViewItemListItemView extends GroupedRecyclerViewAdapter.Item {

	@Nullable private final Drawable mIcon;
	@NonNull private final CharSequence mText;
	private final boolean mHideDivider;

	@Nullable private final View.OnClickListener mClickListener;
	@Nullable private final View.OnLongClickListener mLongClickListener;

	public GroupedRecyclerViewItemListItemView(
			@Nullable final Drawable icon,
			@NonNull final CharSequence text,
			final boolean hideDivider,
			@Nullable final View.OnClickListener clickListener,
			@Nullable final View.OnLongClickListener longClickListener) {

		mIcon = icon;
		mText = text;
		mHideDivider = hideDivider;
		mClickListener = clickListener;
		mLongClickListener = longClickListener;
	}

	@Override
	public Class getViewType() {
		return ListItemView.class;
	}

	@Override
	public RecyclerView.ViewHolder onCreateViewHolder(final ViewGroup viewGroup) {
		return new RecyclerView.ViewHolder(new ListItemView(viewGroup.getContext())) {};
	}

	@Override
	public void onBindViewHolder(final RecyclerView.ViewHolder viewHolder) {

		final ListItemView view = (ListItemView)viewHolder.itemView;

		view.reset(mIcon, mText, mHideDivider);
		view.setOnClickListener(mClickListener);
		view.setOnLongClickListener(mLongClickListener);

		if(mClickListener == null) {
			view.setClickable(false);
		}

		if(mLongClickListener == null) {
			view.setLongClickable(false);
		}
	}

	@Override
	public boolean isHidden() {
		return false;
	}
}
