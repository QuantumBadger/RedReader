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
import android.view.View;
import android.view.ViewGroup;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.recyclerview.widget.RecyclerView;
import org.quantumbadger.redreader.adapters.GroupedRecyclerViewAdapter;
import org.quantumbadger.redreader.common.Optional;

public class GroupedRecyclerViewItemListItemView extends GroupedRecyclerViewAdapter.Item {

	@Nullable private final Drawable mIcon;
	@NonNull private final CharSequence mText;
	@Nullable private final String mContentDescription;
	private final boolean mHideDivider;

	@Nullable private final View.OnClickListener mClickListener;
	@Nullable private final View.OnLongClickListener mLongClickListener;

	@NonNull private final Optional<Drawable> mSecondaryIcon;
	@NonNull private final Optional<View.OnClickListener> mSecondaryAction;
	@NonNull private final Optional<String> mSecondaryContentDesc;

	public GroupedRecyclerViewItemListItemView(
			@Nullable final Drawable icon,
			@NonNull final CharSequence text,
			@Nullable final String contentDescription,
			final boolean hideDivider,
			@Nullable final View.OnClickListener clickListener,
			@Nullable final View.OnLongClickListener longClickListener,
			@NonNull final Optional<Drawable> secondaryIcon,
			@NonNull final Optional<View.OnClickListener> secondaryAction,
			@NonNull final Optional<String> secondaryContentDesc) {

		mIcon = icon;
		mText = text;
		mContentDescription = contentDescription;
		mHideDivider = hideDivider;
		mClickListener = clickListener;
		mLongClickListener = longClickListener;
		mSecondaryIcon = secondaryIcon;
		mSecondaryAction = secondaryAction;
		mSecondaryContentDesc = secondaryContentDesc;
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

		((ListItemView)viewHolder.itemView).reset(
				mIcon,
				mText,
				mContentDescription,
				mHideDivider,
				mClickListener,
				mLongClickListener,
				mSecondaryIcon,
				mSecondaryAction,
				mSecondaryContentDesc);
	}

	@Override
	public boolean isHidden() {
		return false;
	}
}
