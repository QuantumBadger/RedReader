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

package org.quantumbadger.redreader.adapters;

import android.view.View;
import android.view.ViewGroup;
import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;
import org.quantumbadger.redreader.common.FunctionOneArgWithReturn;

public class GroupedRecyclerViewItemView
		extends GroupedRecyclerViewAdapter.Item<RecyclerView.ViewHolder> {

	@NonNull private final FunctionOneArgWithReturn<ViewGroup, View> mFactory;
	@NonNull private final Class<?> mViewType;

	private boolean mHidden = false;

	public GroupedRecyclerViewItemView(
			@NonNull final Class<?> viewType,
			@NonNull final FunctionOneArgWithReturn<ViewGroup, View> factory) {
		mFactory = factory;
		mViewType = viewType;
	}


	@NonNull
	@Override
	public Class<?> getViewType() {
		return mViewType;
	}

	@Override
	public RecyclerView.ViewHolder onCreateViewHolder(final ViewGroup viewGroup) {
		final View view = mFactory.apply(viewGroup);
		return new RecyclerView.ViewHolder(view) {};
	}

	@Override
	public void onBindViewHolder(final RecyclerView.ViewHolder viewHolder) {
		// Nothing to do here
	}

	@Override
	public boolean isHidden() {
		return mHidden;
	}

	public void setHidden(final boolean hidden) {
		mHidden = hidden;
	}
}
