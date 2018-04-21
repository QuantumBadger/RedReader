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

import android.support.v7.widget.RecyclerView;
import android.view.View;
import android.view.ViewGroup;
import android.widget.FrameLayout;

final class GroupedRecyclerViewItemFrameLayout extends GroupedRecyclerViewAdapter.Item {

	private final View mChildView;
	private boolean mHidden;

	private FrameLayout mParent;

	GroupedRecyclerViewItemFrameLayout(final View childView) {
		mChildView = childView;
	}

	@Override
	public Class getViewType() {
		return this.getClass();
	}

	@Override
	public RecyclerView.ViewHolder onCreateViewHolder(final ViewGroup viewGroup) {
		viewGroup.getLayoutParams().width = ViewGroup.LayoutParams.MATCH_PARENT;

		final FrameLayout frameLayout = new FrameLayout(viewGroup.getContext());
		return new RecyclerView.ViewHolder(frameLayout) {};
	}

	@Override
	public void onBindViewHolder(final RecyclerView.ViewHolder viewHolder) {

		final FrameLayout view = (FrameLayout)viewHolder.itemView;
		view.removeAllViews();

		if(mParent != null && mChildView.getParent() == mParent) {
			mParent.removeAllViews();
		}

		mParent = view;

		view.addView(mChildView);
		mChildView.getLayoutParams().width = ViewGroup.LayoutParams.MATCH_PARENT;
	}

	@Override
	public boolean isHidden() {
		return mHidden;
	}

	public void setHidden(final boolean hidden) {
		mHidden = hidden;
	}
}
