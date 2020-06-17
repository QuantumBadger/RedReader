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

import androidx.recyclerview.widget.RecyclerView;
import android.view.ViewGroup;

/**
 * Created by veyndan on 18/04/2016.
 */
public abstract class HeaderRecyclerAdapter<VH extends RecyclerView.ViewHolder> extends RecyclerView.Adapter<VH> {

	private static final int TYPE_HEADER = 0;
	private static final int TYPE_CONTENT = 1;

	protected static final int HEADER_SIZE = 1;

	@Override
	public VH onCreateViewHolder(ViewGroup parent, int viewType) {
		switch (viewType) {
			case TYPE_HEADER:
				return onCreateHeaderItemViewHolder(parent);
			case TYPE_CONTENT:
				return onCreateContentItemViewHolder(parent);
			default:
				throw new IllegalStateException();
		}
	}

	protected abstract VH onCreateHeaderItemViewHolder(ViewGroup parent);

	protected abstract VH onCreateContentItemViewHolder(ViewGroup parent);

	@Override
	public void onBindViewHolder(VH holder, int position) {
		if (position == 0) {
			onBindHeaderItemViewHolder(holder, position);
		} else {
			onBindContentItemViewHolder(holder, position - HEADER_SIZE);
		}
	}

	protected abstract void onBindHeaderItemViewHolder(VH holder, int position);

	protected abstract void onBindContentItemViewHolder(VH holder, int position);

	@Override
	public int getItemCount() {
		return getContentItemCount() + HEADER_SIZE;
	}

	@Override
	public int getItemViewType(int position) {
		return position == 0 ? TYPE_HEADER : TYPE_CONTENT;
	}

	protected abstract int getContentItemCount();
}
