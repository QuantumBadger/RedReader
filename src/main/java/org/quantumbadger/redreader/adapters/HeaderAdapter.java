/*******************************************************************************
 * This file is part of Scroll.
 *
 * Scroll is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Scroll is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Scroll.  If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/

package org.quantumbadger.redreader.adapters;

import android.database.DataSetObserver;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;

public class HeaderAdapter extends BaseAdapter {

	private final View mHeader;
	private final BaseAdapter mAdapter;

	public HeaderAdapter(final View header, final BaseAdapter adapter) {
		mHeader = header;
		mAdapter = adapter;
	}

	@Override
	public int getCount() {
		return mAdapter.getCount() + 1;
	}

	@Override
	public Object getItem(final int position) {
		if(position == 0) {
			return mHeader;
		} else {
			return mAdapter.getItem(position - 1);
		}
	}

	@Override
	public long getItemId(final int position) {
		if(position == 0) {
			return 0;
		} else {
			return mAdapter.getItemId(position - 1) + 1;
		}
	}

	@Override
	public View getView(final int position, final View convertView, final ViewGroup parent) {
		if(position == 0) {
			return mHeader;
		} else {
			return mAdapter.getView(position - 1, convertView, parent);
		}
	}

	@Override
	public boolean hasStableIds() {
		return mAdapter.hasStableIds();
	}

	@Override
	public boolean isEnabled(final int position) {
		if(position == 0) {
			return true;
		} else {
			return mAdapter.isEnabled(position - 1);
		}
	}

	@Override
	public boolean areAllItemsEnabled() {
		return mAdapter.areAllItemsEnabled();
	}

	@Override
	public int getItemViewType(final int position) {
		if(position == 0) {
			return 0;
		} else {
			return mAdapter.getItemViewType(position - 1) + 1;
		}
	}

	@Override
	public int getViewTypeCount() {
		return mAdapter.getViewTypeCount() + 1;
	}

	@Override
	public void notifyDataSetChanged() {
		mAdapter.notifyDataSetChanged();
		super.notifyDataSetChanged();
	}

	@Override
	public void notifyDataSetInvalidated() {
		mAdapter.notifyDataSetInvalidated();
		super.notifyDataSetInvalidated();
	}

	@Override
	public void registerDataSetObserver(final DataSetObserver observer) {
		super.registerDataSetObserver(observer);
		mAdapter.registerDataSetObserver(observer);
	}

	@Override
	public void unregisterDataSetObserver(final DataSetObserver observer) {
		mAdapter.unregisterDataSetObserver(observer);
		super.unregisterDataSetObserver(observer);
	}
}
