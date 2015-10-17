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

package com.konneh.scroll.adapters;

import android.app.Activity;
import android.content.Context;
import android.content.res.TypedArray;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import com.konneh.scroll.R;
import com.konneh.scroll.reddit.RedditPreparedInboxItem;
import com.konneh.scroll.views.RedditInboxItemView;

import java.util.ArrayList;

public final class InboxListingAdapter extends BaseAdapter {

	private final ArrayList<RedditPreparedInboxItem> items = new ArrayList<RedditPreparedInboxItem>(128);

	private final int rrCommentHeaderCol, rrCommentBodyCol;

	private final Activity parentActivity;

	public InboxListingAdapter(Context context, Activity parentActivity) {

		this.parentActivity = parentActivity;

		{
			final TypedArray attr = context.obtainStyledAttributes(new int[]{
					R.attr.rrCommentHeaderCol,
					R.attr.rrCommentBodyCol
			});

			rrCommentHeaderCol = attr.getColor(0, 0);
			rrCommentBodyCol = attr.getColor(1, 0);

			attr.recycle();
		}
	}

	public int getCount() {
		return items.size();
	}

	public RedditPreparedInboxItem getItem(final int i) {
		return items.get(i);
	}

	public long getItemId(int position) {
		return position;
	}

	@Override
	public int getViewTypeCount() {
		return 1;
	}

	@Override
	public int getItemViewType(final int position) {
		return 0;
	}

	public View getView(final int i, View convertView, final ViewGroup viewGroup) {

		if(convertView == null) {
			convertView = new RedditInboxItemView(viewGroup.getContext(), rrCommentHeaderCol, rrCommentBodyCol);
		}

		((RedditInboxItemView)convertView).reset(parentActivity, items.get(i));

		return convertView;
	}

	public void addItem(final RedditPreparedInboxItem comment) {
		items.add(comment);
		notifyDataSetChanged();
	}
}