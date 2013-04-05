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

import android.content.Context;
import android.content.res.TypedArray;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import com.laurencedawson.activetextview.ActiveTextView;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.reddit.RedditPreparedInboxItem;
import org.quantumbadger.redreader.views.RedditInboxItemView;

import java.util.ArrayList;

public final class InboxListingAdapter extends BaseAdapter {

	private final ArrayList<RedditPreparedInboxItem> items = new ArrayList<RedditPreparedInboxItem>(128);

	private final int rrCommentHeaderCol, rrCommentBodyCol;

	private final ActiveTextView.OnLinkClickedListener listener;

	public InboxListingAdapter(Context context, ActiveTextView.OnLinkClickedListener listener) {

		this.listener = listener;

		final TypedArray attr = context.obtainStyledAttributes(new int[] {
				R.attr.rrCommentHeaderCol,
				R.attr.rrCommentBodyCol
		});

		rrCommentHeaderCol = attr.getColor(0, 0);
		rrCommentBodyCol = attr.getColor(1, 0);
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

		((RedditInboxItemView)convertView).reset(viewGroup.getContext(), items.get(i), listener);

		return convertView;
	}

	public void addItem(final RedditPreparedInboxItem comment) {
		items.add(comment);
		notifyDataSetChanged();
	}
}