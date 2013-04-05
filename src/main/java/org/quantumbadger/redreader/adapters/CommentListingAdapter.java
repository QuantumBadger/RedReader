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
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.fragments.CommentListingFragment;
import org.quantumbadger.redreader.reddit.prepared.RedditPreparedComment;
import org.quantumbadger.redreader.views.RedditCommentView;

import java.util.ArrayList;

public final class CommentListingAdapter extends BaseAdapter {

	private final ArrayList<RedditPreparedComment> comments = new ArrayList<RedditPreparedComment>(128),
		commentsToReport = new ArrayList<RedditPreparedComment>(128);

	private final int rrCommentHeaderCol, rrCommentBodyCol;

	private final CommentListingFragment fragment;

	public CommentListingAdapter(Context context, CommentListingFragment fragment) {

		this.fragment = fragment;

		final TypedArray attr = context.obtainStyledAttributes(new int[] {
				R.attr.rrCommentHeaderCol,
				R.attr.rrCommentBodyCol
		});

		rrCommentHeaderCol = attr.getColor(0, 0);
		rrCommentBodyCol = attr.getColor(1, 0);
	}

	public int getCount() {
		return commentsToReport.size();
	}

	public RedditPreparedComment getItem(final int i) {
		return commentsToReport.get(i);
	}

	public long getItemId(int position) {
		return position;
	}

	public int findPositionOf(final RedditPreparedComment comment) {

		for(int i = 0; i < commentsToReport.size(); i++) {
			if(commentsToReport.get(i).equals(comment)) {
				return i;
			}
		}

		return -1;
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
			convertView = new RedditCommentView(viewGroup.getContext(), rrCommentHeaderCol, rrCommentBodyCol);
		}

		((RedditCommentView)convertView).reset(viewGroup.getContext(), fragment, commentsToReport.get(i), fragment);

		return convertView;
	}

	public void addComments(final ArrayList<RedditPreparedComment> comments) {
		this.comments.addAll(comments);
		notifyDataSetChanged();
	}

	@Override
	public void notifyDataSetChanged() {

		commentsToReport.clear();

		for(final RedditPreparedComment comment : comments) {
			if(comment.isVisible()) commentsToReport.add(comment);
		}

		super.notifyDataSetChanged();
	}
}