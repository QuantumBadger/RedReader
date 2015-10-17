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
import com.konneh.scroll.fragments.CommentListingFragment;
import com.konneh.scroll.reddit.RedditCommentListItem;
import com.konneh.scroll.views.LoadMoreCommentsView;
import com.konneh.scroll.views.RedditCommentView;

import java.util.ArrayList;

public final class CommentListingAdapter extends BaseAdapter {

	private final ArrayList<RedditCommentListItem> comments = new ArrayList<RedditCommentListItem>(128),
		commentsToReport = new ArrayList<RedditCommentListItem>(128);

	private final int rrCommentHeaderCol, rrCommentBodyCol;

	private final CommentListingFragment fragment;

	private final Activity mActivity;

	public CommentListingAdapter(Activity activity, CommentListingFragment fragment) {

		mActivity = activity;
		this.fragment = fragment;

		final TypedArray attr = activity.obtainStyledAttributes(new int[] {
				R.attr.rrCommentHeaderCol,
				R.attr.rrCommentBodyCol
		});

		rrCommentHeaderCol = attr.getColor(0, 0);
		rrCommentBodyCol = attr.getColor(1, 0);

		attr.recycle();
	}

	public int getCount() {
		return commentsToReport.size();
	}

	public RedditCommentListItem getItem(final int i) {
		return commentsToReport.get(i);
	}

	public long getItemId(int position) {
		return position;
	}

	@Override
	public int getViewTypeCount() {
		return 2;
	}

	@Override
	public int getItemViewType(final int position) {
		if(commentsToReport.get(position).isComment()) {
			return 0;
		} else {
			return 1;
		}
	}

	public View getView(final int i, View convertView, final ViewGroup viewGroup) {

		final RedditCommentListItem item = commentsToReport.get(i);

		if(item.isComment()) {

			if(convertView == null) {
				convertView = new RedditCommentView(viewGroup.getContext(), rrCommentHeaderCol, rrCommentBodyCol, fragment);
			}

			((RedditCommentView) convertView).reset(mActivity, item.asComment(), item.getIndent());

		} else {

			if(convertView == null) {
				convertView = new LoadMoreCommentsView(viewGroup.getContext());
			}

			((LoadMoreCommentsView)convertView).reset(item);
		}

		return convertView;
	}

	public void addItems(final ArrayList<RedditCommentListItem> comments) {
		this.comments.addAll(comments);
		notifyDataSetChanged();
	}

	@Override
	public void notifyDataSetChanged() {

		commentsToReport.clear();

		for(final RedditCommentListItem item : comments) {
			if(item.isVisible()) commentsToReport.add(item);
		}

		super.notifyDataSetChanged();
	}
}