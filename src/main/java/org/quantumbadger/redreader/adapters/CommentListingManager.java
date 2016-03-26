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
import android.support.v7.widget.RecyclerView;
import android.view.View;
import android.view.ViewGroup;
import android.widget.FrameLayout;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.reddit.RedditCommentListItem;
import org.quantumbadger.redreader.views.LoadingSpinnerView;
import org.quantumbadger.redreader.views.RedditPostHeaderView;
import org.quantumbadger.redreader.views.liststatus.ErrorView;

import java.util.Collection;
import java.util.Collections;

public class CommentListingManager {

	private final GroupedRecyclerViewAdapter mAdapter = new GroupedRecyclerViewAdapter(6);

	private static final int
			GROUP_POST_HEADER = 0,
			GROUP_NOTIFICATIONS = 1,
			GROUP_POST_SELFTEXT = 2,
			GROUP_COMMENTS = 3,
			GROUP_LOADING = 4,
			GROUP_FOOTER_ERRORS = 5;

	private final ItemFrameLayout mLoadingItem;

	private static final class ItemFrameLayout extends GroupedRecyclerViewAdapter.Item {

		private final View mChildView;
		private boolean mHidden;

		private FrameLayout mParent;

		private ItemFrameLayout(final View childView) {
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
			viewGroup.addView(frameLayout);
			frameLayout.getLayoutParams().width = ViewGroup.LayoutParams.MATCH_PARENT;

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

	public CommentListingManager(final Context context) {

		// Workaround for RecyclerView scrolling behaviour
		final View topmostView = new View(context);
		mAdapter.appendToGroup(0, new ItemFrameLayout(topmostView));

		final LoadingSpinnerView loadingSpinnerView = new LoadingSpinnerView(context);
		final int paddingPx = General.dpToPixels(context, 30);
		loadingSpinnerView.setPadding(paddingPx, paddingPx, paddingPx, paddingPx);

		mLoadingItem = new ItemFrameLayout(loadingSpinnerView);
		mAdapter.appendToGroup(GROUP_LOADING, mLoadingItem);
	}

	public void addFooterError(final ErrorView view) {
		mAdapter.appendToGroup(GROUP_FOOTER_ERRORS, new ItemFrameLayout(view));
	}

	public void addPostHeader(final RedditPostHeaderView view) {
		mAdapter.appendToGroup(GROUP_POST_HEADER, new ItemFrameLayout(view));
	}

	public void addPostSelfText(final View view) {
		mAdapter.appendToGroup(GROUP_POST_SELFTEXT, new ItemFrameLayout(view));
	}

	public void addNotification(final View view) {
		mAdapter.appendToGroup(GROUP_NOTIFICATIONS, new ItemFrameLayout(view));
	}

	public void addComments(final Collection<RedditCommentListItem> comments) {
		mAdapter.appendToGroup(
				GROUP_COMMENTS,
				Collections.<GroupedRecyclerViewAdapter.Item>unmodifiableCollection(comments));
	}

	public void setLoadingVisible(final boolean visible) {
		mLoadingItem.setHidden(!visible);
		mAdapter.updateHiddenStatus();
	}

	public GroupedRecyclerViewAdapter getAdapter() {
		return mAdapter;
	}

	public void updateHiddenStatus() {
		mAdapter.updateHiddenStatus();
	}

	public void notifyCommentChanged(final RedditCommentListItem item) {
		mAdapter.notifyItemChanged(GROUP_COMMENTS, item);
	}

	public int getItemCount() {
		return mAdapter.getItemCount();
	}
}
