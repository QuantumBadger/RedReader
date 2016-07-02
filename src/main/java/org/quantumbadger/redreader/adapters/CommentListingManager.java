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
import android.support.v7.widget.LinearLayoutManager;
import android.view.View;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.reddit.RedditCommentListItem;
import org.quantumbadger.redreader.views.LoadingSpinnerView;
import org.quantumbadger.redreader.views.RedditPostHeaderView;
import org.quantumbadger.redreader.views.liststatus.ErrorView;

import java.util.Collection;
import java.util.Collections;

public class CommentListingManager {

	private final GroupedRecyclerViewAdapter mAdapter = new GroupedRecyclerViewAdapter(6);
	private LinearLayoutManager mLayoutManager;

	private int mCommentCount = 0;

	private static final int
			GROUP_POST_HEADER = 0,
			GROUP_NOTIFICATIONS = 1,
			GROUP_POST_SELFTEXT = 2,
			GROUP_COMMENTS = 3,
			GROUP_LOADING = 4,
			GROUP_FOOTER_ERRORS = 5;

	private final GroupedRecyclerViewItemFrameLayout mLoadingItem;
	private boolean mWorkaroundDone = false;

	public CommentListingManager(final Context context) {

		final LoadingSpinnerView loadingSpinnerView = new LoadingSpinnerView(context);
		final int paddingPx = General.dpToPixels(context, 30);
		loadingSpinnerView.setPadding(paddingPx, paddingPx, paddingPx, paddingPx);

		mLoadingItem = new GroupedRecyclerViewItemFrameLayout(loadingSpinnerView);
		mAdapter.appendToGroup(GROUP_LOADING, mLoadingItem);
	}

	public void setLayoutManager(final LinearLayoutManager layoutManager) {
		mLayoutManager = layoutManager;
	}

	// Workaround for RecyclerView scrolling behaviour
	private void doWorkaround() {
		if(!mWorkaroundDone && mLayoutManager != null) {
			mLayoutManager.scrollToPositionWithOffset(0, 0);
			mWorkaroundDone = true;
		}
	}

	public void addFooterError(final ErrorView view) {
		mAdapter.appendToGroup(GROUP_FOOTER_ERRORS, new GroupedRecyclerViewItemFrameLayout(view));
	}

	public void addPostHeader(final RedditPostHeaderView view) {
		mAdapter.appendToGroup(GROUP_POST_HEADER, new GroupedRecyclerViewItemFrameLayout(view));
		doWorkaround();
	}

	public void addPostSelfText(final View view) {
		mAdapter.appendToGroup(GROUP_POST_SELFTEXT, new GroupedRecyclerViewItemFrameLayout(view));
		doWorkaround();
	}

	public void addNotification(final View view) {
		mAdapter.appendToGroup(GROUP_NOTIFICATIONS, new GroupedRecyclerViewItemFrameLayout(view));
		doWorkaround();
	}

	public void addComments(final Collection<RedditCommentListItem> comments) {
		mAdapter.appendToGroup(
				GROUP_COMMENTS,
				Collections.<GroupedRecyclerViewAdapter.Item>unmodifiableCollection(comments));
		mCommentCount += comments.size();
		doWorkaround();
	}

	public void addViewToComments(final View view) {
		mAdapter.appendToGroup(GROUP_COMMENTS, new GroupedRecyclerViewItemFrameLayout(view));
		doWorkaround();
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

	public int getCommentCount() {
		return mCommentCount;
	}
}
