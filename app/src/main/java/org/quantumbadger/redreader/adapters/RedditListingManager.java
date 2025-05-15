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
import android.view.View;
import androidx.recyclerview.widget.LinearLayoutManager;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.views.LoadingSpinnerView;
import org.quantumbadger.redreader.views.RedditPostHeaderView;
import org.quantumbadger.redreader.views.liststatus.ErrorView;

import java.util.Collection;

public abstract class RedditListingManager {

	private final GroupedRecyclerViewAdapter mAdapter = new GroupedRecyclerViewAdapter(7);
	private LinearLayoutManager mLayoutManager;

	private static final int GROUP_HEADER = 0;
	private static final int GROUP_NOTIFICATIONS = 1;
	private static final int GROUP_POST_SELFTEXT = 2;
	private static final int GROUP_ITEMS = 3;
	private static final int GROUP_LOAD_MORE_BUTTON = 4;
	private static final int GROUP_LOADING = 5;
	private static final int GROUP_FOOTER_ERRORS = 6;

	private final GroupedRecyclerViewItemFrameLayout mLoadingItem;
	private boolean mWorkaroundDone = false;

	protected RedditListingManager(final Context context) {
		General.checkThisIsUIThread();
		final LoadingSpinnerView loadingSpinnerView = new LoadingSpinnerView(context);
		final int paddingPx = General.dpToPixels(context, 30);
		loadingSpinnerView.setPadding(paddingPx, paddingPx, paddingPx, paddingPx);

		mLoadingItem = new GroupedRecyclerViewItemFrameLayout(loadingSpinnerView);
		mAdapter.appendToGroup(GROUP_LOADING, mLoadingItem);
	}

	public void setLayoutManager(final LinearLayoutManager layoutManager) {
		General.checkThisIsUIThread();
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
		General.checkThisIsUIThread();
		mAdapter.appendToGroup(
				GROUP_FOOTER_ERRORS,
				new GroupedRecyclerViewItemFrameLayout(view));
	}

	public void addPostHeader(final RedditPostHeaderView view) {
		General.checkThisIsUIThread();
		mAdapter.appendToGroup(
				GROUP_HEADER,
				new GroupedRecyclerViewItemFrameLayout(view));
		doWorkaround();
	}

	public void addPostListingHeader(final View view) {
		General.checkThisIsUIThread();
		mAdapter.appendToGroup(
				GROUP_HEADER,
				new GroupedRecyclerViewItemFrameLayout(view));
		doWorkaround();
	}

	public void addPostSelfText(final View view) {
		General.checkThisIsUIThread();
		mAdapter.appendToGroup(
				GROUP_POST_SELFTEXT,
				new GroupedRecyclerViewItemFrameLayout(view));
		doWorkaround();
	}

	public void addNotification(final View view) {
		General.checkThisIsUIThread();
		mAdapter.appendToGroup(
				GROUP_NOTIFICATIONS,
				new GroupedRecyclerViewItemFrameLayout(view));
		doWorkaround();
	}

	public void addItems(final Collection<GroupedRecyclerViewAdapter.Item<?>> items) {
		General.checkThisIsUIThread();
		mAdapter.appendToGroup(GROUP_ITEMS, items);
		doWorkaround();
	}

	public void addViewToItems(final View view) {
		General.checkThisIsUIThread();
		mAdapter.appendToGroup(GROUP_ITEMS, new GroupedRecyclerViewItemFrameLayout(view));
		doWorkaround();
	}

	public void addLoadMoreButton(final View view) {
		General.checkThisIsUIThread();
		mAdapter.appendToGroup(
				GROUP_LOAD_MORE_BUTTON,
				new GroupedRecyclerViewItemFrameLayout(view));
		doWorkaround();
	}

	public void removeLoadMoreButton() {
		General.checkThisIsUIThread();
		mAdapter.removeAllFromGroup(GROUP_LOAD_MORE_BUTTON);
	}

	public void setLoadingVisible(final boolean visible) {
		General.checkThisIsUIThread();
		mLoadingItem.setHidden(!visible);
		mAdapter.updateHiddenStatus();
	}

	public GroupedRecyclerViewAdapter getAdapter() {
		General.checkThisIsUIThread();
		return mAdapter;
	}

	public void updateHiddenStatus() {
		General.checkThisIsUIThread();
		mAdapter.updateHiddenStatus();
	}

	public GroupedRecyclerViewAdapter.Item getItemAtPosition(final int position) {
		return mAdapter.getItemAtPosition(position);
	}
}
