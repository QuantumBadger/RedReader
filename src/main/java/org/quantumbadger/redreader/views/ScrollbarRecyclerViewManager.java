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

package org.quantumbadger.redreader.views;

import android.content.Context;
import android.support.annotation.NonNull;
import android.support.v4.widget.SwipeRefreshLayout;
import android.support.v7.widget.LinearLayoutManager;
import android.support.v7.widget.RecyclerView;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.FrameLayout;
import org.quantumbadger.redreader.R;

public class ScrollbarRecyclerViewManager {

	private final View mOuter;
	private final SwipeRefreshLayout mSwipeRefreshLayout;
	private final RecyclerView mRecyclerView;
	private final FrameLayout mScrollbarFrame;
	private final View mScrollbar;

	private boolean mScrollUnnecessary = false;

	public ScrollbarRecyclerViewManager(
			final Context context,
			final ViewGroup root,
			final boolean attachToRoot) {

		mOuter = LayoutInflater.from(context).inflate(R.layout.scrollbar_recyclerview, root, attachToRoot);
		mSwipeRefreshLayout = mOuter.findViewById(R.id.scrollbar_recyclerview_refreshlayout);
		mRecyclerView = mOuter.findViewById(R.id.scrollbar_recyclerview_recyclerview);
		mScrollbar = mOuter.findViewById(R.id.scrollbar_recyclerview_scrollbar);
		mScrollbarFrame = mOuter.findViewById(R.id.scrollbar_recyclerview_scrollbarframe);

		mSwipeRefreshLayout.setEnabled(false);

		final LinearLayoutManager linearLayoutManager = new LinearLayoutManager(context);
		mRecyclerView.setLayoutManager(linearLayoutManager);
		mRecyclerView.setHasFixedSize(true);
		linearLayoutManager.setSmoothScrollbarEnabled(false);

		mRecyclerView.addOnScrollListener(new RecyclerView.OnScrollListener() {

			private void updateScroll() {

				final int firstVisible = linearLayoutManager.findFirstVisibleItemPosition();
				final int lastVisible = linearLayoutManager.findLastVisibleItemPosition();
				final int itemsVisible = lastVisible - firstVisible + 1;
				final int totalCount = linearLayoutManager.getItemCount();

				final boolean scrollUnnecessary = (itemsVisible == totalCount);

				if(scrollUnnecessary != mScrollUnnecessary) {
					mScrollbar.setVisibility(scrollUnnecessary ? View.INVISIBLE : View.VISIBLE);
				}

				mScrollUnnecessary = scrollUnnecessary;

				if(!scrollUnnecessary) {
					final int recyclerViewHeight = mRecyclerView.getMeasuredHeight();
					final int scrollBarHeight = mScrollbar.getMeasuredHeight();

					final double topPadding = ((double) firstVisible / (double) (totalCount - itemsVisible)) * (recyclerViewHeight - scrollBarHeight);

					mScrollbarFrame.setPadding(0, (int) Math.round(topPadding), 0, 0);
				}
			}

			@Override
			public void onScrolled(RecyclerView recyclerView, int dx, int dy) {
				updateScroll();
			}

			@Override
			public void onScrollStateChanged(RecyclerView recyclerView, int newState) {

				switch(newState) {
					case RecyclerView.SCROLL_STATE_IDLE:
						hideScrollbar();
						break;
					case RecyclerView.SCROLL_STATE_DRAGGING:
						showScrollbar();
						break;
					case RecyclerView.SCROLL_STATE_SETTLING:
						break;
				}

				updateScroll();
			}
		});
	}

	public void enablePullToRefresh(@NonNull final SwipeRefreshLayout.OnRefreshListener listener) {
		mSwipeRefreshLayout.setOnRefreshListener(listener);
		mSwipeRefreshLayout.setEnabled(true);
	}

	private void showScrollbar() {
		mScrollbar.animate().cancel();
		mScrollbar.setAlpha(1f);
	}

	private void hideScrollbar() {
		mScrollbar.animate().alpha(0).setStartDelay(500).setDuration(500).start();
	}

	public View getOuterView() {
		return mOuter;
	}

	public RecyclerView getRecyclerView() {
		return mRecyclerView;
	}
}
