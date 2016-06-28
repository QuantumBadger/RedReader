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
import android.support.v7.widget.LinearLayoutManager;
import android.support.v7.widget.RecyclerView;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.FrameLayout;
import org.quantumbadger.redreader.R;

public class ScrollbarRecyclerViewManager {

	private final View mOuter;
	private final RecyclerView mRecyclerView;
	private final FrameLayout mScrollbarFrame;
	private final View mScrollbar;

	public ScrollbarRecyclerViewManager(
			final Context context,
			final ViewGroup root,
			final boolean attachToRoot) {

		mOuter = LayoutInflater.from(context).inflate(R.layout.scrollbar_recyclerview, root, attachToRoot);
		mRecyclerView = (RecyclerView) mOuter.findViewById(R.id.scrollbar_recyclerview_recyclerview);
		mScrollbar = mOuter.findViewById(R.id.scrollbar_recyclerview_scrollbar);
		mScrollbarFrame = (FrameLayout) mOuter.findViewById(R.id.scrollbar_recyclerview_scrollbarframe);

		final LinearLayoutManager linearLayoutManager = new LinearLayoutManager(context);
		mRecyclerView.setLayoutManager(linearLayoutManager);
		mRecyclerView.setHasFixedSize(true);
		linearLayoutManager.setSmoothScrollbarEnabled(false);

		mRecyclerView.addOnScrollListener(new RecyclerView.OnScrollListener() {

			@Override
			public void onScrolled(RecyclerView recyclerView, int dx, int dy) {
				final int firstVisible = linearLayoutManager.findFirstVisibleItemPosition();
				final int lastVisible = linearLayoutManager.findLastVisibleItemPosition();
				final int totalCount = linearLayoutManager.getItemCount();

				final int recyclerViewHeight = mRecyclerView.getMeasuredHeight();
				final int scrollBarHeight = mScrollbar.getMeasuredHeight();

				final double topPadding = ((double) firstVisible / (double) totalCount) * (recyclerViewHeight - scrollBarHeight);

				mScrollbarFrame.setPadding(0, (int)Math.round(topPadding), 0, 0);
			}

			@Override
			public void onScrollStateChanged(RecyclerView recyclerView, int newState) {

				switch(newState) {
					case RecyclerView.SCROLL_STATE_IDLE:
						mScrollbar.setVisibility(View.INVISIBLE);
						break;
					case RecyclerView.SCROLL_STATE_DRAGGING:
						mScrollbar.setVisibility(View.VISIBLE);
						break;
					case RecyclerView.SCROLL_STATE_SETTLING:
						break;
				}
			}
		});
	}

	public View getOuterView() {
		return mOuter;
	}

	public RecyclerView getRecyclerView() {
		return mRecyclerView;
	}
}
