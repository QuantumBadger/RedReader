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

import android.os.Handler;
import android.os.Looper;
import android.os.Message;
import android.support.v7.app.AppCompatActivity;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.ListView;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.fragments.PostListingFragment;
import org.quantumbadger.redreader.reddit.prepared.RedditPreparedPost;
import org.quantumbadger.redreader.views.RedditPostView;

import java.util.ArrayList;
import java.util.HashSet;


public final class PostListingAdapter extends BaseAdapter {

	private final AppCompatActivity mActivity;

	private final ArrayList<RedditPreparedPost> mPosts = new ArrayList<>(50);

	private final HashSet<String> postIds = new HashSet<>(100);

	private final ListView listViewParent;
	private final PostListingFragment fragmentParent;

	private final Handler postsAddedHandler;

	public PostListingAdapter(final ListView listViewParent, final PostListingFragment fragmentParent, final AppCompatActivity activity) {

		this.listViewParent = listViewParent;
		this.fragmentParent = fragmentParent;
		mActivity = activity;

		postsAddedHandler = new Handler(Looper.getMainLooper()) {
			@Override
			public void handleMessage(final Message msg) {

				final ArrayList<RedditPreparedPost> posts = (ArrayList<RedditPreparedPost>)msg.obj;

				for(final RedditPreparedPost post : posts) {
					if(postIds.add(post.src.getIdAlone())) {
						mPosts.add(post);
					}
				}

				notifyDataSetChanged();

				fragmentParent.onPostsAdded();
			}
		};
	}

	public void onPostsDownloaded(final ArrayList<RedditPreparedPost> posts) {
		postsAddedHandler.sendMessage(General.handlerMessage(0, posts));
	}

	public int getCount() {
		return mPosts.size();
	}

	public Object getItem(final int i) {
		return null;
	}

	public long getItemId(final int i) {
		return i;
	}

	@Override
	public boolean hasStableIds() {
		return true;
	}

	@Override
	public int getItemViewType(final int position) {
		return 0;
	}

	@Override
	public int getViewTypeCount() {
		return 1;
	}

	public RedditPostView getView(final int i, View convertView, final ViewGroup viewGroup) {

		if(convertView == null) {
			convertView = new RedditPostView(viewGroup.getContext(), listViewParent, fragmentParent, mActivity);
		}

		final RedditPostView rpv = (RedditPostView)convertView;

		rpv.reset(mPosts.get(i));

		return rpv;
	}

	@Override
	public boolean areAllItemsEnabled() {
		return true;
	}

	public int getDownloadedCount() {
		return mPosts.size();
	}
}
