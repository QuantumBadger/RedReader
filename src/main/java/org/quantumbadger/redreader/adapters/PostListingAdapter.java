package org.quantumbadger.redreader.adapters;

import android.app.Activity;
import android.os.Handler;
import android.os.Looper;
import android.os.Message;
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

	private final Activity mActivity;

	private final ArrayList<RedditPreparedPost> mPosts = new ArrayList<>(50);

	private final HashSet<String> postIds = new HashSet<>(100);

	private final ListView listViewParent;
	private final PostListingFragment fragmentParent;

	private final Handler postsAddedHandler;

	public PostListingAdapter(final ListView listViewParent, final PostListingFragment fragmentParent, final Activity activity) {

		this.listViewParent = listViewParent;
		this.fragmentParent = fragmentParent;
		mActivity = activity;

		postsAddedHandler = new Handler(Looper.getMainLooper()) {
			@Override
			public void handleMessage(final Message msg) {

				final ArrayList<RedditPreparedPost> posts = (ArrayList<RedditPreparedPost>)msg.obj;

				for(final RedditPreparedPost post : posts) {
					if(postIds.add(post.idAlone)) {
						mPosts.add(post);
					}
				}

				notifyDataSetChanged();
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
