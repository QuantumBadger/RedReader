package org.quantumbadger.redreader.adapters;

import android.os.Handler;
import android.os.Looper;
import android.os.Message;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import org.holoeverywhere.widget.ListView;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.fragments.PostListingFragment;
import org.quantumbadger.redreader.reddit.prepared.RedditPreparedPost;
import org.quantumbadger.redreader.views.RedditPostView;

import java.util.ArrayList;
import java.util.HashSet;


public final class PostListingAdapter extends BaseAdapter {

	private final ArrayList<RedditPreparedPost> postsToReport = new ArrayList<RedditPreparedPost>(50);
	private final ArrayList<RedditPreparedPost> posts = new ArrayList<RedditPreparedPost>(50);

	private final HashSet<String> postIds = new HashSet<String>(100);

	private final ListView listViewParent;
	private final PostListingFragment fragmentParent;

	private final Handler postAddedHandler;
	private boolean postUpdateQueued = false;

	private final Runnable updatePostsRunnable = new Runnable() {
		@Override
		public void run() {
			updatePosts();
		}
	};

	public PostListingAdapter(final ListView listViewParent, final PostListingFragment fragmentParent) {

		this.listViewParent = listViewParent;
		this.fragmentParent = fragmentParent;

		postAddedHandler = new Handler(Looper.getMainLooper()) {
			@Override
			public void handleMessage(final Message msg) {

				final RedditPreparedPost post = (RedditPreparedPost)msg.obj;

				if(!postIds.add(post.idAlone)) return;

				posts.add(post);

				queuePostUpdate();
			}
		};
	}

	private void queuePostUpdate() {

		if(postsToReport.size() < 8) {
			updatePosts();

		} else if(!postUpdateQueued) {
			postAddedHandler.postDelayed(updatePostsRunnable, 1000);
			postUpdateQueued = true;
		}
	}

	private void updatePosts() {
		postUpdateQueued = false;
		if(postsToReport.size() != posts.size()) {
			postsToReport.clear();
			postsToReport.addAll(posts);
			notifyDataSetChanged();
		}
	}

	public void onPostDownloaded(final RedditPreparedPost post) {
		postAddedHandler.sendMessage(General.handlerMessage(0, post));
	}

	public int getCount() {
		return postsToReport.size();
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
			convertView = new RedditPostView(viewGroup.getContext(), listViewParent, fragmentParent);
		}

		final RedditPostView rpv = (RedditPostView)convertView;

		rpv.reset(postsToReport.get(i));

		return rpv;
	}

	@Override
	public boolean areAllItemsEnabled() {
		return true;
	}

	public int getDownloadedCount() {
		return posts.size();
	}
}
