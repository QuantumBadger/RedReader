package org.quantumbadger.redreader.adapters;

import android.os.Handler;
import android.os.Looper;
import android.os.Message;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import org.holoeverywhere.widget.LinearLayout;
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
	private final LinearLayout outerLayout;
	private final PostListingFragment fragmentParent;

	private final Handler postAddedHandler;

	public PostListingAdapter(final ListView listViewParent, final LinearLayout outerLayout, final PostListingFragment fragmentParent) {

		this.listViewParent = listViewParent;
		this.outerLayout = outerLayout;
		this.fragmentParent = fragmentParent;

		postAddedHandler = new Handler(Looper.getMainLooper()) {
			@Override
			public void handleMessage(final Message msg) {

				final RedditPreparedPost post = (RedditPreparedPost)msg.obj;

				if(!postIds.add(post.idAlone)) return;

				posts.add(post);

				if(listViewParent.getLastVisiblePosition() + 1 >= postsToReport.size()) {
					updatePosts();
				}
			}
		};
	}

	private void updatePosts() {
		postsToReport.clear();
		postsToReport.addAll(posts);
		notifyDataSetChanged();
		listViewParent.invalidateViews();
		listViewParent.invalidate();
		listViewParent.requestLayout();
		outerLayout.invalidate();
		outerLayout.requestLayout();
	}

	public void onScroll() {
		if(posts.size() != postsToReport.size()) {
			updatePosts();
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
