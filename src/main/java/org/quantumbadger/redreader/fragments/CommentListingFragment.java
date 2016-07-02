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

package org.quantumbadger.redreader.fragments;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.os.Bundle;
import android.preference.PreferenceManager;
import android.support.v7.app.AppCompatActivity;
import android.support.v7.widget.LinearLayoutManager;
import android.support.v7.widget.RecyclerView;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.widget.FrameLayout;
import android.widget.TextView;
import com.laurencedawson.activetextview.ActiveTextView;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.activities.BugReportActivity;
import org.quantumbadger.redreader.activities.CommentReplyActivity;
import org.quantumbadger.redreader.adapters.CommentListingManager;
import org.quantumbadger.redreader.cache.CacheRequest;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.LinkHandler;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.common.RRError;
import org.quantumbadger.redreader.common.RRThemeAttributes;
import org.quantumbadger.redreader.common.RRTime;
import org.quantumbadger.redreader.reddit.CommentListingRequest;
import org.quantumbadger.redreader.reddit.RedditCommentListItem;
import org.quantumbadger.redreader.reddit.api.RedditAPICommentAction;
import org.quantumbadger.redreader.reddit.prepared.RedditChangeDataManager;
import org.quantumbadger.redreader.reddit.prepared.RedditPreparedPost;
import org.quantumbadger.redreader.reddit.prepared.RedditRenderableComment;
import org.quantumbadger.redreader.reddit.url.RedditURLParser;
import org.quantumbadger.redreader.views.RedditCommentView;
import org.quantumbadger.redreader.views.RedditPostHeaderView;
import org.quantumbadger.redreader.views.RedditPostView;
import org.quantumbadger.redreader.views.ScrollbarRecyclerViewManager;
import org.quantumbadger.redreader.views.bezelmenu.BezelSwipeOverlay;
import org.quantumbadger.redreader.views.bezelmenu.SideToolbarOverlay;
import org.quantumbadger.redreader.views.liststatus.ErrorView;
import org.quantumbadger.redreader.views.liststatus.SpecificCommentThreadView;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.UUID;

public class CommentListingFragment extends RRFragment
		implements ActiveTextView.OnLinkClickedListener,
		RedditPostView.PostSelectionListener,
		RedditCommentView.CommentListener,
		CommentListingRequest.Listener {

	private static final String SAVEDSTATE_FIRST_VISIBLE_POS = "firstVisiblePosition";

	private final RedditAccount mUser;
	private final ArrayList<RedditURLParser.RedditURL> mAllUrls;
	private final LinkedList<RedditURLParser.RedditURL> mUrlsToDownload;
	private final UUID mSession;
	private final @CacheRequest.DownloadType int mDownloadType;

	private RedditPreparedPost mPost = null;
	private boolean isArchived;

	private final CommentListingManager mCommentListingManager;

	private final RecyclerView mRecyclerView;

	private final FrameLayout mOuterFrame;

	private final float mCommentFontScale;
	private final boolean mShowLinkButtons;

	private Long mCachedTimestamp = null;

	private Integer mPreviousFirstVisibleItemPosition;

	public CommentListingFragment(
			final AppCompatActivity parent,
			final Bundle savedInstanceState,
			final ArrayList<RedditURLParser.RedditURL> urls,
			final UUID session,
			final @CacheRequest.DownloadType int downloadType) {

		super(parent, savedInstanceState);

		if(savedInstanceState != null) {
			mPreviousFirstVisibleItemPosition = savedInstanceState.getInt(SAVEDSTATE_FIRST_VISIBLE_POS);
		}

		mCommentListingManager = new CommentListingManager(parent);
		mAllUrls = urls;

		mUrlsToDownload = new LinkedList<>(mAllUrls);

		this.mSession = session;
		this.mDownloadType = downloadType;

		mUser = RedditAccountManager.getInstance(getActivity()).getDefaultAccount();

		parent.invalidateOptionsMenu();

		final Context context = getActivity();

		final SharedPreferences prefs = PreferenceManager.getDefaultSharedPreferences(context);
		mCommentFontScale = PrefsUtility.appearance_fontscale_comments(context, prefs);
		mShowLinkButtons = PrefsUtility.pref_appearance_linkbuttons(context, prefs);

		mOuterFrame = new FrameLayout(context);

		final ScrollbarRecyclerViewManager recyclerViewManager
				= new ScrollbarRecyclerViewManager(context, null, false);

		mRecyclerView = recyclerViewManager.getRecyclerView();
		mCommentListingManager.setLayoutManager((LinearLayoutManager) mRecyclerView.getLayoutManager());

		mRecyclerView.setAdapter(mCommentListingManager.getAdapter());
		mOuterFrame.addView(recyclerViewManager.getOuterView());

		{
			final RecyclerView.ItemAnimator itemAnimator = mRecyclerView.getItemAnimator();
			itemAnimator.setRemoveDuration(80);
			itemAnimator.setChangeDuration(80);
			itemAnimator.setAddDuration(80);
			itemAnimator.setMoveDuration(80);
		}

		final SideToolbarOverlay toolbarOverlay = new SideToolbarOverlay(context);

		final BezelSwipeOverlay bezelOverlay = new BezelSwipeOverlay(context, new BezelSwipeOverlay.BezelSwipeListener() {
			@Override
			public boolean onSwipe(@BezelSwipeOverlay.SwipeEdge int edge) {

				if(mPost == null) return false;

				toolbarOverlay.setContents(mPost.generateToolbar(getActivity(), true, toolbarOverlay));
				toolbarOverlay.show(edge == BezelSwipeOverlay.LEFT ?
						SideToolbarOverlay.SideToolbarPosition.LEFT : SideToolbarOverlay.SideToolbarPosition.RIGHT);
				return true;
			}

			public boolean onTap() {

				if(toolbarOverlay.isShown()) {
					toolbarOverlay.hide();
					return true;
				}

				return false;
			}
		});

		mOuterFrame.addView(bezelOverlay);
		mOuterFrame.addView(toolbarOverlay);

		bezelOverlay.getLayoutParams().width = android.widget.FrameLayout.LayoutParams.MATCH_PARENT;
		bezelOverlay.getLayoutParams().height = android.widget.FrameLayout.LayoutParams.MATCH_PARENT;

		toolbarOverlay.getLayoutParams().width = android.widget.FrameLayout.LayoutParams.MATCH_PARENT;
		toolbarOverlay.getLayoutParams().height = android.widget.FrameLayout.LayoutParams.MATCH_PARENT;

		makeNextRequest(context);
	}

	public void handleCommentVisibilityToggle(final RedditCommentView view) {

		final RedditChangeDataManager changeDataManager = RedditChangeDataManager.getInstance(mUser);
		final RedditCommentListItem item = view.getComment();

		if(item.isComment()) {

			final RedditRenderableComment comment = item.asComment();

			changeDataManager.markHidden(
					RRTime.utcCurrentTimeMillis(),
					comment,
					!comment.isCollapsed(changeDataManager));

			mCommentListingManager.notifyCommentChanged(item);
			mCommentListingManager.updateHiddenStatus();

			final LinearLayoutManager layoutManager = (LinearLayoutManager)mRecyclerView.getLayoutManager();
			final int position = layoutManager.getPosition(view);

			if(position == layoutManager.findFirstVisibleItemPosition()) {
				layoutManager.scrollToPositionWithOffset(position, 0);
			}
		}
	}

	@Override
	public View getView() {
		return mOuterFrame;
	}

	@Override
	public Bundle onSaveInstanceState() {

		final Bundle bundle = new Bundle();

		final LinearLayoutManager layoutManager = (LinearLayoutManager)mRecyclerView.getLayoutManager();
		bundle.putInt(SAVEDSTATE_FIRST_VISIBLE_POS, layoutManager.findFirstVisibleItemPosition());

		return bundle;
	}

	@SuppressLint("WrongConstant")
	private void makeNextRequest(final Context context) {

		if(!mUrlsToDownload.isEmpty()) {
			new CommentListingRequest(
					context,
					this,
					getActivity(),
					mUrlsToDownload.getFirst(),
					mAllUrls.size() == 1,
					mUrlsToDownload.getFirst(),
					mUser,
					mSession,
					mDownloadType,
					this
			);
		}
	}

	public void onClickUrl(String url) {
		if(url != null) LinkHandler.onLinkClicked(getActivity(), url, false, null);
	}

	public void onClickText(Object attachment) {}

	@Override
	public void onCommentClicked(final RedditCommentView view) {
		switch(PrefsUtility.pref_behaviour_actions_comment_tap(
				getActivity(),
				PreferenceManager.getDefaultSharedPreferences(getActivity()))) {

			case COLLAPSE:
				handleCommentVisibilityToggle(view);
				break;

			case ACTION_MENU: {
				final RedditCommentListItem item = view.getComment();
				if(item != null && item.isComment()) {
					RedditAPICommentAction.showActionMenu(
							getActivity(),
							this,
							item.asComment(),
							view,
							RedditChangeDataManager.getInstance(mUser),
							isArchived);
				}
				break;
			}
		}
	}

	@Override
	public void onCommentLongClicked(final RedditCommentView view) {
		switch(PrefsUtility.pref_behaviour_actions_comment_longclick(
			getActivity(),
			PreferenceManager.getDefaultSharedPreferences(getActivity()))) {

			case ACTION_MENU:{
				final RedditCommentListItem item = view.getComment();
				if(item != null && item.isComment()) {
					RedditAPICommentAction.showActionMenu(
						getActivity(),
						this,
						item.asComment(),
						view,
						RedditChangeDataManager.getInstance(mUser),
						isArchived);
				}
				break;
			}

			case COLLAPSE:
				handleCommentVisibilityToggle(view);
				break;

			case NOTHING:
				break;
		}
	}

	@Override
	public void onCommentChanged(final RedditCommentView view) {
		mCommentListingManager.notifyCommentChanged(view.getComment());
	}

	@Override
	public void onCommentListingRequestDownloadNecessary() {
		mCommentListingManager.setLoadingVisible(true);
	}

	@Override
	public void onCommentListingRequestDownloadStarted() {}

	@Override
	public void onCommentListingRequestException(final Throwable t) {
		BugReportActivity.handleGlobalError(getActivity(), t);
	}

	@Override
	public void onCommentListingRequestFailure(final RRError error) {
		mCommentListingManager.setLoadingVisible(false);
		mCommentListingManager.addFooterError(new ErrorView(getActivity(), error));
	}

	@Override
	public void onCommentListingRequestCachedCopy(final long timestamp) {
		mCachedTimestamp = timestamp;
	}

	@Override
	public void onCommentListingRequestParseStart() {
		mCommentListingManager.setLoadingVisible(true);
	}


	@Override
	public void onCommentListingRequestAuthorizing() {
		mCommentListingManager.setLoadingVisible(true);
	}

	@Override
	public void onCommentListingRequestPostDownloaded(final RedditPreparedPost post) {

		final Context context = getActivity();

		if(mPost == null) {

			final RRThemeAttributes attr = new RRThemeAttributes(context);

			mPost = post;
			isArchived = post.isArchived;

			final RedditPostHeaderView postHeader = new RedditPostHeaderView(
					getActivity(),
					CommentListingFragment.this.mPost);

			mCommentListingManager.addPostHeader(postHeader);
			((LinearLayoutManager)mRecyclerView.getLayoutManager()).scrollToPositionWithOffset(0, 0);

			if(post.src.getSelfText() != null) {
				final ViewGroup selfText = post.src.getSelfText().buildView(
						getActivity(), attr.rrMainTextCol, 14f * mCommentFontScale, mShowLinkButtons);
				selfText.setFocusable(false);
				selfText.setDescendantFocusability(ViewGroup.FOCUS_BLOCK_DESCENDANTS);

				final int paddingPx = General.dpToPixels(context, 10);
				final FrameLayout paddingLayout = new FrameLayout(context);
				paddingLayout.addView(selfText);
				paddingLayout.setPadding(paddingPx, paddingPx, paddingPx, paddingPx);

				// TODO mListHeaderNotifications.setBackgroundColor(Color.argb(35, 128, 128, 128));

				mCommentListingManager.addPostSelfText(paddingLayout);
			}

			if(!General.isTablet(context, PreferenceManager.getDefaultSharedPreferences(context))) {
				getActivity().getSupportActionBar().setTitle(post.src.getTitle());
			}

			if(!mAllUrls.isEmpty()
					&& mAllUrls.get(0).pathType() == RedditURLParser.POST_COMMENT_LISTING_URL
					&& mAllUrls.get(0).asPostCommentListURL().commentId != null) {

				final SpecificCommentThreadView specificCommentThreadView = new SpecificCommentThreadView(
						getActivity(),
						mAllUrls.get(0).asPostCommentListURL());

				mCommentListingManager.addNotification(specificCommentThreadView);
			}

			// TODO pref (currently 10 mins)
			if(mCachedTimestamp != null && RRTime.since(mCachedTimestamp) > 10 * 60 * 1000) {

				final TextView cacheNotif = (TextView) LayoutInflater.from(getActivity())
					.inflate(R.layout.cached_header, null, false);
				cacheNotif.setText(getActivity().getString(R.string.listing_cached,
							RRTime.formatDateTime(mCachedTimestamp, getActivity())));
				mCommentListingManager.addNotification(cacheNotif);
			}
		}
	}

	@Override
	public void onCommentListingRequestAllItemsDownloaded(final ArrayList<RedditCommentListItem> items) {

		mCommentListingManager.addComments(items);

		mUrlsToDownload.removeFirst();

		if(mPreviousFirstVisibleItemPosition != null
				&& mCommentListingManager.getItemCount() > mPreviousFirstVisibleItemPosition) {

			((LinearLayoutManager)mRecyclerView.getLayoutManager()).scrollToPositionWithOffset(
					mPreviousFirstVisibleItemPosition,
					0);

			mPreviousFirstVisibleItemPosition = null;
		}

		if(mUrlsToDownload.isEmpty()) {

			if(mCommentListingManager.getCommentCount() == 0) {

				final View noCommentsYet = LayoutInflater.from(getContext()).inflate(
						R.layout.no_comments_yet,
						mRecyclerView,
						false);

				mCommentListingManager.addViewToComments(noCommentsYet);
			}

			mCommentListingManager.setLoadingVisible(false);

		} else {
			makeNextRequest(getActivity());
		}
	}

	@Override
	public void onCreateOptionsMenu(Menu menu) {
		if(mAllUrls != null && mAllUrls.size() > 0 && mAllUrls.get(0).pathType() == RedditURLParser.POST_COMMENT_LISTING_URL) {
			menu.add(R.string.action_reply);
		}
	}

	@Override
	public boolean onOptionsItemSelected(MenuItem item) {

		if(item.getTitle() != null
				&& item.getTitle().equals(getActivity().getString(R.string.action_reply))) {

			onParentReply();
			return true;
		}

		return false;
	}

	private void onParentReply() {

		if(mPost != null) {
			final Intent intent = new Intent(getActivity(), CommentReplyActivity.class);
			intent.putExtra("parentIdAndType", mPost.src.getIdAndType());
			startActivity(intent);

		} else {
			General.quickToast(getActivity(), R.string.error_toast_parent_post_not_downloaded);
		}
	}

	public void onPostSelected(final RedditPreparedPost post) {
		((RedditPostView.PostSelectionListener)getActivity()).onPostSelected(post);
	}

	public void onPostCommentsSelected(final RedditPreparedPost post) {
		((RedditPostView.PostSelectionListener)getActivity()).onPostCommentsSelected(post);
	}
}
