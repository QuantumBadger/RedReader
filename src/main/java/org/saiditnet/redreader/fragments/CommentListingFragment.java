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

package org.saiditnet.redreader.fragments;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.graphics.Color;
import android.os.Bundle;
import android.preference.PreferenceManager;
import android.support.annotation.Nullable;
import android.support.v4.widget.SwipeRefreshLayout;
import android.support.v7.app.AppCompatActivity;
import android.support.v7.widget.LinearLayoutManager;
import android.support.v7.widget.RecyclerView;
import android.view.*;
import android.view.animation.Animation;
import android.view.animation.AnimationUtils;
import android.view.animation.OvershootInterpolator;
import android.widget.FrameLayout;
import android.widget.ImageButton;
import android.widget.LinearLayout;
import android.widget.TextView;
import org.saiditnet.redreader.R;
import org.saiditnet.redreader.account.RedditAccount;
import org.saiditnet.redreader.account.RedditAccountManager;
import org.saiditnet.redreader.activities.BugReportActivity;
import org.saiditnet.redreader.activities.CommentReplyActivity;
import org.saiditnet.redreader.activities.OptionsMenuUtility;
import org.saiditnet.redreader.adapters.FilteredCommentListingManager;
import org.saiditnet.redreader.adapters.GroupedRecyclerViewAdapter;
import org.saiditnet.redreader.cache.downloadstrategy.DownloadStrategy;
import org.saiditnet.redreader.cache.downloadstrategy.DownloadStrategyAlways;
import org.saiditnet.redreader.cache.downloadstrategy.DownloadStrategyIfNotCached;
import org.saiditnet.redreader.common.*;
import org.saiditnet.redreader.reddit.CommentListingRequest;
import org.saiditnet.redreader.reddit.RedditCommentListItem;
import org.saiditnet.redreader.reddit.api.RedditAPICommentAction;
import org.saiditnet.redreader.reddit.prepared.RedditChangeDataManager;
import org.saiditnet.redreader.reddit.prepared.RedditPreparedPost;
import org.saiditnet.redreader.reddit.prepared.RedditRenderableComment;
import org.saiditnet.redreader.reddit.url.RedditURLParser;
import org.saiditnet.redreader.views.RedditCommentView;
import org.saiditnet.redreader.views.RedditPostHeaderView;
import org.saiditnet.redreader.views.RedditPostView;
import org.saiditnet.redreader.views.ScrollbarRecyclerViewManager;
import org.saiditnet.redreader.views.bezelmenu.BezelSwipeOverlay;
import org.saiditnet.redreader.views.bezelmenu.SideToolbarOverlay;
import org.saiditnet.redreader.views.liststatus.CommentSubThreadView;
import org.saiditnet.redreader.views.liststatus.ErrorView;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.UUID;

public class CommentListingFragment extends RRFragment
		implements RedditPostView.PostSelectionListener,
		RedditCommentView.CommentListener,
		CommentListingRequest.Listener {

	private static final String SAVEDSTATE_FIRST_VISIBLE_POS = "firstVisiblePosition";

	private final RedditAccount mUser;
	private final ArrayList<RedditURLParser.RedditURL> mAllUrls;
	private final LinkedList<RedditURLParser.RedditURL> mUrlsToDownload;
	private final UUID mSession;
	private final DownloadStrategy mDownloadStrategy;

	private RedditPreparedPost mPost = null;
	private boolean isArchived;

	private final FilteredCommentListingManager mCommentListingManager;

	private final RecyclerView mRecyclerView;

	private final FrameLayout mOuterFrame;
	private final @Nullable LinearLayout mFloatingToolbar;

	private final float mCommentFontScale;
	private final boolean mShowLinkButtons;

	private Long mCachedTimestamp = null;

	private Integer mPreviousFirstVisibleItemPosition;

	public CommentListingFragment(
			final AppCompatActivity parent,
			final Bundle savedInstanceState,
			final ArrayList<RedditURLParser.RedditURL> urls,
			final UUID session,
			final String searchString,
			final boolean forceDownload) {

		super(parent, savedInstanceState);

		if(savedInstanceState != null) {
			mPreviousFirstVisibleItemPosition = savedInstanceState.getInt(SAVEDSTATE_FIRST_VISIBLE_POS);
		}

		mCommentListingManager = new FilteredCommentListingManager(parent, searchString);
		mAllUrls = urls;

		mUrlsToDownload = new LinkedList<>(mAllUrls);

		this.mSession = session;

		if(forceDownload) {
			mDownloadStrategy = DownloadStrategyAlways.INSTANCE;

		} else {
			mDownloadStrategy = DownloadStrategyIfNotCached.INSTANCE;
		}

		mUser = RedditAccountManager.getInstance(getActivity()).getDefaultAccount();

		parent.invalidateOptionsMenu();

		final Context context = getActivity();

		final SharedPreferences prefs = PreferenceManager.getDefaultSharedPreferences(context);
		mCommentFontScale = PrefsUtility.appearance_fontscale_comments(context, prefs);
		mShowLinkButtons = PrefsUtility.pref_appearance_linkbuttons(context, prefs);

		mOuterFrame = new FrameLayout(context);

		final ScrollbarRecyclerViewManager recyclerViewManager
				= new ScrollbarRecyclerViewManager(context, null, false);

		if(parent instanceof OptionsMenuUtility.OptionsMenuCommentsListener
				&& PrefsUtility.pref_behaviour_enable_swipe_refresh(context, prefs)) {

			recyclerViewManager.enablePullToRefresh(new SwipeRefreshLayout.OnRefreshListener() {
				@Override
				public void onRefresh() {
					((OptionsMenuUtility.OptionsMenuCommentsListener)parent).onRefreshComments();
				}
			});
		}

		mRecyclerView = recyclerViewManager.getRecyclerView();
		mCommentListingManager.setLayoutManager((LinearLayoutManager) mRecyclerView.getLayoutManager());

		mRecyclerView.setAdapter(mCommentListingManager.getAdapter());
		mOuterFrame.addView(recyclerViewManager.getOuterView());

		mRecyclerView.setItemAnimator(null);

		/* TODO
		{
			final RecyclerView.ItemAnimator itemAnimator = mRecyclerView.getItemAnimator();
			itemAnimator.setRemoveDuration(80);
			itemAnimator.setChangeDuration(80);
			itemAnimator.setAddDuration(80);
			itemAnimator.setMoveDuration(80);
		}
		*/

		if(!PrefsUtility.pref_appearance_comments_show_floating_toolbar(context, prefs)) {
			mFloatingToolbar = null;

		} else {
			mFloatingToolbar = (LinearLayout) LayoutInflater.from(context).inflate(R.layout.floating_toolbar, mOuterFrame, false);

			// We need a container so that setVisible() doesn't mess with the Z-order
			final FrameLayout floatingToolbarContainer = new FrameLayout(context);

			floatingToolbarContainer.addView(mFloatingToolbar);
			mOuterFrame.addView(floatingToolbarContainer);

			if(PrefsUtility.isNightMode(context)) {
				mFloatingToolbar.setBackgroundColor(Color.argb(0xCC, 0x33, 0x33, 0x33));
			}

			final int buttonVPadding = General.dpToPixels(context, 12);
			final int buttonHPadding = General.dpToPixels(context, 16);

			{
				final ImageButton previousButton = (ImageButton) LayoutInflater.from(context).inflate(
						R.layout.flat_image_button, mFloatingToolbar, false);

				previousButton.setPadding(buttonHPadding, buttonVPadding, buttonHPadding, buttonVPadding);
				previousButton.setImageResource(R.drawable.ic_ff_up_dark);
				previousButton.setContentDescription(getString(R.string.button_prev_comment_parent));
				mFloatingToolbar.addView(previousButton);

				previousButton.setOnClickListener(new View.OnClickListener() {
					@Override
					public void onClick(final View view) {

						final LinearLayoutManager layoutManager = (LinearLayoutManager) mRecyclerView.getLayoutManager();

						for(int pos = layoutManager.findFirstVisibleItemPosition() - 1;
								pos > 0;
								pos--) {

							final GroupedRecyclerViewAdapter.Item item = mCommentListingManager.getItemAtPosition(pos);

							if(item instanceof RedditCommentListItem
									&& ((RedditCommentListItem) item).isComment()
									&& ((RedditCommentListItem) item).getIndent() == 0) {

								layoutManager.scrollToPositionWithOffset(pos, 0);
								return;
							}
						}

						layoutManager.scrollToPositionWithOffset(0, 0);
					}
				});

				previousButton.setOnLongClickListener(new View.OnLongClickListener() {
					@Override
					public boolean onLongClick(final View view) {
						General.quickToast(context, R.string.button_prev_comment_parent);
						return true;
					}
				});
			}

			{
				final ImageButton nextButton = (ImageButton) LayoutInflater.from(context).inflate(
						R.layout.flat_image_button, mFloatingToolbar, false);

				nextButton.setPadding(buttonHPadding, buttonVPadding, buttonHPadding, buttonVPadding);
				nextButton.setImageResource(R.drawable.ic_ff_down_dark);
				nextButton.setContentDescription(getString(R.string.button_next_comment_parent));
				mFloatingToolbar.addView(nextButton);

				nextButton.setOnClickListener(new View.OnClickListener() {
					@Override
					public void onClick(final View view) {

						final LinearLayoutManager layoutManager = (LinearLayoutManager) mRecyclerView.getLayoutManager();

						for(int pos = layoutManager.findFirstVisibleItemPosition() + 1;
							pos < layoutManager.getItemCount();
							pos++) {

							final GroupedRecyclerViewAdapter.Item item = mCommentListingManager.getItemAtPosition(pos);

							if(item instanceof RedditCommentListItem
									&& ((RedditCommentListItem) item).isComment()
									&& ((RedditCommentListItem) item).getIndent() == 0) {

								layoutManager.scrollToPositionWithOffset(pos, 0);
								break;
							}
						}
					}
				});

				nextButton.setOnLongClickListener(new View.OnLongClickListener() {
					@Override
					public boolean onLongClick(final View view) {
						General.quickToast(context, R.string.button_next_comment_parent);
						return true;
					}
				});
			}
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
					mDownloadStrategy,
					this
			);
		}
	}

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
					this.mPost);

			mCommentListingManager.addPostHeader(postHeader);

			final LinearLayoutManager layoutManager = (LinearLayoutManager)mRecyclerView.getLayoutManager();
			layoutManager.scrollToPositionWithOffset(0, 0);

			if(post.src.getSelfText() != null) {
				final ViewGroup selfText = post.src.getSelfText().buildView(
						getActivity(), attr.rrMainTextCol, 14f * mCommentFontScale, mShowLinkButtons);
				selfText.setFocusable(false);
				selfText.setDescendantFocusability(ViewGroup.FOCUS_BLOCK_DESCENDANTS);

				final int paddingPx = General.dpToPixels(context, 10);
				final FrameLayout paddingLayout = new FrameLayout(context);
				final TextView collapsedView = new TextView(context);
				collapsedView.setText("[ + ]  " + getActivity().getString(R.string.collapsed_self_post));
				collapsedView.setVisibility(View.GONE);
				collapsedView.setPadding(paddingPx, paddingPx, paddingPx, paddingPx);
				paddingLayout.addView(selfText);
				paddingLayout.addView(collapsedView);
				paddingLayout.setPadding(paddingPx, paddingPx, paddingPx, paddingPx);

				PrefsUtility.SelfpostAction actionOnClick = PrefsUtility.pref_behaviour_self_post_tap_actions(context, PreferenceManager.getDefaultSharedPreferences(context));
				if (actionOnClick == PrefsUtility.SelfpostAction.COLLAPSE) {
					paddingLayout.setOnClickListener(new View.OnClickListener() {
						@Override
						public void onClick(View v) {
							if (selfText.getVisibility() == View.GONE) {
								selfText.setVisibility(View.VISIBLE);
								collapsedView.setVisibility(View.GONE);
							} else {
								selfText.setVisibility(View.GONE);
								collapsedView.setVisibility(View.VISIBLE);
								layoutManager.scrollToPositionWithOffset(0, 0);
							}
						}
					});
				}
				// TODO mListHeaderNotifications.setBackgroundColor(Color.argb(35, 128, 128, 128));

				mCommentListingManager.addPostSelfText(paddingLayout);
			}

			if(!General.isTablet(context, PreferenceManager.getDefaultSharedPreferences(context))) {
				getActivity().setTitle(post.src.getTitle());
			}

			if (mCommentListingManager.isSearchListing()) {
				final CommentSubThreadView searchCommentThreadView= new CommentSubThreadView(
						getActivity(),
						mAllUrls.get(0).asPostCommentListURL(),
						R.string.comment_header_search_thread_title
				);

				mCommentListingManager.addNotification(searchCommentThreadView);
			} else if(!mAllUrls.isEmpty()
					&& mAllUrls.get(0).pathType() == RedditURLParser.POST_COMMENT_LISTING_URL
					&& mAllUrls.get(0).asPostCommentListURL().commentId != null) {

				final CommentSubThreadView specificCommentThreadView = new CommentSubThreadView(
						getActivity(),
						mAllUrls.get(0).asPostCommentListURL(),
						R.string.comment_header_specific_thread_title);

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

		if(mFloatingToolbar != null && mFloatingToolbar.getVisibility() != View.VISIBLE) {
			mFloatingToolbar.setVisibility(View.VISIBLE);
			final Animation animation = AnimationUtils.loadAnimation(getContext(), R.anim.slide_in_from_bottom);
			animation.setInterpolator(new OvershootInterpolator());
			mFloatingToolbar.startAnimation(animation);
		}

		mUrlsToDownload.removeFirst();

		final LinearLayoutManager layoutManager = (LinearLayoutManager)mRecyclerView.getLayoutManager();

		if(mPreviousFirstVisibleItemPosition != null
				&& layoutManager.getItemCount() > mPreviousFirstVisibleItemPosition) {

			layoutManager.scrollToPositionWithOffset(
					mPreviousFirstVisibleItemPosition,
					0);

			mPreviousFirstVisibleItemPosition = null;
		}

		if(mUrlsToDownload.isEmpty()) {

			if(mCommentListingManager.getCommentCount() == 0) {

				final View emptyView = LayoutInflater.from(getContext()).inflate(
						R.layout.no_comments_yet,
						mRecyclerView,
						false);

				if (mCommentListingManager.isSearchListing()) {
					((TextView) emptyView.findViewById(R.id.empty_view_text)).setText(R.string.no_search_results);
				}

				mCommentListingManager.addViewToItems(emptyView);

			} else {

				final View blankView = new View(getContext());
				blankView.setMinimumWidth(1);
				blankView.setMinimumHeight(General.dpToPixels(getContext(), 96));
				mCommentListingManager.addViewToItems(blankView);
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
			intent.putExtra(CommentReplyActivity.PARENT_ID_AND_TYPE_KEY, mPost.src.getIdAndType());
			intent.putExtra(CommentReplyActivity.PARENT_MARKDOWN_KEY, mPost.src.getUnescapedSelfText());
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
