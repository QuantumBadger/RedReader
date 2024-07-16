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
import android.graphics.Color;
import android.os.Bundle;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.view.animation.Animation;
import android.view.animation.AnimationUtils;
import android.view.animation.OvershootInterpolator;
import android.widget.FrameLayout;
import android.widget.ImageButton;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.activities.BaseActivity;
import org.quantumbadger.redreader.activities.OptionsMenuUtility;
import org.quantumbadger.redreader.adapters.FilteredCommentListingManager;
import org.quantumbadger.redreader.adapters.GroupedRecyclerViewAdapter;
import org.quantumbadger.redreader.cache.downloadstrategy.DownloadStrategy;
import org.quantumbadger.redreader.cache.downloadstrategy.DownloadStrategyAlways;
import org.quantumbadger.redreader.cache.downloadstrategy.DownloadStrategyIfNotCached;
import org.quantumbadger.redreader.cache.downloadstrategy.DownloadStrategyIfTimestampOutsideBounds;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.common.RRError;
import org.quantumbadger.redreader.common.RRThemeAttributes;
import org.quantumbadger.redreader.common.TimestampBound;
import org.quantumbadger.redreader.common.time.TimeDuration;
import org.quantumbadger.redreader.common.time.TimestampUTC;
import org.quantumbadger.redreader.reddit.CommentListingRequest;
import org.quantumbadger.redreader.reddit.RedditCommentListItem;
import org.quantumbadger.redreader.reddit.api.RedditAPICommentAction;
import org.quantumbadger.redreader.reddit.api.RedditPostActions;
import org.quantumbadger.redreader.reddit.prepared.RedditChangeDataManager;
import org.quantumbadger.redreader.reddit.prepared.RedditPreparedPost;
import org.quantumbadger.redreader.reddit.prepared.RedditRenderableComment;
import org.quantumbadger.redreader.reddit.url.RedditURLParser;
import org.quantumbadger.redreader.views.AccessibilityActionManager;
import org.quantumbadger.redreader.views.RedditCommentView;
import org.quantumbadger.redreader.views.RedditPostHeaderView;
import org.quantumbadger.redreader.views.RedditPostView;
import org.quantumbadger.redreader.views.ScrollbarRecyclerViewManager;
import org.quantumbadger.redreader.views.bezelmenu.BezelSwipeOverlay;
import org.quantumbadger.redreader.views.bezelmenu.SideToolbarOverlay;
import org.quantumbadger.redreader.views.liststatus.CommentSubThreadView;
import org.quantumbadger.redreader.views.liststatus.ErrorView;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.Map;
import java.util.UUID;

public class CommentListingFragment extends RRFragment
		implements RedditPostView.PostSelectionListener,
		RedditCommentView.CommentListener,
		CommentListingRequest.Listener {

	private static final String SAVEDSTATE_FIRST_VISIBLE_POS = "firstVisiblePosition";
	private static final String SAVEDSTATE_SELFTEXT_VISIBLE = "selftextVisible";

	private final RedditAccount mUser;
	private final ArrayList<RedditURLParser.RedditURL> mAllUrls;
	private final LinkedList<RedditURLParser.RedditURL> mUrlsToDownload;
	private final UUID mSession;
	private final DownloadStrategy mDownloadStrategy;

	private RedditPreparedPost mPost = null;

	private boolean mSelfTextVisible = true;

	private final FilteredCommentListingManager mCommentListingManager;

	private final RecyclerView mRecyclerView;

	private final View mListingView;
	private final FrameLayout mOverlayFrame;
	private final @Nullable LinearLayout mFloatingToolbar;

	private final float mSelfTextFontScale;
	private final boolean mShowLinkButtons;

	private TimestampUTC mCachedTimestamp = null;

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
			mPreviousFirstVisibleItemPosition = savedInstanceState.getInt(
					SAVEDSTATE_FIRST_VISIBLE_POS);

			if(savedInstanceState.containsKey(SAVEDSTATE_SELFTEXT_VISIBLE)) {
				mSelfTextVisible = savedInstanceState.getBoolean(SAVEDSTATE_SELFTEXT_VISIBLE);
			}
		}

		mCommentListingManager = new FilteredCommentListingManager(parent, searchString);
		mAllUrls = urls;

		mUrlsToDownload = new LinkedList<>(mAllUrls);

		this.mSession = session;

		if(forceDownload) {
			mDownloadStrategy = DownloadStrategyAlways.INSTANCE;

		} else if(session == null
				&& savedInstanceState == null
				&& General.isNetworkConnected(parent)) {
			mDownloadStrategy = new DownloadStrategyIfTimestampOutsideBounds(
					TimestampBound.notOlderThan(TimeDuration.minutes(20)));

		} else {
			mDownloadStrategy = DownloadStrategyIfNotCached.INSTANCE;
		}

		mUser = RedditAccountManager.getInstance(getActivity()).getDefaultAccount();

		parent.invalidateOptionsMenu();

		final Context context = getActivity();

		mSelfTextFontScale = PrefsUtility.appearance_fontscale_bodytext();

		mShowLinkButtons = PrefsUtility.pref_appearance_linkbuttons();

		mOverlayFrame = new FrameLayout(context);

		final ScrollbarRecyclerViewManager recyclerViewManager
				= new ScrollbarRecyclerViewManager(context, null, false);

		if(parent instanceof OptionsMenuUtility.OptionsMenuCommentsListener
				&& PrefsUtility.pref_behaviour_enable_swipe_refresh()) {

			recyclerViewManager.enablePullToRefresh(
					((OptionsMenuUtility.OptionsMenuCommentsListener)parent)::onRefreshComments);
		}

		mRecyclerView = recyclerViewManager.getRecyclerView();
		mCommentListingManager.setLayoutManager(
				(LinearLayoutManager)mRecyclerView.getLayoutManager());

		mRecyclerView.setAdapter(mCommentListingManager.getAdapter());
		mListingView = recyclerViewManager.getOuterView();

		mRecyclerView.setItemAnimator(null);

		if(!PrefsUtility.pref_appearance_comments_show_floating_toolbar()) {
			mFloatingToolbar = null;

		} else {
			mFloatingToolbar = (LinearLayout)LayoutInflater.from(context).inflate(
					R.layout.floating_toolbar,
					mOverlayFrame,
					false);

			if (PrefsUtility.pref_appearance_left_handed()) {
				final FrameLayout.LayoutParams toolBarParams =
						(FrameLayout.LayoutParams) mFloatingToolbar.getLayoutParams();
				toolBarParams.gravity = Gravity.START | Gravity.BOTTOM;
				mFloatingToolbar.setLayoutParams(toolBarParams);
			}

			// We need a container so that setVisible() doesn't mess with the Z-order
			final FrameLayout floatingToolbarContainer = new FrameLayout(context);

			floatingToolbarContainer.addView(mFloatingToolbar);
			mOverlayFrame.addView(floatingToolbarContainer);

			if(PrefsUtility.isNightMode()) {
				mFloatingToolbar.setBackgroundColor(Color.argb(0xCC, 0x33, 0x33, 0x33));
			}

			final int buttonVPadding = General.dpToPixels(context, 12);
			final int buttonHPadding = General.dpToPixels(context, 16);

			{
				final ImageButton previousButton = (ImageButton)LayoutInflater.from(
						context).inflate(
						R.layout.flat_image_button, mFloatingToolbar, false);

				previousButton.setPadding(
						buttonHPadding,
						buttonVPadding,
						buttonHPadding,
						buttonVPadding);
				previousButton.setImageResource(R.drawable.ic_ff_up_dark);
				previousButton.setContentDescription(
						getString(R.string.button_prev_comment_parent));
				mFloatingToolbar.addView(previousButton);

				previousButton.setOnClickListener(view -> {

					final LinearLayoutManager layoutManager
							= (LinearLayoutManager)mRecyclerView.getLayoutManager();

					for(int pos = layoutManager.findFirstVisibleItemPosition() - 1;
						pos > 0;
						pos--) {

						final GroupedRecyclerViewAdapter.Item item
								= mCommentListingManager.getItemAtPosition(pos);

						if(item instanceof RedditCommentListItem
								&& ((RedditCommentListItem)item).isComment()
								&& ((RedditCommentListItem)item).getIndent() == 0) {

							layoutManager.scrollToPositionWithOffset(pos, 0);
							return;
						}
					}

					layoutManager.scrollToPositionWithOffset(0, 0);
				});

				previousButton.setOnLongClickListener(view -> {
					General.quickToast(context, R.string.button_prev_comment_parent);
					return true;
				});
			}

			{
				final ImageButton nextButton = (ImageButton)LayoutInflater.from(context)
						.inflate(
								R.layout.flat_image_button,
								mFloatingToolbar,
								false);

				nextButton.setPadding(
						buttonHPadding,
						buttonVPadding,
						buttonHPadding,
						buttonVPadding);
				nextButton.setImageResource(R.drawable.ic_ff_down_dark);
				nextButton.setContentDescription(getString(R.string.button_next_comment_parent));
				mFloatingToolbar.addView(nextButton);

				nextButton.setOnClickListener(view -> {

					final LinearLayoutManager layoutManager
							= (LinearLayoutManager)mRecyclerView.getLayoutManager();

					for(int pos = layoutManager.findFirstVisibleItemPosition() + 1;
						pos < layoutManager.getItemCount();
						pos++) {

						final GroupedRecyclerViewAdapter.Item item
								= mCommentListingManager.getItemAtPosition(pos);

						if(item instanceof RedditCommentListItem
								&& ((RedditCommentListItem)item).isComment()
								&& ((RedditCommentListItem)item).getIndent() == 0) {

							layoutManager.scrollToPositionWithOffset(pos, 0);
							break;
						}
					}
				});

				nextButton.setOnLongClickListener(view -> {
					General.quickToast(context, R.string.button_next_comment_parent);
					return true;
				});
			}
		}

		final SideToolbarOverlay toolbarOverlay = new SideToolbarOverlay(context);

		final BezelSwipeOverlay bezelOverlay = new BezelSwipeOverlay(
				context,
				new BezelSwipeOverlay.BezelSwipeListener() {
					@Override
					public boolean onSwipe(@BezelSwipeOverlay.SwipeEdge final int edge) {

						if(mPost == null) {
							return false;
						}

						toolbarOverlay.setContents(RedditPostActions.INSTANCE.generateToolbar(
								mPost,
								(BaseActivity)getActivity(),
								true,
								toolbarOverlay));
						toolbarOverlay.show(edge == BezelSwipeOverlay.LEFT
								?
								SideToolbarOverlay.SideToolbarPosition.LEFT
								: SideToolbarOverlay.SideToolbarPosition.RIGHT);
						return true;
					}

					@Override
					public boolean onTap() {

						if(toolbarOverlay.isShown()) {
							toolbarOverlay.hide();
							return true;
						}

						return false;
					}
				});

		mOverlayFrame.addView(bezelOverlay);
		mOverlayFrame.addView(toolbarOverlay);

		General.setLayoutMatchParent(bezelOverlay);
		General.setLayoutMatchParent(toolbarOverlay);

		makeNextRequest(context);
	}

	public void handleCommentVisibilityToggle(final RedditCommentView view) {

		final RedditChangeDataManager changeDataManager
				= RedditChangeDataManager.getInstance(mUser);
		final RedditCommentListItem item = view.getComment();

		if(item.isComment()) {
			final RedditRenderableComment comment = item.asComment();

			changeDataManager.markHidden(
					TimestampUTC.now(),
					comment.getIdAndType(),
					!comment.isCollapsed(changeDataManager));

			mCommentListingManager.updateHiddenStatus();

			final LinearLayoutManager layoutManager
					= (LinearLayoutManager)mRecyclerView.getLayoutManager();
			final int position = layoutManager.getPosition(view);

			if(position == layoutManager.findFirstVisibleItemPosition()) {
				layoutManager.scrollToPositionWithOffset(position, 0);
			}
		}
	}

	@Override
	public View getListingView() {
		return mListingView;
	}

	@Nullable
	@Override
	public View getOverlayView() {
		return mOverlayFrame;
	}

	public RedditPreparedPost getPost() {
		return mPost;
	}

	@Override
	public Bundle onSaveInstanceState() {

		final Bundle bundle = new Bundle();

		final LinearLayoutManager layoutManager
				= (LinearLayoutManager)mRecyclerView.getLayoutManager();
		bundle.putInt(
				SAVEDSTATE_FIRST_VISIBLE_POS,
				layoutManager.findFirstVisibleItemPosition());

		if(mPost != null && mPost.isSelf()) {
			bundle.putBoolean(SAVEDSTATE_SELFTEXT_VISIBLE, mSelfTextVisible);
		}

		return bundle;
	}

	@SuppressLint("WrongConstant")
	private void makeNextRequest(final Context context) {

		if(!mUrlsToDownload.isEmpty()) {
			new CommentListingRequest(
					context,
					this,
					(BaseActivity)getActivity(),
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
		switch(PrefsUtility.pref_behaviour_actions_comment_tap()) {

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
							mPost != null && mPost.isLocked);
				}
				break;
			}
		}
	}

	@Override
	public void onCommentLongClicked(final RedditCommentView view) {
		switch(PrefsUtility.pref_behaviour_actions_comment_longclick()) {

			case ACTION_MENU: {
				final RedditCommentListItem item = view.getComment();
				if(item != null && item.isComment()) {
					RedditAPICommentAction.showActionMenu(
							getActivity(),
							this,
							item.asComment(),
							view,
							RedditChangeDataManager.getInstance(mUser),
							mPost != null && mPost.isLocked);
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
	public void onCommentListingRequestFailure(final RRError error) {
		mCommentListingManager.setLoadingVisible(false);
		mCommentListingManager.addFooterError(new ErrorView(getActivity(), error));
	}

	@Override
	public void onCommentListingRequestCachedCopy(final TimestampUTC timestamp) {
		mCachedTimestamp = timestamp;
	}

	@Override
	public void onCommentListingRequestParseStart() {
		mCommentListingManager.setLoadingVisible(true);
	}

	@Override
	public void onCommentListingRequestPostDownloaded(final RedditPreparedPost post) {

		final BaseActivity activity = (BaseActivity)getActivity();

		if(mPost == null) {

			final RRThemeAttributes attr = new RRThemeAttributes(activity);

			mPost = post;

			// Invalidate the options menu, so the suggested sort will be shown if needed.
			activity.invalidateOptionsMenu();

			final RedditPostHeaderView postHeader = new RedditPostHeaderView(
					activity,
					this.mPost);

			mCommentListingManager.addPostHeader(postHeader);

			final LinearLayoutManager layoutManager
					= (LinearLayoutManager)mRecyclerView.getLayoutManager();
			layoutManager.scrollToPositionWithOffset(0, 0);

			if(post.src.getSelfText() != null) {
				final View selfText = post.src.getSelfText().generateView(
						activity,
						attr.rrMainTextCol,
						13f * mSelfTextFontScale,
						mShowLinkButtons);
				selfText.setFocusable(false);

				if(selfText instanceof ViewGroup) {
					((ViewGroup)selfText).setDescendantFocusability(
							ViewGroup.FOCUS_BLOCK_DESCENDANTS);
				}

				final int paddingPx = General.dpToPixels(activity, 10);
				final FrameLayout paddingLayout = new FrameLayout(activity);
				final TextView collapsedView = new TextView(activity);
				//noinspection SetTextI18n
				collapsedView.setText("[ + ]  "
						+ activity.getString(R.string.collapsed_self_post));
				collapsedView.setVisibility(View.GONE);
				collapsedView.setPadding(paddingPx, paddingPx, paddingPx, paddingPx);
				paddingLayout.addView(selfText);
				paddingLayout.addView(collapsedView);
				paddingLayout.setPadding(paddingPx, paddingPx, paddingPx, paddingPx);

				final PrefsUtility.SelfpostAction actionOnClick
						= PrefsUtility.pref_behaviour_self_post_tap_actions();
				if(actionOnClick == PrefsUtility.SelfpostAction.COLLAPSE) {
					paddingLayout.setOnClickListener(v -> {
						if(selfText.getVisibility() == View.GONE) {
							mSelfTextVisible = true;
							selfText.setVisibility(View.VISIBLE);
							collapsedView.setVisibility(View.GONE);
						} else {
							mSelfTextVisible = false;
							selfText.setVisibility(View.GONE);
							collapsedView.setVisibility(View.VISIBLE);
							layoutManager.scrollToPositionWithOffset(0, 0);
						}
					});
				}

				if(!mSelfTextVisible) {
					selfText.setVisibility(View.GONE);
					collapsedView.setVisibility(View.VISIBLE);
					layoutManager.scrollToPositionWithOffset(0, 0);
				}

				paddingLayout.setOnLongClickListener(v -> {
					RedditPostActions.INSTANCE.showActionMenu(activity, mPost);
					return true;
				});

				RedditPostActions.INSTANCE.setupAccessibilityActions(
						new AccessibilityActionManager(
								paddingLayout,
								activity.getResources()),
						post,
						activity,
						true);

				mCommentListingManager.addPostSelfText(paddingLayout);
			}

			if(!General.isTablet(activity)) {
				activity.setTitle(post.src.getTitle());
			}

			if(mCommentListingManager.isSearchListing()) {
				final CommentSubThreadView searchCommentThreadView
						= new CommentSubThreadView(
						activity,
						mAllUrls.get(0).asPostCommentListURL(),
						R.string.comment_header_search_thread_title
				);

				mCommentListingManager.addNotification(searchCommentThreadView);
			} else if(!mAllUrls.isEmpty()
					&& mAllUrls.get(0).pathType() == RedditURLParser.POST_COMMENT_LISTING_URL
					&& mAllUrls.get(0).asPostCommentListURL().commentId != null) {

				final CommentSubThreadView specificCommentThreadView
						= new CommentSubThreadView(
						activity,
						mAllUrls.get(0).asPostCommentListURL(),
						R.string.comment_header_specific_thread_title);

				mCommentListingManager.addNotification(specificCommentThreadView);
			}

			// 30 minutes
			if(mCachedTimestamp != null) {
				if (mCachedTimestamp.elapsed().isGreaterThan(TimeDuration.minutes(30))) {

					final TextView cacheNotif = (TextView) LayoutInflater.from(activity).inflate(
							R.layout.cached_header,
							null,
							false);
					cacheNotif.setText(activity.getString(
							R.string.listing_cached,
							mCachedTimestamp.format()));
					mCommentListingManager.addNotification(cacheNotif);
				}
			}
		}
	}

	@Override
	public void onCommentListingRequestAllItemsDownloaded(
			final ArrayList<RedditCommentListItem> items) {

		mCommentListingManager.addComments(items);

		if(mFloatingToolbar != null && mFloatingToolbar.getVisibility() != View.VISIBLE) {
			mFloatingToolbar.setVisibility(View.VISIBLE);
			final Animation animation = AnimationUtils.loadAnimation(
					getContext(),
					R.anim.slide_in_from_bottom);
			animation.setInterpolator(new OvershootInterpolator());
			mFloatingToolbar.startAnimation(animation);
		}

		mUrlsToDownload.removeFirst();

		final LinearLayoutManager layoutManager
				= (LinearLayoutManager)mRecyclerView.getLayoutManager();

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
						R.layout.no_items_yet,
						mRecyclerView,
						false);

				if(mCommentListingManager.isSearchListing()) {
					((TextView)emptyView.findViewById(R.id.empty_view_text))
							.setText(R.string.no_search_results);
				} else {
					((TextView)emptyView.findViewById(R.id.empty_view_text))
							.setText(R.string.no_comments_yet);
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
	public void onCreateOptionsMenu(final Menu menu) {

		final Map<OptionsMenuUtility.AppbarItemsPref, Integer> appbarItemsPrefs =
				PrefsUtility.pref_menus_appbar_items();
		final int replyShowAsAction = OptionsMenuUtility.getOrThrow(
				appbarItemsPrefs,
				OptionsMenuUtility.AppbarItemsPref.REPLY);

		if(mAllUrls != null
				&& !mAllUrls.isEmpty()
				&& mAllUrls.get(0).pathType() == RedditURLParser.POST_COMMENT_LISTING_URL
				&& replyShowAsAction != OptionsMenuUtility.DO_NOT_SHOW) {
			final MenuItem reply = menu.add(
					Menu.NONE,
					OptionsMenuUtility.AppbarItemsPref.REPLY.ordinal(),
					Menu.NONE,
					R.string.action_reply);

			reply.setShowAsAction(OptionsMenuUtility.handleShowAsActionIfRoom(
					replyShowAsAction));
			reply.setIcon(R.drawable.ic_action_reply_dark);

			OptionsMenuUtility.pruneMenu(getActivity(), menu, appbarItemsPrefs, true);
		}
	}

	@Override
	public boolean onOptionsItemSelected(final MenuItem item) {

		if(item.getTitle() != null
				&& item.getTitle()
				.equals(getActivity().getString(R.string.action_reply))) {

			RedditPostActions.INSTANCE.onActionMenuItemSelected(
					mPost,
					(BaseActivity)getActivity(),
					RedditPostActions.Action.REPLY);
			return true;
		}

		return false;
	}

	@Override
	public void onPostSelected(final RedditPreparedPost post) {
		((RedditPostView.PostSelectionListener)getActivity()).onPostSelected(post);
	}

	@Override
	public void onPostCommentsSelected(final RedditPreparedPost post) {
		((RedditPostView.PostSelectionListener)getActivity()).onPostCommentsSelected(post);
	}
}
