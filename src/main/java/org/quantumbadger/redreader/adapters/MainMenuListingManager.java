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

import android.app.AlertDialog;
import android.content.ClipData;
import android.content.ClipboardManager;
import android.content.Context;
import android.content.Intent;
import android.content.res.TypedArray;
import android.graphics.drawable.Drawable;
import android.net.Uri;
import android.view.View;
import android.widget.FrameLayout;
import android.widget.Toast;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;
import androidx.core.content.ContextCompat;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.common.AndroidCommon;
import org.quantumbadger.redreader.common.Constants;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.LinkHandler;
import org.quantumbadger.redreader.common.Optional;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.common.ScreenreaderPronunciation;
import org.quantumbadger.redreader.common.SharedPrefsWrapper;
import org.quantumbadger.redreader.fragments.MainMenuFragment;
import org.quantumbadger.redreader.receivers.announcements.Announcement;
import org.quantumbadger.redreader.receivers.announcements.AnnouncementDownloader;
import org.quantumbadger.redreader.reddit.api.RedditSubredditSubscriptionManager;
import org.quantumbadger.redreader.reddit.api.SubredditSubscriptionState;
import org.quantumbadger.redreader.reddit.things.SubredditCanonicalId;
import org.quantumbadger.redreader.reddit.url.MultiredditPostListURL;
import org.quantumbadger.redreader.reddit.url.PostListingURL;
import org.quantumbadger.redreader.reddit.url.SubredditPostListURL;
import org.quantumbadger.redreader.views.AnnouncementView;
import org.quantumbadger.redreader.views.LoadingSpinnerView;
import org.quantumbadger.redreader.views.list.GroupedRecyclerViewItemListItemView;
import org.quantumbadger.redreader.views.list.GroupedRecyclerViewItemListSectionHeaderView;
import org.quantumbadger.redreader.views.liststatus.ErrorView;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.EnumSet;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicBoolean;

public class MainMenuListingManager {

	@SuppressWarnings("unused")
	private static final int GROUP_MAIN_HEADER = 0;

	private static final int GROUP_MAIN_ITEMS = 1;
	private static final int GROUP_USER_HEADER = 2;
	private static final int GROUP_USER_ITEMS = 3;
	private static final int GROUP_ANNOUNCEMENTS = 4;
	private static final int GROUP_PINNED_SUBREDDITS_HEADER = 5;
	private static final int GROUP_PINNED_SUBREDDITS_ITEMS = 6;
	private static final int GROUP_BLOCKED_SUBREDDITS_HEADER = 7;
	private static final int GROUP_BLOCKED_SUBREDDITS_ITEMS = 8;
	private static final int GROUP_MULTIREDDITS_HEADER = 9;
	private static final int GROUP_MULTIREDDITS_ITEMS = 10;
	private static final int GROUP_SUBREDDITS_HEADER = 11;
	private static final int GROUP_SUBREDDITS_ITEMS = 12;

	@NonNull private final GroupedRecyclerViewAdapter mAdapter
			= new GroupedRecyclerViewAdapter(13);
	@NonNull private final Context mContext;
	@NonNull private final AppCompatActivity mActivity;

	@NonNull private final MainMenuSelectionListener mListener;

	@Nullable private GroupedRecyclerViewAdapter.Item mMultiredditHeaderItem;

	@Nullable private ArrayList<SubredditCanonicalId> mSubredditSubscriptions;
	@Nullable private ArrayList<String> mMultiredditSubscriptions;

	@NonNull private final FrameLayout mAnnouncementHolder;

	@NonNull
	public GroupedRecyclerViewAdapter getAdapter() {
		return mAdapter;
	}

	public enum SubredditAction {
		SHARE(R.string.action_share),
		COPY_URL(R.string.action_copy_link),
		BLOCK(R.string.block_subreddit),
		UNBLOCK(R.string.unblock_subreddit),
		PIN(R.string.pin_subreddit),
		UNPIN(R.string.unpin_subreddit),
		SUBSCRIBE(R.string.options_subscribe),
		UNSUBSCRIBE(R.string.options_unsubscribe),
		EXTERNAL(R.string.action_external);

		public final int descriptionResId;

		SubredditAction(final int descriptionResId) {
			this.descriptionResId = descriptionResId;
		}
	}

	public MainMenuListingManager(
			@NonNull final AppCompatActivity activity,
			@NonNull final MainMenuSelectionListener listener,
			@NonNull final RedditAccount user) {

		General.checkThisIsUIThread();

		mActivity = activity;
		mContext = activity.getApplicationContext();
		mListener = listener;

		mAnnouncementHolder = new FrameLayout(mActivity);
		General.setLayoutMatchWidthWrapHeight(mAnnouncementHolder);

		final Drawable rrIconPerson;
		final Drawable rrIconEnvOpen;
		final Drawable rrIconSentMessages;
		final Drawable rrIconSend;
		final Drawable rrIconStarFilled;
		final Drawable rrIconCross;
		final Drawable rrIconUpvote;
		final Drawable rrIconDownvote;
		final Drawable rrIconAccountSearch;

		{
			final TypedArray attr = activity.obtainStyledAttributes(new int[] {
					R.attr.rrIconPerson,
					R.attr.rrIconEnvOpen,
					R.attr.rrIconSentMessages,
					R.attr.rrIconSend,
					R.attr.rrIconStarFilled,
					R.attr.rrIconCross,
					R.attr.rrIconArrowUpBold,
					R.attr.rrIconArrowDownBold,
					R.attr.rrIconAccountSearch
			});

			rrIconPerson = ContextCompat.getDrawable(activity, attr.getResourceId(0, 0));
			rrIconEnvOpen = ContextCompat.getDrawable(activity, attr.getResourceId(1, 0));
			rrIconSentMessages = ContextCompat.getDrawable(activity, attr.getResourceId(2,0));
			rrIconSend = ContextCompat.getDrawable(activity, attr.getResourceId(3, 0));
			rrIconStarFilled = ContextCompat.getDrawable(
					activity,
					attr.getResourceId(4, 0));
			rrIconCross = ContextCompat.getDrawable(activity, attr.getResourceId(5, 0));
			rrIconUpvote = ContextCompat.getDrawable(activity, attr.getResourceId(6, 0));
			rrIconDownvote = ContextCompat.getDrawable(
					activity,
					attr.getResourceId(7, 0));
			rrIconAccountSearch = Objects.requireNonNull(ContextCompat.getDrawable(
					activity,
					attr.getResourceId(8, 0)));

			attr.recycle();
		}

		{
			final EnumSet<MainMenuFragment.MainMenuShortcutItems> mainMenuShortcutItems
					= PrefsUtility.pref_menus_mainmenu_shortcutitems();

			if(mainMenuShortcutItems.contains(MainMenuFragment.MainMenuShortcutItems.FRONTPAGE)) {
				mAdapter.appendToGroup(
						GROUP_MAIN_ITEMS,
						makeItem(
								R.string.mainmenu_frontpage,
								MainMenuFragment.MENU_MENU_ACTION_FRONTPAGE,
								null,
								true));
			}

			if(mainMenuShortcutItems.contains(MainMenuFragment.MainMenuShortcutItems.POPULAR)) {
				mAdapter.appendToGroup(
						GROUP_MAIN_ITEMS,
						makeItem(
								R.string.mainmenu_popular,
								MainMenuFragment.MENU_MENU_ACTION_POPULAR,
								null,
								false));
			}

			if(mainMenuShortcutItems.contains(MainMenuFragment.MainMenuShortcutItems.ALL)) {
				mAdapter.appendToGroup(
						GROUP_MAIN_ITEMS,
						makeItem(
								R.string.mainmenu_all,
								MainMenuFragment.MENU_MENU_ACTION_ALL,
								null,
								false));
			}

			if(mainMenuShortcutItems.contains(
					MainMenuFragment.MainMenuShortcutItems.SUBREDDIT_SEARCH)) {

				if(mainMenuShortcutItems.contains(
						MainMenuFragment.MainMenuShortcutItems.CUSTOM)) {

					final View.OnClickListener clickListener = view -> mListener.onSelected(
							MainMenuFragment.MENU_MENU_ACTION_FIND_SUBREDDIT);

					final GroupedRecyclerViewItemListItemView item
							= new GroupedRecyclerViewItemListItemView(
									null,
									activity.getString(R.string.find_subreddit),
									null,
									false,
									clickListener,
									null,
									Optional.of(rrIconAccountSearch),
									Optional.of(view -> mListener.onSelected(
											MainMenuFragment.MENU_MENU_ACTION_CUSTOM)),
									Optional.of(activity.getString(
											R.string.mainmenu_custom_destination)));

					mAdapter.appendToGroup(GROUP_MAIN_ITEMS, item);

				} else {
					mAdapter.appendToGroup(
							GROUP_MAIN_ITEMS,
							makeItem(
									R.string.find_subreddit,
									MainMenuFragment.MENU_MENU_ACTION_FIND_SUBREDDIT,
									null,
									false));
				}

			} else if(mainMenuShortcutItems.contains(
					MainMenuFragment.MainMenuShortcutItems.CUSTOM)) {

				mAdapter.appendToGroup(
						GROUP_MAIN_ITEMS,
						makeItem(
								R.string.mainmenu_custom_destination,
								MainMenuFragment.MENU_MENU_ACTION_CUSTOM,
								null,
								false));
			}

			if(mainMenuShortcutItems.contains(MainMenuFragment.MainMenuShortcutItems.RANDOM)) {
				mAdapter.appendToGroup(
						GROUP_MAIN_ITEMS,
						makeItem(
								R.string.mainmenu_random,
								MainMenuFragment.MENU_MENU_ACTION_RANDOM,
								null,
								false));
			}

			if(mainMenuShortcutItems.contains(MainMenuFragment.MainMenuShortcutItems.RANDOM_NSFW)) {
				mAdapter.appendToGroup(
						GROUP_MAIN_ITEMS,
						makeItem(
								R.string.mainmenu_random_nsfw,
								MainMenuFragment.MENU_MENU_ACTION_RANDOM_NSFW,
								null,
								false));
			}
		}

		if(PrefsUtility.pref_menus_mainmenu_dev_announcements()) {

			mAdapter.appendToGroup(
					GROUP_ANNOUNCEMENTS,
					new GroupedRecyclerViewItemFrameLayout(mAnnouncementHolder));

			onUpdateAnnouncement();
		}

		if(!user.isAnonymous()) {

			final EnumSet<MainMenuFragment.MainMenuUserItems> mainMenuUserItems
					= PrefsUtility.pref_menus_mainmenu_useritems();

			if(!mainMenuUserItems.isEmpty()) {
				if(PrefsUtility.pref_appearance_hide_username_main_menu()) {

					mAdapter.appendToGroup(
							GROUP_USER_HEADER,
							new GroupedRecyclerViewItemListSectionHeaderView(
									activity.getString(R.string.mainmenu_useritems)));

				} else {
					mAdapter.appendToGroup(
							GROUP_USER_HEADER,
							new GroupedRecyclerViewItemListSectionHeaderView(user.username));
				}

				final AtomicBoolean isFirst = new AtomicBoolean(true);

				if(mainMenuUserItems.contains(MainMenuFragment.MainMenuUserItems.PROFILE)) {
					mAdapter.appendToGroup(
							GROUP_USER_ITEMS,
							makeItem(
									R.string.mainmenu_profile,
									MainMenuFragment.MENU_MENU_ACTION_PROFILE,
									rrIconPerson,
									isFirst.getAndSet(false)));
				}

				if(mainMenuUserItems.contains(MainMenuFragment.MainMenuUserItems.INBOX)) {
					mAdapter.appendToGroup(
							GROUP_USER_ITEMS,
							makeItem(
									R.string.mainmenu_inbox,
									MainMenuFragment.MENU_MENU_ACTION_INBOX,
									rrIconEnvOpen,
									isFirst.getAndSet(false)));
				}

				if(mainMenuUserItems.contains(MainMenuFragment.MainMenuUserItems.SENT_MESSAGES)) {
					mAdapter.appendToGroup(
							GROUP_USER_ITEMS,
							makeItem(
									R.string.mainmenu_sent_messages,
									MainMenuFragment.MENU_MENU_ACTION_SENT_MESSAGES,
									rrIconSentMessages,
									isFirst.getAndSet(false)));
				}

				if(mainMenuUserItems.contains(MainMenuFragment.MainMenuUserItems.SUBMITTED)) {
					mAdapter.appendToGroup(
							GROUP_USER_ITEMS,
							makeItem(
									R.string.mainmenu_submitted,
									MainMenuFragment.MENU_MENU_ACTION_SUBMITTED,
									rrIconSend,
									isFirst.getAndSet(false)));
				}

				if(mainMenuUserItems.contains(MainMenuFragment.MainMenuUserItems.SAVED)) {
					mAdapter.appendToGroup(
							GROUP_USER_ITEMS,
							makeItem(
									R.string.mainmenu_saved,
									MainMenuFragment.MENU_MENU_ACTION_SAVED,
									rrIconStarFilled,
									isFirst.getAndSet(false)));
				}

				if(mainMenuUserItems.contains(MainMenuFragment.MainMenuUserItems.HIDDEN)) {
					mAdapter.appendToGroup(
							GROUP_USER_ITEMS,
							makeItem(
									R.string.mainmenu_hidden,
									MainMenuFragment.MENU_MENU_ACTION_HIDDEN,
									rrIconCross,
									isFirst.getAndSet(false)));
				}

				if(mainMenuUserItems.contains(MainMenuFragment.MainMenuUserItems.UPVOTED)) {
					mAdapter.appendToGroup(
							GROUP_USER_ITEMS,
							makeItem(
									R.string.mainmenu_upvoted,
									MainMenuFragment.MENU_MENU_ACTION_UPVOTED,
									rrIconUpvote,
									isFirst.getAndSet(false)));
				}

				if(mainMenuUserItems.contains(MainMenuFragment.MainMenuUserItems.DOWNVOTED)) {
					mAdapter.appendToGroup(
							GROUP_USER_ITEMS,
							makeItem(
									R.string.mainmenu_downvoted,
									MainMenuFragment.MENU_MENU_ACTION_DOWNVOTED,
									rrIconDownvote,
									isFirst.getAndSet(false)));
				}

				if(mainMenuUserItems.contains(MainMenuFragment.MainMenuUserItems.MODMAIL)) {
					mAdapter.appendToGroup(
							GROUP_USER_ITEMS,
							makeItem(
									R.string.mainmenu_modmail,
									MainMenuFragment.MENU_MENU_ACTION_MODMAIL,
									rrIconEnvOpen,
									isFirst.getAndSet(false)));
				}
			}
		}

		setPinnedSubreddits();

		if(PrefsUtility.pref_appearance_show_blocked_subreddits_main_menu()) {

			setBlockedSubreddits();
		}

		if(!user.isAnonymous()) {
			if(PrefsUtility.pref_show_multireddit_main_menu()) {

				showMultiredditsHeader(activity);

				final LoadingSpinnerView multiredditsLoadingSpinnerView
						= new LoadingSpinnerView(activity);
				final int paddingPx = General.dpToPixels(activity, 30);
				multiredditsLoadingSpinnerView.setPadding(
						paddingPx,
						paddingPx,
						paddingPx,
						paddingPx);

				final GroupedRecyclerViewItemFrameLayout multiredditsLoadingItem
						= new GroupedRecyclerViewItemFrameLayout(
						multiredditsLoadingSpinnerView);
				mAdapter.appendToGroup(GROUP_MULTIREDDITS_ITEMS, multiredditsLoadingItem);
			}
		}

		if(PrefsUtility.pref_show_subscribed_subreddits_main_menu()) {

			mAdapter.appendToGroup(
					GROUP_SUBREDDITS_HEADER,
					new GroupedRecyclerViewItemListSectionHeaderView(
							activity.getString(R.string.mainmenu_header_subreddits_subscribed)));

			{
				final LoadingSpinnerView subredditsLoadingSpinnerView
						= new LoadingSpinnerView(activity);
				final int paddingPx = General.dpToPixels(activity, 30);
				subredditsLoadingSpinnerView.setPadding(
						paddingPx,
						paddingPx,
						paddingPx,
						paddingPx);

				final GroupedRecyclerViewItemFrameLayout subredditsLoadingItem
						= new GroupedRecyclerViewItemFrameLayout(
						subredditsLoadingSpinnerView);
				mAdapter.appendToGroup(GROUP_SUBREDDITS_ITEMS, subredditsLoadingItem);
			}
		}
	}

	private void setPinnedSubreddits() {

		final List<SubredditCanonicalId> pinnedSubreddits
				= PrefsUtility.pref_pinned_subreddits();

		mAdapter.removeAllFromGroup(GROUP_PINNED_SUBREDDITS_ITEMS);
		mAdapter.removeAllFromGroup(GROUP_PINNED_SUBREDDITS_HEADER);

		if(!pinnedSubreddits.isEmpty()) {

			final PrefsUtility.PinnedSubredditSort pinnedSubredditsSort
					= PrefsUtility.pref_behaviour_pinned_subredditsort();

			mAdapter.appendToGroup(
					GROUP_PINNED_SUBREDDITS_HEADER,
					new GroupedRecyclerViewItemListSectionHeaderView(
							mActivity.getString(R.string.mainmenu_header_subreddits_pinned)));

			if(pinnedSubredditsSort == PrefsUtility.PinnedSubredditSort.NAME) {
				Collections.sort(pinnedSubreddits);
			}

			boolean isFirst = true;

			for(final SubredditCanonicalId sr : pinnedSubreddits) {
				mAdapter.appendToGroup(
						GROUP_PINNED_SUBREDDITS_ITEMS,
						makeSubredditItem(sr, isFirst, true));
				isFirst = false;
			}
		}
	}

	private void setBlockedSubreddits() {

		final List<SubredditCanonicalId> blockedSubreddits
				= PrefsUtility.pref_blocked_subreddits();

		mAdapter.removeAllFromGroup(GROUP_BLOCKED_SUBREDDITS_ITEMS);
		mAdapter.removeAllFromGroup(GROUP_BLOCKED_SUBREDDITS_HEADER);

		if(!blockedSubreddits.isEmpty()) {

			final PrefsUtility.BlockedSubredditSort blockedSubredditsSort
					= PrefsUtility.pref_behaviour_blocked_subredditsort();

			mAdapter.appendToGroup(
					GROUP_BLOCKED_SUBREDDITS_HEADER,
					new GroupedRecyclerViewItemListSectionHeaderView(
							mActivity.getString(R.string.mainmenu_header_subreddits_blocked)));

			if(blockedSubredditsSort == PrefsUtility.BlockedSubredditSort.NAME) {
				Collections.sort(blockedSubreddits);
			}

			boolean isFirst = true;
			for(final SubredditCanonicalId sr : blockedSubreddits) {
				mAdapter.appendToGroup(
						GROUP_BLOCKED_SUBREDDITS_ITEMS,
						makeSubredditItem(sr, isFirst, true));
				isFirst = false;
			}
		}
	}


	private void showMultiredditsHeader(@NonNull final Context context) {

		General.checkThisIsUIThread();

		if(mMultiredditHeaderItem == null) {
			mMultiredditHeaderItem = new GroupedRecyclerViewItemListSectionHeaderView(
					context.getString(R.string.mainmenu_header_multireddits));

			mAdapter.appendToGroup(GROUP_MULTIREDDITS_HEADER, mMultiredditHeaderItem);
		}
	}

	private void hideMultiredditsHeader() {

		General.checkThisIsUIThread();

		mMultiredditHeaderItem = null;
		mAdapter.removeAllFromGroup(GROUP_MULTIREDDITS_HEADER);
	}

	public void setMultiredditsError(final ErrorView errorView) {

		AndroidCommon.UI_THREAD_HANDLER.post(() -> {

			mAdapter.removeAllFromGroup(GROUP_MULTIREDDITS_ITEMS);
			mAdapter.appendToGroup(
					GROUP_MULTIREDDITS_ITEMS,
					new GroupedRecyclerViewItemFrameLayout(errorView));
		});
	}

	public void setSubredditsError(final ErrorView errorView) {

		AndroidCommon.UI_THREAD_HANDLER.post(() -> {

			mAdapter.removeAllFromGroup(GROUP_SUBREDDITS_ITEMS);
			mAdapter.appendToGroup(
					GROUP_SUBREDDITS_ITEMS,
					new GroupedRecyclerViewItemFrameLayout(errorView));
		});
	}

	public void setSubreddits(final Collection<SubredditCanonicalId> subscriptions) {

		final ArrayList<SubredditCanonicalId> subscriptionsSorted = new ArrayList<>(
				subscriptions);
		Collections.sort(subscriptionsSorted);

		AndroidCommon.UI_THREAD_HANDLER.post(() -> {

			if(mSubredditSubscriptions != null
					&& mSubredditSubscriptions.equals(subscriptionsSorted)) {

				return;
			}

			if(!PrefsUtility.pref_show_subscribed_subreddits_main_menu()) {
				mAdapter.removeAllFromGroup(GROUP_SUBREDDITS_HEADER);
				mAdapter.removeAllFromGroup(GROUP_SUBREDDITS_ITEMS);
				return;
			}

			mSubredditSubscriptions = subscriptionsSorted;

			mAdapter.removeAllFromGroup(GROUP_SUBREDDITS_ITEMS);

			boolean isFirst = true;

			for(final SubredditCanonicalId subreddit : subscriptionsSorted) {

				mAdapter.appendToGroup(
						GROUP_SUBREDDITS_ITEMS,
						makeSubredditItem(subreddit, isFirst, false));

				isFirst = false;
			}
		});
	}

	public void setMultireddits(final Collection<String> subscriptions) {

		final ArrayList<String> subscriptionsSorted = new ArrayList<>(subscriptions);
		Collections.sort(subscriptionsSorted);

		AndroidCommon.UI_THREAD_HANDLER.post(() -> {

			if(mMultiredditSubscriptions != null
					&& mMultiredditSubscriptions.equals(subscriptionsSorted)) {

				return;
			}

			if(!PrefsUtility.pref_show_multireddit_main_menu()) {
				mAdapter.removeAllFromGroup(GROUP_MULTIREDDITS_HEADER);
				mAdapter.removeAllFromGroup(GROUP_MULTIREDDITS_ITEMS);
				return;
			}

			mMultiredditSubscriptions = subscriptionsSorted;

			mAdapter.removeAllFromGroup(GROUP_MULTIREDDITS_ITEMS);

			if(subscriptionsSorted.isEmpty()) {
				hideMultiredditsHeader();

			} else {

				showMultiredditsHeader(mContext);

				boolean isFirst = true;

				for(final String multireddit : subscriptionsSorted) {

					final GroupedRecyclerViewItemListItemView item
							= makeMultiredditItem(multireddit, isFirst);
					mAdapter.appendToGroup(GROUP_MULTIREDDITS_ITEMS, item);

					isFirst = false;
				}
			}
		});
	}

	private GroupedRecyclerViewItemListItemView makeItem(
			final int nameRes,
			final @MainMenuFragment.MainMenuAction int action,
			@Nullable final Drawable icon,
			final boolean hideDivider) {

		return makeItem(mContext.getString(nameRes), action, icon, hideDivider);
	}

	private GroupedRecyclerViewItemListItemView makeItem(
			@NonNull final String name,
			final @MainMenuFragment.MainMenuAction int action,
			@Nullable final Drawable icon,
			final boolean hideDivider) {

		final View.OnClickListener clickListener = view -> mListener.onSelected(action);

		return new GroupedRecyclerViewItemListItemView(
				icon,
				name,
				null,
				hideDivider,
				clickListener,
				null,
				Optional.empty(),
				Optional.empty(),
				Optional.empty());
	}

	private GroupedRecyclerViewItemListItemView makeSubredditItem(
			final SubredditCanonicalId subreddit,
			final boolean hideDivider,
			final boolean showRSlashPrefix) {

		final View.OnClickListener clickListener = view -> {

			if(subreddit.toString().startsWith("/r/")) {
				mListener.onSelected((PostListingURL)SubredditPostListURL.getSubreddit(
						subreddit));

			} else {
				LinkHandler.onLinkClicked(mActivity, subreddit.toString());
			}
		};

		final View.OnLongClickListener longClickListener = view -> {
			showActionMenu(mActivity, subreddit);
			return true;
		};

		final String displayName = showRSlashPrefix
				? subreddit.toString()
				: subreddit.getDisplayNameLowercase();

		return new GroupedRecyclerViewItemListItemView(
				null,
				displayName,
				ScreenreaderPronunciation.getPronunciation(mContext, displayName),
				hideDivider,
				clickListener,
				longClickListener,
				Optional.empty(),
				Optional.empty(),
				Optional.empty());
	}

	public static void showActionMenu(
			final AppCompatActivity activity,
			final SubredditCanonicalId subreddit) {
		final EnumSet<SubredditAction> itemPref
				= PrefsUtility.pref_menus_subreddit_context_items();

		if(itemPref.isEmpty()) {
			return;
		}

		final ArrayList<SubredditMenuItem> menu = new ArrayList<>();
		if(itemPref.contains(SubredditAction.COPY_URL)) {
			menu.add(new SubredditMenuItem(
					activity,
					R.string.action_copy_link,
					SubredditAction.COPY_URL));
		}
		if(itemPref.contains(SubredditAction.EXTERNAL)) {
			menu.add(new SubredditMenuItem(
					activity,
					R.string.action_external,
					SubredditAction.EXTERNAL));
		}
		if(itemPref.contains(SubredditAction.SHARE)) {
			menu.add(new SubredditMenuItem(
					activity,
					R.string.action_share,
					SubredditAction.SHARE));
		}

		if(itemPref.contains(SubredditAction.BLOCK)) {

			final boolean isBlocked = PrefsUtility.pref_blocked_subreddits_check(subreddit);

			if(isBlocked) {
				menu.add(new SubredditMenuItem(
						activity,
						R.string.unblock_subreddit,
						SubredditAction.UNBLOCK));
			} else {
				menu.add(new SubredditMenuItem(
						activity,
						R.string.block_subreddit,
						SubredditAction.BLOCK));
			}
		}

		if(itemPref.contains(SubredditAction.PIN)) {

			final boolean isPinned = PrefsUtility.pref_pinned_subreddits_check(subreddit);

			if(isPinned) {
				menu.add(new SubredditMenuItem(
						activity,
						R.string.unpin_subreddit,
						SubredditAction.UNPIN));
			} else {
				menu.add(new SubredditMenuItem(
						activity,
						R.string.pin_subreddit,
						SubredditAction.PIN));
			}
		}

		if(!RedditAccountManager.getInstance(activity)
				.getDefaultAccount()
				.isAnonymous()) {

			if(itemPref.contains(SubredditAction.SUBSCRIBE)) {

				final RedditSubredditSubscriptionManager subscriptionManager
						= RedditSubredditSubscriptionManager
						.getSingleton(
								activity,
								RedditAccountManager.getInstance(activity)
										.getDefaultAccount());

				if(subscriptionManager.areSubscriptionsReady()) {
					if(subscriptionManager.getSubscriptionState(subreddit)
							== SubredditSubscriptionState.SUBSCRIBED) {
						menu.add(new SubredditMenuItem(
								activity,
								R.string.options_unsubscribe,
								SubredditAction.UNSUBSCRIBE));
					} else {
						menu.add(new SubredditMenuItem(
								activity,
								R.string.options_subscribe,
								SubredditAction.SUBSCRIBE));
					}
				}
			}
		}

		final String[] menuText = new String[menu.size()];

		for(int i = 0; i < menuText.length; i++) {
			menuText[i] = menu.get(i).title;
		}

		final AlertDialog.Builder builder = new AlertDialog.Builder(activity);

		builder.setItems(menuText, (dialog, which) -> onSubredditActionMenuItemSelected(
				subreddit,
				activity,
				menu.get(which).action));

		final AlertDialog alert = builder.create();
		alert.setCanceledOnTouchOutside(true);
		alert.show();
	}

	private static void onSubredditActionMenuItemSelected(
			final SubredditCanonicalId subredditCanonicalId,
			final AppCompatActivity activity,
			final SubredditAction action) {

		final String url = Constants.Reddit.getNonAPIUri(subredditCanonicalId.toString())
				.toString();

		final RedditSubredditSubscriptionManager subMan = RedditSubredditSubscriptionManager
				.getSingleton(
						activity,
						RedditAccountManager.getInstance(
								activity)
								.getDefaultAccount());

		switch(action) {
			case SHARE: {
				LinkHandler.shareText(activity, subredditCanonicalId.toString(), url);
				break;
			}

			case COPY_URL: {
				final ClipboardManager clipboardManager
						= (ClipboardManager)activity.getSystemService(Context.CLIPBOARD_SERVICE);
				if(clipboardManager != null) {
					final ClipData data = ClipData.newPlainText(null, url);
					clipboardManager.setPrimaryClip(data);

					General.quickToast(
							activity.getApplicationContext(),
							R.string.subreddit_link_copied_to_clipboard);
				}
				break;
			}

			case EXTERNAL: {
				final Intent intent = new Intent(Intent.ACTION_VIEW);
				intent.setData(Uri.parse(url));
				activity.startActivity(intent);
				break;
			}

			case PIN:
				PrefsUtility.pref_pinned_subreddits_add(
						activity,
						subredditCanonicalId);
				break;

			case UNPIN:
				PrefsUtility.pref_pinned_subreddits_remove(
						activity,
						subredditCanonicalId);
				break;

			case BLOCK:
				PrefsUtility.pref_blocked_subreddits_add(
						activity,
						subredditCanonicalId);
				break;

			case UNBLOCK:
				PrefsUtility.pref_blocked_subreddits_remove(
						activity,
						subredditCanonicalId);
				break;

			case SUBSCRIBE:

				if(subMan.getSubscriptionState(subredditCanonicalId)
						== SubredditSubscriptionState.NOT_SUBSCRIBED) {
					subMan.subscribe(subredditCanonicalId, activity);
					Toast.makeText(
							activity,
							R.string.options_subscribing,
							Toast.LENGTH_SHORT).show();
				} else {
					Toast.makeText(
							activity,
							R.string.mainmenu_toast_subscribed,
							Toast.LENGTH_SHORT).show();
				}
				break;

			case UNSUBSCRIBE:

				if(subMan.getSubscriptionState(subredditCanonicalId)
						== SubredditSubscriptionState.SUBSCRIBED) {
					subMan.unsubscribe(subredditCanonicalId, activity);
					Toast.makeText(
							activity,
							R.string.options_unsubscribing,
							Toast.LENGTH_SHORT).show();
				} else {
					Toast.makeText(
							activity,
							R.string.mainmenu_toast_not_subscribed,
							Toast.LENGTH_SHORT).show();
				}
				break;
		}
	}

	private GroupedRecyclerViewItemListItemView makeMultiredditItem(
			final String name,
			final boolean hideDivider) {

		final View.OnClickListener clickListener = view -> mListener.onSelected(
				(PostListingURL)MultiredditPostListURL.getMultireddit(name));

		return new GroupedRecyclerViewItemListItemView(
				null,
				name,
				ScreenreaderPronunciation.getPronunciation(mContext, name),
				hideDivider,
				clickListener,
				null,
				Optional.empty(),
				Optional.empty(),
				Optional.empty());
	}

	private static class SubredditMenuItem {
		public final String title;
		public final SubredditAction action;

		private SubredditMenuItem(
				final Context context,
				final int titleRes,
				final SubredditAction action) {
			this.title = context.getString(titleRes);
			this.action = action;
		}
	}

	public void onUpdateAnnouncement() {

		final SharedPrefsWrapper sharedPreferences = General.getSharedPrefs(mContext);

		if(PrefsUtility.pref_menus_mainmenu_dev_announcements()) {

			final Optional<Announcement> announcement
					= AnnouncementDownloader.getMostRecentUnreadAnnouncement(sharedPreferences);

			if(announcement.isPresent()) {
				mAnnouncementHolder.removeAllViews();
				mAnnouncementHolder.addView(new AnnouncementView(mActivity, announcement.get()));
			}
		}
	}
}
