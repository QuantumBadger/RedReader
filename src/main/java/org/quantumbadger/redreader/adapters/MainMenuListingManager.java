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
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.SharedPreferences;
import android.content.res.TypedArray;
import android.graphics.drawable.Drawable;
import android.net.Uri;
import android.preference.PreferenceManager;
import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.support.v4.content.ContextCompat;
import android.support.v7.app.AppCompatActivity;
import android.text.ClipboardManager;
import android.view.View;
import android.widget.Toast;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.common.AndroidCommon;
import org.quantumbadger.redreader.common.Constants;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.LinkHandler;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.fragments.MainMenuFragment;
import org.quantumbadger.redreader.reddit.api.RedditSubredditSubscriptionManager;
import org.quantumbadger.redreader.reddit.things.RedditSubreddit;
import org.quantumbadger.redreader.reddit.url.MultiredditPostListURL;
import org.quantumbadger.redreader.reddit.url.PostListingURL;
import org.quantumbadger.redreader.reddit.url.SubredditPostListURL;
import org.quantumbadger.redreader.views.LoadingSpinnerView;
import org.quantumbadger.redreader.views.list.GroupedRecyclerViewItemListItemView;
import org.quantumbadger.redreader.views.list.GroupedRecyclerViewItemListSectionHeaderView;
import org.quantumbadger.redreader.views.liststatus.ErrorView;

import java.util.*;
import java.util.concurrent.atomic.AtomicBoolean;

public class MainMenuListingManager {

	private static final int
			GROUP_MAIN_HEADER 				= 0,
			GROUP_MAIN_ITEMS 				= 1,
			GROUP_USER_HEADER 				= 2,
			GROUP_USER_ITEMS 				= 3,
			GROUP_PINNED_SUBREDDITS_HEADER 	= 4,
			GROUP_PINNED_SUBREDDITS_ITEMS 	= 5,
			GROUP_BLOCKED_SUBREDDITS_HEADER	= 6,
			GROUP_BLOCKED_SUBREDDITS_ITEMS  = 7,
			GROUP_MULTIREDDITS_HEADER 		= 8,
			GROUP_MULTIREDDITS_ITEMS 		= 9,
			GROUP_SUBREDDITS_HEADER 		= 10,
			GROUP_SUBREDDITS_ITEMS 			= 11;

	@NonNull private final GroupedRecyclerViewAdapter mAdapter = new GroupedRecyclerViewAdapter(12);
	@NonNull private final Context mContext;
	@NonNull private final AppCompatActivity mActivity;

	@NonNull private final MainMenuSelectionListener mListener;

	@Nullable private GroupedRecyclerViewAdapter.Item mMultiredditHeaderItem;

	@Nullable private ArrayList<String> mSubredditSubscriptions;
	@Nullable private ArrayList<String> mMultiredditSubscriptions;

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

		SubredditAction(final int descriptionResId){
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

		final Drawable rrIconPerson;
		final Drawable rrIconEnvOpen;
		final Drawable rrIconSend;
		final Drawable rrIconStarFilled;
		final Drawable rrIconCross;
		final Drawable rrIconUpvote;
		final Drawable rrIconDownvote;

		{
			final TypedArray attr = activity.obtainStyledAttributes(new int[]{
					R.attr.rrIconPerson,
					R.attr.rrIconEnvOpen,
					R.attr.rrIconSend,
					R.attr.rrIconStarFilled,
					R.attr.rrIconCross,
					R.attr.rrIconUpvote,
					R.attr.rrIconDownvote
			});

			rrIconPerson = ContextCompat.getDrawable(activity, attr.getResourceId(0, 0));
			rrIconEnvOpen = ContextCompat.getDrawable(activity, attr.getResourceId(1, 0));
			rrIconSend = ContextCompat.getDrawable(activity, attr.getResourceId(2, 0));
			rrIconStarFilled = ContextCompat.getDrawable(activity, attr.getResourceId(3, 0));
			rrIconCross = ContextCompat.getDrawable(activity, attr.getResourceId(4, 0));
			rrIconUpvote = ContextCompat.getDrawable(activity, attr.getResourceId(5, 0));
			rrIconDownvote = ContextCompat.getDrawable(activity, attr.getResourceId(6, 0));

			attr.recycle();
		}

		mAdapter.appendToGroup(GROUP_MAIN_ITEMS,
				makeItem(R.string.mainmenu_frontpage, MainMenuFragment.MENU_MENU_ACTION_FRONTPAGE, null, true));

		if(PrefsUtility.pref_show_popular_main_menu(
				activity,
				PreferenceManager.getDefaultSharedPreferences(activity))) {
			mAdapter.appendToGroup(GROUP_MAIN_ITEMS,
								   makeItem(R.string.mainmenu_popular, MainMenuFragment.MENU_MENU_ACTION_POPULAR, null, false));
		}

		mAdapter.appendToGroup(GROUP_MAIN_ITEMS,
				makeItem(R.string.mainmenu_all, MainMenuFragment.MENU_MENU_ACTION_ALL, null, false));

		mAdapter.appendToGroup(GROUP_MAIN_ITEMS,
				makeItem(R.string.mainmenu_custom_destination, MainMenuFragment.MENU_MENU_ACTION_CUSTOM, null, false));

		if(PrefsUtility.pref_show_random_main_menu(
				activity,
				PreferenceManager.getDefaultSharedPreferences(activity))) {
			mAdapter.appendToGroup(GROUP_MAIN_ITEMS,
					makeItem(R.string.mainmenu_random, MainMenuFragment.MENU_MENU_ACTION_RANDOM, null, false));
		}

		if(!user.isAnonymous()) {

			final SharedPreferences sharedPreferences = PreferenceManager.getDefaultSharedPreferences(activity);
			final EnumSet<MainMenuFragment.MainMenuUserItems> mainMenuUserItems
					= PrefsUtility.pref_menus_mainmenu_useritems(activity, sharedPreferences);

			if(!mainMenuUserItems.isEmpty()) {
				if(PrefsUtility.pref_appearance_hide_username_main_menu(
						activity,
						PreferenceManager.getDefaultSharedPreferences(activity))) {

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

				if(mainMenuUserItems.contains(MainMenuFragment.MainMenuUserItems.PROFILE))
					mAdapter.appendToGroup(
							GROUP_USER_ITEMS,
							makeItem(R.string.mainmenu_profile, MainMenuFragment.MENU_MENU_ACTION_PROFILE,
									rrIconPerson, isFirst.getAndSet(false)));

				if(mainMenuUserItems.contains(MainMenuFragment.MainMenuUserItems.INBOX))
					mAdapter.appendToGroup(
							GROUP_USER_ITEMS,
							makeItem(R.string.mainmenu_inbox, MainMenuFragment.MENU_MENU_ACTION_INBOX,
									rrIconEnvOpen, isFirst.getAndSet(false)));

				if(mainMenuUserItems.contains(MainMenuFragment.MainMenuUserItems.SUBMITTED))
					mAdapter.appendToGroup(
							GROUP_USER_ITEMS,
							makeItem(R.string.mainmenu_submitted, MainMenuFragment.MENU_MENU_ACTION_SUBMITTED,
									rrIconSend, isFirst.getAndSet(false)));

				if(mainMenuUserItems.contains(MainMenuFragment.MainMenuUserItems.SAVED))
					mAdapter.appendToGroup(
							GROUP_USER_ITEMS,
							makeItem(R.string.mainmenu_saved, MainMenuFragment.MENU_MENU_ACTION_SAVED,
									rrIconStarFilled, isFirst.getAndSet(false)));

				if(mainMenuUserItems.contains(MainMenuFragment.MainMenuUserItems.HIDDEN))
					mAdapter.appendToGroup(
							GROUP_USER_ITEMS,
							makeItem(R.string.mainmenu_hidden, MainMenuFragment.MENU_MENU_ACTION_HIDDEN,
									rrIconCross, isFirst.getAndSet(false)));

				if(mainMenuUserItems.contains(MainMenuFragment.MainMenuUserItems.UPVOTED))
					mAdapter.appendToGroup(
							GROUP_USER_ITEMS,
							makeItem(R.string.mainmenu_upvoted, MainMenuFragment.MENU_MENU_ACTION_UPVOTED,
									rrIconUpvote, isFirst.getAndSet(false)));

				if(mainMenuUserItems.contains(MainMenuFragment.MainMenuUserItems.DOWNVOTED))
					mAdapter.appendToGroup(
							GROUP_USER_ITEMS,
							makeItem(R.string.mainmenu_downvoted, MainMenuFragment.MENU_MENU_ACTION_DOWNVOTED,
									rrIconDownvote, isFirst.getAndSet(false)));

				if(mainMenuUserItems.contains(MainMenuFragment.MainMenuUserItems.MODMAIL))
					mAdapter.appendToGroup(
							GROUP_USER_ITEMS,
							makeItem(R.string.mainmenu_modmail, MainMenuFragment.MENU_MENU_ACTION_MODMAIL,
									rrIconEnvOpen, isFirst.getAndSet(false)));
			}
		}

		setPinnedSubreddits();

		if(PrefsUtility.pref_appearance_show_blocked_subreddits_main_menu(
				activity,
				PreferenceManager.getDefaultSharedPreferences(activity))) {

			setBlockedSubreddits();
		}

		if(!user.isAnonymous()) {
			showMultiredditsHeader(activity);

			final LoadingSpinnerView multiredditsLoadingSpinnerView = new LoadingSpinnerView(activity);
			final int paddingPx = General.dpToPixels(activity, 30);
			multiredditsLoadingSpinnerView.setPadding(paddingPx, paddingPx, paddingPx, paddingPx);

			final GroupedRecyclerViewItemFrameLayout multiredditsLoadingItem
					= new GroupedRecyclerViewItemFrameLayout(multiredditsLoadingSpinnerView);
			mAdapter.appendToGroup(GROUP_MULTIREDDITS_ITEMS, multiredditsLoadingItem);
		}

		mAdapter.appendToGroup(
				GROUP_SUBREDDITS_HEADER,
				new GroupedRecyclerViewItemListSectionHeaderView(
						activity.getString(R.string.mainmenu_header_subreddits_subscribed)));

		{
			final LoadingSpinnerView subredditsLoadingSpinnerView = new LoadingSpinnerView(activity);
			final int paddingPx = General.dpToPixels(activity, 30);
			subredditsLoadingSpinnerView.setPadding(paddingPx, paddingPx, paddingPx, paddingPx);

			final GroupedRecyclerViewItemFrameLayout subredditsLoadingItem
					= new GroupedRecyclerViewItemFrameLayout(subredditsLoadingSpinnerView);
			mAdapter.appendToGroup(GROUP_SUBREDDITS_ITEMS, subredditsLoadingItem);
		}
	}

	private void setPinnedSubreddits() {
		final List<String> pinnedSubreddits
				= PrefsUtility.pref_pinned_subreddits(mActivity, PreferenceManager.getDefaultSharedPreferences(mActivity));
		final PrefsUtility.PinnedSubredditSort pinnedSubredditsSort = PrefsUtility.pref_behaviour_pinned_subredditsort(mActivity, PreferenceManager.getDefaultSharedPreferences(mActivity));


		if (pinnedSubreddits != null) {
			mAdapter.removeAllFromGroup(GROUP_PINNED_SUBREDDITS_ITEMS);
			mAdapter.removeAllFromGroup(GROUP_PINNED_SUBREDDITS_HEADER);
		}
		if (!pinnedSubreddits.isEmpty()) {
			mAdapter.appendToGroup(
					GROUP_PINNED_SUBREDDITS_HEADER,
					new GroupedRecyclerViewItemListSectionHeaderView(
							mActivity.getString(R.string.mainmenu_header_subreddits_pinned)));
			boolean isFirst = true;
			switch (pinnedSubredditsSort) {
				case NAME:
					Collections.sort(pinnedSubreddits);
					break;
				case DATE: /*noop*/
					break;
			}

			for (final String sr : pinnedSubreddits) {
				mAdapter.appendToGroup(GROUP_PINNED_SUBREDDITS_ITEMS, makeSubredditItem(sr, isFirst));
				isFirst = false;
			}
		}

	}

	private void setBlockedSubreddits() {


		final List<String> blockedSubreddits
				= PrefsUtility.pref_blocked_subreddits(mActivity, PreferenceManager.getDefaultSharedPreferences(mActivity));
		final PrefsUtility.BlockedSubredditSort blockedSubredditsSort = PrefsUtility.pref_behaviour_blocked_subredditsort(mActivity, PreferenceManager.getDefaultSharedPreferences(mActivity));

		if (blockedSubreddits != null) {
			mAdapter.removeAllFromGroup(GROUP_BLOCKED_SUBREDDITS_ITEMS);
			mAdapter.removeAllFromGroup(GROUP_BLOCKED_SUBREDDITS_HEADER);
		}

		if (!blockedSubreddits.isEmpty()) {
			mAdapter.appendToGroup(
					GROUP_BLOCKED_SUBREDDITS_HEADER,
					new GroupedRecyclerViewItemListSectionHeaderView(
							mActivity.getString(R.string.mainmenu_header_subreddits_blocked)));

			switch (blockedSubredditsSort) {
				case NAME:
					Collections.sort(blockedSubreddits);
					break;
				case DATE: /*noop*/
					break;
			}

			boolean isFirst = true;
			for (final String sr : blockedSubreddits) {
				mAdapter.appendToGroup(GROUP_BLOCKED_SUBREDDITS_ITEMS, makeSubredditItem(sr, isFirst));
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

		AndroidCommon.UI_THREAD_HANDLER.post(new Runnable() {
			@Override
			public void run() {

				mAdapter.removeAllFromGroup(GROUP_MULTIREDDITS_ITEMS);
				mAdapter.appendToGroup(GROUP_MULTIREDDITS_ITEMS, new GroupedRecyclerViewItemFrameLayout(errorView));
			}
		});
	}

	public void setSubredditsError(final ErrorView errorView) {

		AndroidCommon.UI_THREAD_HANDLER.post(new Runnable() {
			@Override
			public void run() {

				mAdapter.removeAllFromGroup(GROUP_SUBREDDITS_ITEMS);
				mAdapter.appendToGroup(GROUP_SUBREDDITS_ITEMS, new GroupedRecyclerViewItemFrameLayout(errorView));
			}
		});
	}

	public void setSubreddits(final Collection<String> subscriptions) {

		final ArrayList<String> subscriptionsSorted = new ArrayList<>(subscriptions);
		Collections.sort(subscriptionsSorted);

		AndroidCommon.UI_THREAD_HANDLER.post(new Runnable() {
			@Override
			public void run() {

				if(mSubredditSubscriptions != null
						&& mSubredditSubscriptions.equals(subscriptionsSorted)) {

					return;
				}

				mSubredditSubscriptions = subscriptionsSorted;

				mAdapter.removeAllFromGroup(GROUP_SUBREDDITS_ITEMS);

				boolean isFirst = true;

				for(final String subreddit : subscriptionsSorted) {

					GroupedRecyclerViewItemListItemView item;

					try {
						item = makeSubredditItem(
								RedditSubreddit.getDisplayNameFromCanonicalName(RedditSubreddit.getCanonicalName(subreddit)),
								isFirst);

					} catch(RedditSubreddit.InvalidSubredditNameException e) {
						item = makeSubredditItem("Invalid: " + subreddit, isFirst);
					}

					mAdapter.appendToGroup(GROUP_SUBREDDITS_ITEMS, item);

					isFirst = false;
				}
			}
		});
	}

	public void setMultireddits(final Collection<String> subscriptions) {

		final ArrayList<String> subscriptionsSorted = new ArrayList<>(subscriptions);
		Collections.sort(subscriptionsSorted);

		AndroidCommon.UI_THREAD_HANDLER.post(new Runnable() {
			@Override
			public void run() {

				if(mMultiredditSubscriptions != null
						&& mMultiredditSubscriptions.equals(subscriptionsSorted)) {

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

						final GroupedRecyclerViewItemListItemView item = makeMultiredditItem(multireddit, isFirst);
						mAdapter.appendToGroup(GROUP_MULTIREDDITS_ITEMS, item);

						isFirst = false;
					}
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

		final View.OnClickListener clickListener = new View.OnClickListener() {
			@Override
			public void onClick(final View view) {
				mListener.onSelected(action);
			}
		};

		return new GroupedRecyclerViewItemListItemView(icon, name, hideDivider, clickListener, null);
	}

	private GroupedRecyclerViewItemListItemView makeSubredditItem(
			final String name,
			final boolean hideDivider) {

		final View.OnClickListener clickListener = new View.OnClickListener() {
			@Override
			public void onClick(final View view) {
				try {
					final String canonicalName = RedditSubreddit.getCanonicalName(name);

					if(canonicalName.startsWith("/r/")) {
						mListener.onSelected((PostListingURL) SubredditPostListURL.getSubreddit(canonicalName));

					} else {
						LinkHandler.onLinkClicked(mActivity, canonicalName);
					}


				} catch(RedditSubreddit.InvalidSubredditNameException e) {
					throw new RuntimeException(e);
				}
			}
		};

		final View.OnLongClickListener longClickListener = new View.OnLongClickListener() {
			@Override
			public boolean onLongClick(final View view) {
				try {
					final EnumSet<SubredditAction> itemPref = PrefsUtility.pref_menus_subreddit_context_items(mActivity, PreferenceManager.getDefaultSharedPreferences(mActivity));
					List<String> pinnedSubreddits = PrefsUtility.pref_pinned_subreddits(mActivity, PreferenceManager.getDefaultSharedPreferences(mActivity));

					if (itemPref.isEmpty()) {
						return true;
					}
					final String subredditCanonicalName = RedditSubreddit.getCanonicalName(name);
					final ArrayList<SubredditMenuItem> menu = new ArrayList<>();
					if (itemPref.contains(SubredditAction.COPY_URL)){
						menu.add(new SubredditMenuItem(mActivity, R.string.action_copy_link, SubredditAction.COPY_URL));
					}
					if (itemPref.contains(SubredditAction.EXTERNAL)) {
						menu.add(new SubredditMenuItem(mActivity, R.string.action_external, SubredditAction.EXTERNAL));
					}
					if (itemPref.contains(SubredditAction.SHARE)){
						menu.add(new SubredditMenuItem(mActivity, R.string.action_share, SubredditAction.SHARE));
					}

					if (itemPref.contains(SubredditAction.BLOCK)){
						final List<String> blockedSubreddits = PrefsUtility.pref_blocked_subreddits(mActivity, PreferenceManager.getDefaultSharedPreferences(mActivity));

						if (blockedSubreddits.contains(subredditCanonicalName)){
							menu.add(new SubredditMenuItem(mActivity, R.string.unblock_subreddit, SubredditAction.UNBLOCK));
						} else {
							menu.add(new SubredditMenuItem(mActivity, R.string.block_subreddit, SubredditAction.BLOCK));
						}
					}

					if (itemPref.contains(SubredditAction.PIN)){
						if (pinnedSubreddits.contains(subredditCanonicalName)){
							menu.add(new SubredditMenuItem(mActivity, R.string.unpin_subreddit,SubredditAction.UNPIN));
						} else {
							menu.add(new SubredditMenuItem(mActivity, R.string.pin_subreddit, SubredditAction.PIN));
						}
					}

					if (!RedditAccountManager.getInstance(mActivity).getDefaultAccount().isAnonymous()) {

						if(itemPref.contains(SubredditAction.SUBSCRIBE)) {

							final RedditSubredditSubscriptionManager subscriptionManager = RedditSubredditSubscriptionManager
									.getSingleton(mActivity, RedditAccountManager.getInstance(mActivity).getDefaultAccount());

							if(subscriptionManager.areSubscriptionsReady()) {
								if(subscriptionManager.getSubscriptionState(subredditCanonicalName)
										== RedditSubredditSubscriptionManager.SubredditSubscriptionState.SUBSCRIBED) {
									menu.add(new SubredditMenuItem(mActivity, R.string.options_unsubscribe, SubredditAction.UNSUBSCRIBE));
								} else {
									menu.add(new SubredditMenuItem(mActivity, R.string.options_subscribe, SubredditAction.SUBSCRIBE));
								}
							}
						}
					}

					final String[] menuText = new String[menu.size()];

					for (int i = 0; i < menuText.length; i++) {
						menuText[i] = menu.get(i).title;
					}

					final AlertDialog.Builder builder = new AlertDialog.Builder(mActivity);

					builder.setItems(menuText, new DialogInterface.OnClickListener() {
						@Override
						public void onClick(DialogInterface dialog, int which) {
							onSubredditActionMenuItemSelected(subredditCanonicalName, mActivity, menu.get(which).action);
						}
					});

					final AlertDialog alert = builder.create();
					alert.setCanceledOnTouchOutside(true);
					alert.show();

				} catch(RedditSubreddit.InvalidSubredditNameException e) {
					throw new RuntimeException(e);
				}
				return true;
			}
		};

		return new GroupedRecyclerViewItemListItemView(null, name, hideDivider, clickListener, longClickListener);
	}

	private void onSubredditActionMenuItemSelected(String subredditCanonicalName, AppCompatActivity activity, SubredditAction action) {
		try {
			final String url = Constants.Reddit.getNonAPIUri(subredditCanonicalName).toString();

			RedditSubredditSubscriptionManager subMan = RedditSubredditSubscriptionManager
					.getSingleton(activity, RedditAccountManager.getInstance(activity).getDefaultAccount());
			List<String> pinnedSubreddits = PrefsUtility.pref_pinned_subreddits(mActivity, PreferenceManager.getDefaultSharedPreferences(mActivity));
			List<String> blockedSubreddits = PrefsUtility.pref_blocked_subreddits(mActivity, PreferenceManager.getDefaultSharedPreferences(mActivity));

			switch (action) {
				case SHARE:
					final Intent mailer = new Intent(Intent.ACTION_SEND);
					mailer.setType("text/plain");
					mailer.putExtra(Intent.EXTRA_TEXT, url);
					activity.startActivity(Intent.createChooser(mailer, activity.getString(R.string.action_share)));
					break;
				case COPY_URL:
					ClipboardManager manager = (ClipboardManager) activity.getSystemService(Context.CLIPBOARD_SERVICE);
					manager.setText(url);
					break;

				case EXTERNAL:
					final Intent intent = new Intent(Intent.ACTION_VIEW);
					intent.setData(Uri.parse(url));
					activity.startActivity(intent);
					break;

				case PIN:
					if (!pinnedSubreddits.contains(subredditCanonicalName)){
						PrefsUtility.pref_pinned_subreddits_add(
							mActivity,
							PreferenceManager.getDefaultSharedPreferences(mActivity),
							subredditCanonicalName);
					} else {
						Toast.makeText(mActivity, R.string.mainmenu_toast_subscribed, Toast.LENGTH_SHORT).show();
					}
					break;

				case UNPIN:
					if (pinnedSubreddits.contains(subredditCanonicalName)) {
						PrefsUtility.pref_pinned_subreddits_remove(
								mActivity,
								PreferenceManager.getDefaultSharedPreferences(mActivity),
								subredditCanonicalName);
					} else {
						Toast.makeText(mActivity, R.string.mainmenu_toast_not_pinned, Toast.LENGTH_SHORT).show();
					}
					break;

				case BLOCK:

					if (!blockedSubreddits.contains(subredditCanonicalName)){
						PrefsUtility.pref_blocked_subreddits_add(
								mActivity,
								PreferenceManager.getDefaultSharedPreferences(mActivity),
								subredditCanonicalName);
					} else {
						Toast.makeText(mActivity, R.string.mainmenu_toast_blocked, Toast.LENGTH_SHORT).show();
					}
					break;

				case UNBLOCK:

					if (blockedSubreddits.contains(subredditCanonicalName)){
						PrefsUtility.pref_blocked_subreddits_remove(
								mActivity,
								PreferenceManager.getDefaultSharedPreferences(mActivity),
								subredditCanonicalName);
					} else {
						Toast.makeText(mActivity, R.string.mainmenu_toast_not_blocked, Toast.LENGTH_SHORT).show();
					}
					break;

				case SUBSCRIBE:

					if (subMan.getSubscriptionState(subredditCanonicalName) == RedditSubredditSubscriptionManager.SubredditSubscriptionState.NOT_SUBSCRIBED){
						subMan.subscribe(subredditCanonicalName, activity);
						setPinnedSubreddits();
						setBlockedSubreddits();
						Toast.makeText(mActivity, R.string.options_subscribing, Toast.LENGTH_SHORT).show();
					} else {
						Toast.makeText(mActivity, R.string.mainmenu_toast_subscribed, Toast.LENGTH_SHORT).show();
					}
					break;

				case UNSUBSCRIBE:

					if (subMan.getSubscriptionState(subredditCanonicalName) == RedditSubredditSubscriptionManager.SubredditSubscriptionState.SUBSCRIBED){
						subMan.unsubscribe(subredditCanonicalName, activity);
						setPinnedSubreddits();
						setBlockedSubreddits();
						Toast.makeText(mActivity, R.string.options_unsubscribing , Toast.LENGTH_SHORT).show();
					} else {
						Toast.makeText(mActivity, R.string.mainmenu_toast_not_subscribed, Toast.LENGTH_SHORT).show();
					}
					break;
			}
		} catch (RedditSubreddit.InvalidSubredditNameException ex){
			throw new RuntimeException(ex);
		}
	}

	private GroupedRecyclerViewItemListItemView makeMultiredditItem(
			final String name,
			final boolean hideDivider) {

		final View.OnClickListener clickListener = new View.OnClickListener() {
			@Override
			public void onClick(final View view) {
				mListener.onSelected((PostListingURL)MultiredditPostListURL.getMultireddit(name));
			}
		};

		return new GroupedRecyclerViewItemListItemView(null, name, hideDivider, clickListener, null);
	}

	private static class SubredditMenuItem {
		public final String title;
		public final SubredditAction action;

		private SubredditMenuItem(Context context, int titleRes, SubredditAction action) {
			this.title = context.getString(titleRes);
			this.action = action;
		}
	}
}
