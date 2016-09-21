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

import android.content.Context;
import android.content.SharedPreferences;
import android.content.res.TypedArray;
import android.graphics.drawable.Drawable;
import android.preference.PreferenceManager;
import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.support.v4.content.ContextCompat;
import android.view.View;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.common.AndroidApi;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.fragments.MainMenuFragment;
import org.quantumbadger.redreader.reddit.things.RedditSubreddit;
import org.quantumbadger.redreader.reddit.url.MultiredditPostListURL;
import org.quantumbadger.redreader.reddit.url.PostListingURL;
import org.quantumbadger.redreader.reddit.url.SubredditPostListURL;
import org.quantumbadger.redreader.views.LoadingSpinnerView;
import org.quantumbadger.redreader.views.list.GroupedRecyclerViewItemListItemView;
import org.quantumbadger.redreader.views.list.GroupedRecyclerViewItemListSectionHeaderView;
import org.quantumbadger.redreader.views.liststatus.ErrorView;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.EnumSet;
import java.util.List;
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

	@NonNull private final MainMenuSelectionListener mListener;

	@Nullable private GroupedRecyclerViewAdapter.Item mMultiredditHeaderItem;

	@Nullable private ArrayList<String> mSubredditSubscriptions;
	@Nullable private ArrayList<String> mMultiredditSubscriptions;

	@NonNull
	public GroupedRecyclerViewAdapter getAdapter() {
		return mAdapter;
	}

	public MainMenuListingManager(
			@NonNull final Context context,
			@NonNull final MainMenuSelectionListener listener,
			@NonNull final RedditAccount user) {

		General.checkThisIsUIThread();

		mContext = context.getApplicationContext();
		mListener = listener;

		final Drawable rrIconPerson;
		final Drawable rrIconEnvOpen;
		final Drawable rrIconSend;
		final Drawable rrIconStarFilled;
		final Drawable rrIconCross;
		final Drawable rrIconUpvote;
		final Drawable rrIconDownvote;

		{
			final TypedArray attr = context.obtainStyledAttributes(new int[]{
					R.attr.rrIconPerson,
					R.attr.rrIconEnvOpen,
					R.attr.rrIconSend,
					R.attr.rrIconStarFilled,
					R.attr.rrIconCross,
					R.attr.rrIconUpvote,
					R.attr.rrIconDownvote
			});

			rrIconPerson = ContextCompat.getDrawable(context, attr.getResourceId(0, 0));
			rrIconEnvOpen = ContextCompat.getDrawable(context, attr.getResourceId(1, 0));
			rrIconSend = ContextCompat.getDrawable(context, attr.getResourceId(2, 0));
			rrIconStarFilled = ContextCompat.getDrawable(context, attr.getResourceId(3, 0));
			rrIconCross = ContextCompat.getDrawable(context, attr.getResourceId(4, 0));
			rrIconUpvote = ContextCompat.getDrawable(context, attr.getResourceId(5, 0));
			rrIconDownvote = ContextCompat.getDrawable(context, attr.getResourceId(6, 0));

			attr.recycle();
		}

		mAdapter.appendToGroup(GROUP_MAIN_ITEMS,
				makeItem(R.string.mainmenu_frontpage, MainMenuFragment.MENU_MENU_ACTION_FRONTPAGE, null, true));

		mAdapter.appendToGroup(GROUP_MAIN_ITEMS,
				makeItem(R.string.mainmenu_all, MainMenuFragment.MENU_MENU_ACTION_ALL, null, false));

		mAdapter.appendToGroup(GROUP_MAIN_ITEMS,
				makeItem(R.string.mainmenu_custom_destination, MainMenuFragment.MENU_MENU_ACTION_CUSTOM, null, false));

		if(!user.isAnonymous()) {

			final SharedPreferences sharedPreferences = PreferenceManager.getDefaultSharedPreferences(context);
			final EnumSet<MainMenuFragment.MainMenuUserItems> mainMenuUserItems
					= PrefsUtility.pref_menus_mainmenu_useritems(context, sharedPreferences);

			if(!mainMenuUserItems.isEmpty()) {
				if(PrefsUtility.pref_appearance_hide_username_main_menu(
						context,
						PreferenceManager.getDefaultSharedPreferences(context))) {

					mAdapter.appendToGroup(
							GROUP_USER_HEADER,
							new GroupedRecyclerViewItemListSectionHeaderView(
									context.getString(R.string.mainmenu_useritems)));

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

		final List<String> pinnedSubreddits
				= PrefsUtility.pref_pinned_subreddits(context, PreferenceManager.getDefaultSharedPreferences(context));
		final PrefsUtility.PinnedSubredditSort pinnedSubredditsSort = PrefsUtility.pref_behaviour_pinned_subredditsort(context, PreferenceManager.getDefaultSharedPreferences(context));
		if(!pinnedSubreddits.isEmpty()) {

			mAdapter.appendToGroup(
					GROUP_PINNED_SUBREDDITS_HEADER,
					new GroupedRecyclerViewItemListSectionHeaderView(
							context.getString(R.string.mainmenu_header_subreddits_pinned)));

			boolean isFirst = true;
			switch (pinnedSubredditsSort){
				case NAME: Collections.sort(pinnedSubreddits); break;
				case DATE: /*noop*/break;
			}

			for(final String sr : pinnedSubreddits) {
				mAdapter.appendToGroup(GROUP_PINNED_SUBREDDITS_ITEMS, makeSubredditItem(sr, isFirst));
				isFirst = false;
			}
		}

		if(PrefsUtility.pref_appearance_show_blocked_subreddits_main_menu(
				context,
				PreferenceManager.getDefaultSharedPreferences(context))) {
			final List<String> blockedSubreddits
					= PrefsUtility.pref_blocked_subreddits(context, PreferenceManager.getDefaultSharedPreferences(context));
			final PrefsUtility.BlockedSubredditSort blockedSubredditsSort = PrefsUtility.pref_behaviour_blocked_subredditsort(context, PreferenceManager.getDefaultSharedPreferences(context));
			if (!blockedSubreddits.isEmpty()) {
				mAdapter.appendToGroup(
						GROUP_BLOCKED_SUBREDDITS_HEADER,
						new GroupedRecyclerViewItemListSectionHeaderView(
								context.getString(R.string.mainmenu_header_subreddits_blocked)));

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

		if(!user.isAnonymous()) {
			showMultiredditsHeader(context);

			final LoadingSpinnerView multiredditsLoadingSpinnerView = new LoadingSpinnerView(context);
			final int paddingPx = General.dpToPixels(context, 30);
			multiredditsLoadingSpinnerView.setPadding(paddingPx, paddingPx, paddingPx, paddingPx);

			final GroupedRecyclerViewItemFrameLayout multiredditsLoadingItem
					= new GroupedRecyclerViewItemFrameLayout(multiredditsLoadingSpinnerView);
			mAdapter.appendToGroup(GROUP_MULTIREDDITS_ITEMS, multiredditsLoadingItem);
		}

		mAdapter.appendToGroup(
				GROUP_SUBREDDITS_HEADER,
				new GroupedRecyclerViewItemListSectionHeaderView(
						context.getString(R.string.mainmenu_header_subreddits_subscribed)));

		{
			final LoadingSpinnerView subredditsLoadingSpinnerView = new LoadingSpinnerView(context);
			final int paddingPx = General.dpToPixels(context, 30);
			subredditsLoadingSpinnerView.setPadding(paddingPx, paddingPx, paddingPx, paddingPx);

			final GroupedRecyclerViewItemFrameLayout subredditsLoadingItem
					= new GroupedRecyclerViewItemFrameLayout(subredditsLoadingSpinnerView);
			mAdapter.appendToGroup(GROUP_SUBREDDITS_ITEMS, subredditsLoadingItem);
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

		AndroidApi.UI_THREAD_HANDLER.post(new Runnable() {
			@Override
			public void run() {

				mAdapter.removeAllFromGroup(GROUP_MULTIREDDITS_ITEMS);
				mAdapter.appendToGroup(GROUP_MULTIREDDITS_ITEMS, new GroupedRecyclerViewItemFrameLayout(errorView));
			}
		});
	}

	public void setSubredditsError(final ErrorView errorView) {

		AndroidApi.UI_THREAD_HANDLER.post(new Runnable() {
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

		AndroidApi.UI_THREAD_HANDLER.post(new Runnable() {
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
						item = makeSubredditItem(RedditSubreddit.stripRPrefix(subreddit), isFirst);

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

		AndroidApi.UI_THREAD_HANDLER.post(new Runnable() {
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
					mListener.onSelected(
							(PostListingURL) SubredditPostListURL.getSubreddit(RedditSubreddit.getCanonicalName(name)));
				} catch(RedditSubreddit.InvalidSubredditNameException e) {
					throw new RuntimeException(e);
				}
			}
		};

		return new GroupedRecyclerViewItemListItemView(null, name, hideDivider, clickListener, null);
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
}
