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

import android.content.Context;
import android.content.res.TypedArray;
import android.graphics.drawable.ColorDrawable;
import android.os.Bundle;
import android.view.View;
import androidx.annotation.IntDef;
import androidx.appcompat.app.AppCompatActivity;
import androidx.recyclerview.widget.RecyclerView;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.activities.OptionsMenuUtility;
import org.quantumbadger.redreader.adapters.MainMenuListingManager;
import org.quantumbadger.redreader.adapters.MainMenuSelectionListener;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.common.RRError;
import org.quantumbadger.redreader.common.TimestampBound;
import org.quantumbadger.redreader.io.RequestResponseHandler;
import org.quantumbadger.redreader.reddit.api.RedditMultiredditSubscriptionManager;
import org.quantumbadger.redreader.reddit.api.RedditSubredditSubscriptionManager;
import org.quantumbadger.redreader.reddit.api.SubredditRequestFailure;
import org.quantumbadger.redreader.reddit.things.SubredditCanonicalId;
import org.quantumbadger.redreader.reddit.url.PostListingURL;
import org.quantumbadger.redreader.views.ScrollbarRecyclerViewManager;
import org.quantumbadger.redreader.views.liststatus.ErrorView;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.util.Collection;
import java.util.HashSet;

public class MainMenuFragment extends RRFragment implements
		MainMenuSelectionListener,
		RedditSubredditSubscriptionManager.SubredditSubscriptionStateChangeListener,
		RedditMultiredditSubscriptionManager.MultiredditListChangeListener {

	public static final int MENU_MENU_ACTION_FRONTPAGE = 0;
	public static final int MENU_MENU_ACTION_PROFILE = 1;
	public static final int MENU_MENU_ACTION_INBOX = 2;
	public static final int MENU_MENU_ACTION_SUBMITTED = 3;
	public static final int MENU_MENU_ACTION_UPVOTED = 4;
	public static final int MENU_MENU_ACTION_DOWNVOTED = 5;
	public static final int MENU_MENU_ACTION_SAVED = 6;
	public static final int MENU_MENU_ACTION_MODMAIL = 7;
	public static final int MENU_MENU_ACTION_HIDDEN = 8;
	public static final int MENU_MENU_ACTION_CUSTOM = 9;
	public static final int MENU_MENU_ACTION_ALL = 10;
	public static final int MENU_MENU_ACTION_POPULAR = 11;
	public static final int MENU_MENU_ACTION_RANDOM = 12;
	public static final int MENU_MENU_ACTION_RANDOM_NSFW = 13;
	public static final int MENU_MENU_ACTION_SENT_MESSAGES = 14;
	public static final int MENU_MENU_ACTION_FIND_SUBREDDIT = 15;

	@IntDef({
			MENU_MENU_ACTION_FRONTPAGE,
			MENU_MENU_ACTION_PROFILE,
			MENU_MENU_ACTION_INBOX,
			MENU_MENU_ACTION_SUBMITTED,
			MENU_MENU_ACTION_UPVOTED,
			MENU_MENU_ACTION_DOWNVOTED,
			MENU_MENU_ACTION_SAVED,
			MENU_MENU_ACTION_MODMAIL,
			MENU_MENU_ACTION_HIDDEN,
			MENU_MENU_ACTION_CUSTOM,
			MENU_MENU_ACTION_ALL,
			MENU_MENU_ACTION_POPULAR,
			MENU_MENU_ACTION_RANDOM,
			MENU_MENU_ACTION_RANDOM_NSFW,
			MENU_MENU_ACTION_SENT_MESSAGES,
			MENU_MENU_ACTION_FIND_SUBREDDIT})
	@Retention(RetentionPolicy.SOURCE)
	public @interface MainMenuAction {
	}

	private final MainMenuListingManager mManager;

	private final View mOuter;

	public MainMenuFragment(
			final AppCompatActivity parent,
			final Bundle savedInstanceState,
			final boolean force) {

		super(parent, savedInstanceState);
		final Context context = getActivity();

		final RedditAccount user = RedditAccountManager.getInstance(context)
				.getDefaultAccount();

		final ScrollbarRecyclerViewManager recyclerViewManager
				= new ScrollbarRecyclerViewManager(parent, null, false);

		mOuter = recyclerViewManager.getOuterView();
		final RecyclerView recyclerView = recyclerViewManager.getRecyclerView();

		if(parent instanceof OptionsMenuUtility.OptionsMenuSubredditsListener
				&& PrefsUtility.pref_behaviour_enable_swipe_refresh()) {

			recyclerViewManager.enablePullToRefresh(
					((OptionsMenuUtility.OptionsMenuSubredditsListener)parent)
							::onRefreshSubreddits);
		}

		mManager = new MainMenuListingManager(getActivity(), this, user);

		recyclerView.setAdapter(mManager.getAdapter());

		final int paddingPx = General.dpToPixels(context, 8);
		recyclerView.setPadding(paddingPx, 0, paddingPx, 0);
		recyclerView.setClipToPadding(false);

		{
			final TypedArray appearance = context.obtainStyledAttributes(new int[] {
					R.attr.rrListItemBackgroundCol});

			getActivity().getWindow().setBackgroundDrawable(
					new ColorDrawable(appearance.getColor(0, General.COLOR_INVALID)));

			appearance.recycle();
		}

		final RedditMultiredditSubscriptionManager multiredditSubscriptionManager
				= RedditMultiredditSubscriptionManager.getSingleton(context, user);

		final RedditSubredditSubscriptionManager subredditSubscriptionManager
				= RedditSubredditSubscriptionManager.getSingleton(context, user);

		if(force) {
			multiredditSubscriptionManager.triggerUpdate(
					new RequestResponseHandler<HashSet<String>, SubredditRequestFailure>() {

						@Override
						public void onRequestFailed(final SubredditRequestFailure failureReason) {
							onMultiredditError(failureReason.asError(context));
						}

						@Override
						public void onRequestSuccess(
								final HashSet<String> result,
								final long timeCached) {

							multiredditSubscriptionManager.addListener(MainMenuFragment.this);
							onMultiredditSubscriptionsChanged(result);
						}
					}, TimestampBound.NONE);

			subredditSubscriptionManager.triggerUpdate(
					new RequestResponseHandler<
							HashSet<SubredditCanonicalId>,
							SubredditRequestFailure>() {
						@Override
						public void onRequestFailed(final SubredditRequestFailure failureReason) {
							onSubredditError(failureReason.asError(context));
						}

						@Override
						public void onRequestSuccess(
								final HashSet<SubredditCanonicalId> result,
								final long timeCached) {
							subredditSubscriptionManager.addListener(MainMenuFragment.this);
							onSubredditSubscriptionsChanged(result);
						}
					}, TimestampBound.NONE);

		} else {

			multiredditSubscriptionManager.addListener(this);
			subredditSubscriptionManager.addListener(this);

			if(multiredditSubscriptionManager.areSubscriptionsReady()) {
				onMultiredditSubscriptionsChanged(
						multiredditSubscriptionManager.getSubscriptionList());
			}

			if(subredditSubscriptionManager.areSubscriptionsReady()) {
				onSubredditSubscriptionsChanged(
						subredditSubscriptionManager.getSubscriptionList());
			}

			final TimestampBound.MoreRecentThanBound oneHour
					= TimestampBound.notOlderThan(1000 * 60 * 60);
			multiredditSubscriptionManager.triggerUpdate(null, oneHour);
			subredditSubscriptionManager.triggerUpdate(null, oneHour);
		}
	}

	public enum MainMenuUserItems {
		PROFILE, INBOX, SUBMITTED, SAVED, HIDDEN, UPVOTED, DOWNVOTED, MODMAIL, SENT_MESSAGES
	}

	public enum MainMenuShortcutItems {
		FRONTPAGE, POPULAR, ALL, SUBREDDIT_SEARCH, CUSTOM, RANDOM, RANDOM_NSFW
	}

	@Override
	public View getListingView() {
		return mOuter;
	}

	@Override
	public Bundle onSaveInstanceState() {
		return null;
	}

	public void onSubredditSubscriptionsChanged(
			final Collection<SubredditCanonicalId> subscriptions) {
		mManager.setSubreddits(subscriptions);
	}

	public void onMultiredditSubscriptionsChanged(final Collection<String> subscriptions) {
		mManager.setMultireddits(subscriptions);
	}

	private void onSubredditError(final RRError error) {
		mManager.setSubredditsError(new ErrorView(getActivity(), error));
	}

	private void onMultiredditError(final RRError error) {
		mManager.setMultiredditsError(new ErrorView(getActivity(), error));
	}

	@Override
	public void onSelected(final @MainMenuAction int type) {
		((MainMenuSelectionListener)getActivity()).onSelected(type);
	}

	@Override
	public void onSelected(final PostListingURL postListingURL) {
		((MainMenuSelectionListener)getActivity()).onSelected(postListingURL);
	}

	@Override
	public void onSubredditSubscriptionListUpdated(
			final RedditSubredditSubscriptionManager subredditSubscriptionManager) {
		onSubredditSubscriptionsChanged(subredditSubscriptionManager.getSubscriptionList());
	}

	@Override
	public void onMultiredditListUpdated(
			final RedditMultiredditSubscriptionManager multiredditSubscriptionManager) {
		onMultiredditSubscriptionsChanged(multiredditSubscriptionManager.getSubscriptionList());
	}

	@Override
	public void onSubredditSubscriptionAttempted(
			final RedditSubredditSubscriptionManager subredditSubscriptionManager) {
	}

	@Override
	public void onSubredditUnsubscriptionAttempted(
			final RedditSubredditSubscriptionManager subredditSubscriptionManager) {
	}

	public void onUpdateAnnouncement() {
		mManager.onUpdateAnnouncement();
	}
}
