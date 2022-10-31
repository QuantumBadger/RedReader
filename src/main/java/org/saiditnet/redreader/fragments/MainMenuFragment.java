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

import android.content.Context;
import android.content.res.TypedArray;
import android.graphics.drawable.ColorDrawable;
import android.os.Bundle;
import android.support.annotation.IntDef;
import android.support.v7.app.AppCompatActivity;
import android.support.v7.widget.RecyclerView;
import android.view.View;
import org.saiditnet.redreader.R;
import org.saiditnet.redreader.account.RedditAccount;
import org.saiditnet.redreader.account.RedditAccountManager;
import org.saiditnet.redreader.adapters.MainMenuListingManager;
import org.saiditnet.redreader.adapters.MainMenuSelectionListener;
import org.saiditnet.redreader.common.General;
import org.saiditnet.redreader.common.RRError;
import org.saiditnet.redreader.common.TimestampBound;
import org.saiditnet.redreader.io.RequestResponseHandler;
import org.saiditnet.redreader.reddit.api.RedditMultiredditSubscriptionManager;
import org.saiditnet.redreader.reddit.api.RedditSubredditSubscriptionManager;
import org.saiditnet.redreader.reddit.api.SubredditRequestFailure;
import org.saiditnet.redreader.reddit.url.PostListingURL;
import org.saiditnet.redreader.views.ScrollbarRecyclerViewManager;
import org.saiditnet.redreader.views.liststatus.ErrorView;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.util.Collection;
import java.util.HashSet;

public class MainMenuFragment extends RRFragment
		implements MainMenuSelectionListener,
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
	public static final int MENU_MENU_ACTION_SUBSCRIBED = 14;

	@IntDef({MENU_MENU_ACTION_FRONTPAGE, MENU_MENU_ACTION_PROFILE, MENU_MENU_ACTION_INBOX,
		MENU_MENU_ACTION_SUBMITTED, MENU_MENU_ACTION_UPVOTED, MENU_MENU_ACTION_DOWNVOTED,
		MENU_MENU_ACTION_SAVED, MENU_MENU_ACTION_MODMAIL, MENU_MENU_ACTION_HIDDEN,
		MENU_MENU_ACTION_CUSTOM, MENU_MENU_ACTION_ALL, MENU_MENU_ACTION_POPULAR,
		MENU_MENU_ACTION_RANDOM, MENU_MENU_ACTION_RANDOM_NSFW, MENU_MENU_ACTION_SUBSCRIBED})
	@Retention(RetentionPolicy.SOURCE)
	public @interface MainMenuAction {}

	private final MainMenuListingManager mManager;

	private final View mOuter;

	public MainMenuFragment(
			final AppCompatActivity parent,
			final Bundle savedInstanceState,
			final boolean force) {

		super(parent, savedInstanceState);
		final Context context = getActivity();

		final RedditAccount user = RedditAccountManager.getInstance(context).getDefaultAccount();

		ScrollbarRecyclerViewManager recyclerViewManager = new ScrollbarRecyclerViewManager(parent, null, false);

		mOuter = recyclerViewManager.getOuterView();
		final RecyclerView recyclerView = recyclerViewManager.getRecyclerView();

		mManager = new MainMenuListingManager(getActivity(), this, user);

		recyclerView.setAdapter(mManager.getAdapter());

		final int paddingPx = General.dpToPixels(context, 8);
		recyclerView.setPadding(paddingPx, 0, paddingPx, 0);
		recyclerView.setClipToPadding(false);

		{
			final TypedArray appearance = context.obtainStyledAttributes(new int[]{
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
			multiredditSubscriptionManager.triggerUpdate(new RequestResponseHandler<HashSet<String>, SubredditRequestFailure>() {
				@Override
				public void onRequestFailed(SubredditRequestFailure failureReason) {
					onMultiredditError(failureReason.asError(context));
				}

				@Override
				public void onRequestSuccess(HashSet<String> result, long timeCached) {
					multiredditSubscriptionManager.addListener(MainMenuFragment.this);
					onMultiredditSubscriptionsChanged(result);
				}
			}, TimestampBound.NONE);

			subredditSubscriptionManager.triggerUpdate(new RequestResponseHandler<HashSet<String>, SubredditRequestFailure>() {
				@Override
				public void onRequestFailed(SubredditRequestFailure failureReason) {
					onSubredditError(failureReason.asError(context));
				}

				@Override
				public void onRequestSuccess(HashSet<String> result, long timeCached) {
					subredditSubscriptionManager.addListener(MainMenuFragment.this);
					onSubredditSubscriptionsChanged(result);
				}
			}, TimestampBound.NONE);

		} else {

			multiredditSubscriptionManager.addListener(this);
			subredditSubscriptionManager.addListener(this);

			if(multiredditSubscriptionManager.areSubscriptionsReady()) {
				onMultiredditSubscriptionsChanged(multiredditSubscriptionManager.getSubscriptionList());
			}

			if(subredditSubscriptionManager.areSubscriptionsReady()) {
				onSubredditSubscriptionsChanged(subredditSubscriptionManager.getSubscriptionList());
			}

			final TimestampBound.MoreRecentThanBound oneHour = TimestampBound.notOlderThan(1000 * 60 * 60);
			multiredditSubscriptionManager.triggerUpdate(null, oneHour);
			subredditSubscriptionManager.triggerUpdate(null, oneHour);
		}
	}

	public enum MainMenuUserItems {
		PROFILE, INBOX, SUBMITTED, SAVED, HIDDEN, UPVOTED, DOWNVOTED, MODMAIL
	}

	public enum MainMenuShortcutItems{
		FRONTPAGE, POPULAR, ALL, CUSTOM, RANDOM, RANDOM_NSFW, SUBSCRIBED
	}

	@Override
	public View getView() {
		return mOuter;
	}

	@Override
	public Bundle onSaveInstanceState() {
		return null;
	}

	public void onSubredditSubscriptionsChanged(final Collection<String> subscriptions) {
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
	public void onSubredditSubscriptionListUpdated(RedditSubredditSubscriptionManager subredditSubscriptionManager) {
		onSubredditSubscriptionsChanged(subredditSubscriptionManager.getSubscriptionList());
	}

	@Override
	public void onMultiredditListUpdated(final RedditMultiredditSubscriptionManager multiredditSubscriptionManager) {
		onMultiredditSubscriptionsChanged(multiredditSubscriptionManager.getSubscriptionList());
	}

	@Override
	public void onSubredditSubscriptionAttempted(RedditSubredditSubscriptionManager subredditSubscriptionManager) {
	}

	@Override
	public void onSubredditUnsubscriptionAttempted(RedditSubredditSubscriptionManager subredditSubscriptionManager) {
	}
}
