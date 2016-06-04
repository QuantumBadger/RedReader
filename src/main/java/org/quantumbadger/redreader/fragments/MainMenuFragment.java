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
import android.os.Bundle;
import android.support.annotation.IntDef;
import android.support.v7.app.AppCompatActivity;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.LinearLayout;
import android.widget.ListView;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.adapters.MainMenuAdapter;
import org.quantumbadger.redreader.adapters.MainMenuSelectionListener;
import org.quantumbadger.redreader.common.AndroidApi;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.RRError;
import org.quantumbadger.redreader.common.TimestampBound;
import org.quantumbadger.redreader.io.RequestResponseHandler;
import org.quantumbadger.redreader.reddit.api.RedditSubredditSubscriptionManager;
import org.quantumbadger.redreader.reddit.api.SubredditRequestFailure;
import org.quantumbadger.redreader.reddit.url.PostListingURL;
import org.quantumbadger.redreader.views.liststatus.ErrorView;
import org.quantumbadger.redreader.views.liststatus.LoadingView;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.util.Collection;
import java.util.HashSet;

public class MainMenuFragment extends RRFragment implements MainMenuSelectionListener, RedditSubredditSubscriptionManager.SubredditSubscriptionStateChangeListener {

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

	@IntDef({MENU_MENU_ACTION_FRONTPAGE, MENU_MENU_ACTION_PROFILE, MENU_MENU_ACTION_INBOX,
		MENU_MENU_ACTION_SUBMITTED, MENU_MENU_ACTION_UPVOTED, MENU_MENU_ACTION_DOWNVOTED,
		MENU_MENU_ACTION_SAVED, MENU_MENU_ACTION_MODMAIL, MENU_MENU_ACTION_HIDDEN,
		MENU_MENU_ACTION_CUSTOM, MENU_MENU_ACTION_ALL})
	@Retention(RetentionPolicy.SOURCE)
	public @interface MainMenuAction {}

	private final MainMenuAdapter mAdapter;

	private final LinearLayout mNotifications;
	private final LoadingView mLoadingView;

	private final LinearLayout mOuter;

	public MainMenuFragment(
			final AppCompatActivity parent,
			final Bundle savedInstanceState,
			final boolean force) {

		super(parent, savedInstanceState);
		final Context context = getActivity();

		final RedditAccount user = RedditAccountManager.getInstance(context).getDefaultAccount();

		mOuter = new LinearLayout(context);
		mOuter.setOrientation(LinearLayout.VERTICAL);

		mNotifications = new LinearLayout(context);
		mNotifications.setOrientation(LinearLayout.VERTICAL);

		mLoadingView = new LoadingView(context, R.string.download_waiting, true, true);

		final ListView lv = new ListView(context);
		lv.setDivider(null);

		lv.addFooterView(mNotifications);

		final int paddingPx = General.dpToPixels(context, 8);
		lv.setPadding(paddingPx, 0, paddingPx, 0);

		mAdapter = new MainMenuAdapter(context, user, this);
		lv.setAdapter(mAdapter);

		lv.setOnItemClickListener(new AdapterView.OnItemClickListener() {
			public void onItemClick(final AdapterView<?> adapterView, final View view, final int position, final long id) {
				mAdapter.clickOn(position);
			}
		});

		AndroidApi.UI_THREAD_HANDLER.post(new Runnable() {
			@Override
			public void run() {
				mNotifications.addView(mLoadingView);
				mLoadingView.setIndeterminate(R.string.download_subreddits);
			}
		});

		final RedditSubredditSubscriptionManager subredditSubscriptionManager
				= RedditSubredditSubscriptionManager.getSingleton(context, user);

		if(force) {
			subredditSubscriptionManager.triggerUpdate(new RequestResponseHandler<HashSet<String>, SubredditRequestFailure>() {
				@Override
				public void onRequestFailed(SubredditRequestFailure failureReason) {
					onError(failureReason.asError(context));
				}

				@Override
				public void onRequestSuccess(HashSet<String> result, long timeCached) {
					subredditSubscriptionManager.addListener(MainMenuFragment.this);
					onSubscriptionsChanged(result);
				}
			}, TimestampBound.NONE);

		} else {

			subredditSubscriptionManager.addListener(MainMenuFragment.this);

			if(subredditSubscriptionManager.areSubscriptionsReady()) {
				onSubscriptionsChanged(subredditSubscriptionManager.getSubscriptionList());
			}
		}

		mOuter.addView(lv);
		lv.getLayoutParams().height = ViewGroup.LayoutParams.MATCH_PARENT;
	}

	public enum MainMenuUserItems {
		PROFILE, INBOX, SUBMITTED, SAVED, HIDDEN, UPVOTED, DOWNVOTED, MODMAIL
	}

	@Override
	public View getView() {
		return mOuter;
	}

	@Override
	public Bundle onSaveInstanceState() {
		return null;
	}

	public void onSubscriptionsChanged(final Collection<String> subscriptions) {
		mAdapter.setSubreddits(subscriptions);
		mLoadingView.setDone(R.string.download_done);
	}

	private void onError(final RRError error) {
		mLoadingView.setDone(R.string.download_failed);
		AndroidApi.UI_THREAD_HANDLER.post(new Runnable() {
			@Override
			public void run() {
				mNotifications.addView(new ErrorView(getActivity(), error));
			}
		});
	}

	@Override
	public void onSelected(final @MainMenuAction int type, final String name) {
		((MainMenuSelectionListener)getActivity()).onSelected(type, name);
	}

	@Override
	public void onSelected(final PostListingURL postListingURL) {
		((MainMenuSelectionListener)getActivity()).onSelected(postListingURL);
	}

	@Override
	public void onSubredditSubscriptionListUpdated(RedditSubredditSubscriptionManager subredditSubscriptionManager) {
		onSubscriptionsChanged(subredditSubscriptionManager.getSubscriptionList());
	}

	@Override
	public void onSubredditSubscriptionAttempted(RedditSubredditSubscriptionManager subredditSubscriptionManager) {
	}

	@Override
	public void onSubredditUnsubscriptionAttempted(RedditSubredditSubscriptionManager subredditSubscriptionManager) {
	}
}
