/*******************************************************************************
 * This file is part of Scroll.
 *
 * Scroll is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Scroll is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Scroll.  If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/

package org.quantumbadger.redreader.fragments;

import android.app.Activity;
import android.content.Context;
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

import java.util.Collection;
import java.util.HashSet;

public class MainMenuFragment extends RRFragment implements MainMenuSelectionListener, RedditSubredditSubscriptionManager.SubredditSubscriptionStateChangeListener {

	private MainMenuAdapter adapter;

	private LinearLayout notifications;
	private LoadingView loadingView;

	private final Context context;

	private final boolean force;

	public MainMenuFragment(final Activity parent, final boolean force) {
		super(parent);
		context = parent;
		this.force = force;
	}

	public enum MainMenuAction {
		FRONTPAGE, PROFILE, INBOX, SUBMITTED, UPVOTED, DOWNVOTED, SAVED, MODMAIL, HIDDEN, CUSTOM, ALL
	}

	public enum MainMenuUserItems {
		PROFILE, INBOX, SUBMITTED, SAVED, HIDDEN, UPVOTED, DOWNVOTED, MODMAIL
	}

	@Override
	public View onCreateView() {

		final RedditAccount user = RedditAccountManager.getInstance(context).getDefaultAccount();

		final LinearLayout outer = new LinearLayout(context);
		outer.setOrientation(LinearLayout.VERTICAL);

		notifications = new LinearLayout(context);
		notifications.setOrientation(LinearLayout.VERTICAL);

		loadingView = new LoadingView(context, R.string.download_waiting, true, true);

		final ListView lv = new ListView(context);
		lv.setDivider(null);

		lv.addFooterView(notifications);

		final int paddingPx = General.dpToPixels(context, 8);
		lv.setPadding(paddingPx, 0, paddingPx, 0);

		adapter = new MainMenuAdapter(context, user, this);
		lv.setAdapter(adapter);

		lv.setOnItemClickListener(new AdapterView.OnItemClickListener() {
			public void onItemClick(final AdapterView<?> adapterView, final View view, final int position, final long id) {
				adapter.clickOn(position);
			}
		});

		AndroidApi.UI_THREAD_HANDLER.post(new Runnable() {
			public void run() {
				notifications.addView(loadingView);
				loadingView.setIndeterminate(R.string.download_subreddits);
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

		outer.addView(lv);
		lv.getLayoutParams().height = ViewGroup.LayoutParams.MATCH_PARENT;

		return outer;
	}

	public void onSubscriptionsChanged(final Collection<String> subscriptions) {
		adapter.setSubreddits(subscriptions);
		if(loadingView != null) loadingView.setDone(R.string.download_done);
	}

	private void onError(final RRError error) {
		if(loadingView != null) loadingView.setDone(R.string.download_failed);
		AndroidApi.UI_THREAD_HANDLER.post(new Runnable() {
			public void run() {
				notifications.addView(new ErrorView(getActivity(), error));
			}
		});
	}

	public void onSelected(final MainMenuAction type, final String name) {
		((MainMenuSelectionListener)getActivity()).onSelected(type, name);
	}

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
