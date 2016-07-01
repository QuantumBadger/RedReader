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

package org.quantumbadger.redreader.activities;


import android.app.AlertDialog;
import android.content.DialogInterface;
import android.content.Intent;
import android.graphics.drawable.ColorDrawable;
import android.os.Bundle;
import android.preference.PreferenceManager;
import android.support.v7.app.AppCompatActivity;
import android.view.Menu;
import android.view.MenuItem;
import android.view.WindowManager;
import android.widget.EditText;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.account.RedditAccountChangeListener;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.LinkHandler;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.fragments.PostListingFragment;
import org.quantumbadger.redreader.fragments.SessionListDialog;
import org.quantumbadger.redreader.listingcontrollers.PostListingController;
import org.quantumbadger.redreader.reddit.api.RedditSubredditSubscriptionManager;
import org.quantumbadger.redreader.reddit.prepared.RedditPreparedPost;
import org.quantumbadger.redreader.reddit.things.RedditSubreddit;
import org.quantumbadger.redreader.reddit.url.PostCommentListingURL;
import org.quantumbadger.redreader.reddit.url.PostListingURL;
import org.quantumbadger.redreader.reddit.url.RedditURLParser;
import org.quantumbadger.redreader.reddit.url.SearchPostListURL;
import org.quantumbadger.redreader.views.RedditPostView;

import java.util.UUID;

public class PostListingActivity extends RefreshableActivity
		implements RedditAccountChangeListener,
		RedditPostView.PostSelectionListener,
		OptionsMenuUtility.OptionsMenuPostsListener,
		SessionChangeListener,
		RedditSubredditSubscriptionManager.SubredditSubscriptionStateChangeListener {

	private static final String SAVEDSTATE_SESSION = "pla_session";
	private static final String SAVEDSTATE_SORT = "pla_sort";
	private static final String SAVEDSTATE_FRAGMENT = "pla_fragment";

	private PostListingFragment fragment;
	private PostListingController controller;

	public void onCreate(final Bundle savedInstanceState) {

		PrefsUtility.applyTheme(this);

		super.onCreate(savedInstanceState);

		getSupportActionBar().setHomeButtonEnabled(true);
		getSupportActionBar().setDisplayHomeAsUpEnabled(true);

		getWindow().setBackgroundDrawable(new ColorDrawable(obtainStyledAttributes(new int[] {R.attr.rrListBackgroundCol}).getColor(0,0)));

		RedditAccountManager.getInstance(this).addUpdateListener(this);

		if(getIntent() != null) {

			final Intent intent = getIntent();

			final RedditURLParser.RedditURL url = RedditURLParser.parseProbablePostListing(intent.getData());

			if(!(url instanceof PostListingURL)) {
				throw new RuntimeException(String.format("'%s' is not a post listing URL!", url.generateJsonUri()));
			}

			controller = new PostListingController((PostListingURL)url);

			Bundle fragmentSavedInstanceState = null;

			if(savedInstanceState != null) {

				if(savedInstanceState.containsKey(SAVEDSTATE_SESSION)) {
					controller.setSession(UUID.fromString(savedInstanceState.getString(SAVEDSTATE_SESSION)));
				}

				if(savedInstanceState.containsKey(SAVEDSTATE_SORT)) {
					controller.setSort(PostListingController.Sort.valueOf(
							savedInstanceState.getString(SAVEDSTATE_SORT)));
				}

				if(savedInstanceState.containsKey(SAVEDSTATE_FRAGMENT)) {
					fragmentSavedInstanceState = savedInstanceState.getBundle(SAVEDSTATE_FRAGMENT);
				}
			}

			getSupportActionBar().setTitle(url.humanReadableName(this, false));

			setBaseActivityContentView(R.layout.main_single);
			doRefresh(RefreshableFragment.POSTS, false, fragmentSavedInstanceState);

		} else {
			throw new RuntimeException("Nothing to show!");
		}

		addSubscriptionListener();
	}

	@Override
	protected void onSaveInstanceState(final Bundle outState) {
		super.onSaveInstanceState(outState);

		final UUID session = controller.getSession();
		if(session != null) {
			outState.putString(SAVEDSTATE_SESSION, session.toString());
		}

		final PostListingController.Sort sort = controller.getSort();
		if(sort != null) {
			outState.putString(SAVEDSTATE_SORT, sort.name());
		}

		if(fragment != null) {
			outState.putBundle(SAVEDSTATE_FRAGMENT, fragment.onSaveInstanceState());
		}
	}

	@Override
	public boolean onCreateOptionsMenu(final Menu menu) {

		final RedditAccount user = RedditAccountManager.getInstance(this).getDefaultAccount();
		final RedditSubredditSubscriptionManager.SubredditSubscriptionState subredditSubscriptionState;
		final RedditSubredditSubscriptionManager subredditSubscriptionManager
				= RedditSubredditSubscriptionManager.getSingleton(this, user);

		if(!user.isAnonymous()
				&& controller.isSubreddit()
				&& subredditSubscriptionManager.areSubscriptionsReady()
				&& fragment != null
				&& fragment.getSubreddit() != null) {

			subredditSubscriptionState = subredditSubscriptionManager.getSubscriptionState(controller.subredditCanonicalName());

		} else {
			subredditSubscriptionState = null;
		}

		final String subredditDescription = fragment != null && fragment.getSubreddit() != null
				? fragment.getSubreddit().description_html : null;

		Boolean subredditPinState = null;
		Boolean subredditBlockedState = null;

		if(controller.isSubreddit()
				&& fragment != null
				&& fragment.getSubreddit() != null) {

			try {
				subredditPinState = PrefsUtility.pref_pinned_subreddits_check(
						this,
						PreferenceManager.getDefaultSharedPreferences(this),
						fragment.getSubreddit().getCanonicalName());

				subredditBlockedState = PrefsUtility.pref_blocked_subreddits_check(
						this,
						PreferenceManager.getDefaultSharedPreferences(this),
						fragment.getSubreddit().getCanonicalName());

			} catch(RedditSubreddit.InvalidSubredditNameException e) {
				subredditPinState = null;
				subredditBlockedState = null;
			}
		}

		OptionsMenuUtility.prepare(
				this,
				menu,
				false,
				true,
				false,
				controller.isSearchResults(),
				controller.isSortable(),
				true,
				subredditSubscriptionState,
				subredditDescription != null && subredditDescription.length() > 0,
				false,
				subredditPinState,
				subredditBlockedState);

		return true;
	}

	private void addSubscriptionListener() {
		RedditSubredditSubscriptionManager
				.getSingleton(this, RedditAccountManager.getInstance(this).getDefaultAccount())
				.addListener(this);
	}

	public void onRedditAccountChanged() {
		addSubscriptionListener();
		postInvalidateOptionsMenu();
		requestRefresh(RefreshableFragment.ALL, false);
	}

	@Override
	protected void doRefresh(final RefreshableFragment which, final boolean force, final Bundle savedInstanceState) {
		if(fragment != null) fragment.cancel();
		fragment = controller.get(this, force, savedInstanceState);
		setBaseActivityContentView(fragment.getView());
	}

	public void onPostSelected(final RedditPreparedPost post) {
		LinkHandler.onLinkClicked(this, post.src.getUrl(), false, post.src.getSrc());
	}

	public void onPostCommentsSelected(final RedditPreparedPost post) {
		LinkHandler.onLinkClicked(this, PostCommentListingURL.forPostId(post.src.getIdAlone()).toString(), false);
	}

	public void onRefreshPosts() {
		controller.setSession(null);
		requestRefresh(RefreshableFragment.POSTS, true);
	}

	public void onPastPosts() {
		final SessionListDialog sessionListDialog = SessionListDialog.newInstance(controller.getUri(), controller.getSession(), SessionChangeType.POSTS);
		sessionListDialog.show(getSupportFragmentManager(), "SessionListDialog");
	}

	public void onSubmitPost() {
		final Intent intent = new Intent(this, PostSubmitActivity.class);
		if(controller.isSubreddit()) {
			intent.putExtra("subreddit", controller.subredditCanonicalName());
		}
		startActivity(intent);
	}

	public void onSortSelected(final PostListingController.Sort order) {
		controller.setSort(order);
		requestRefresh(RefreshableFragment.POSTS, false);
	}

	@Override
	public void onSearchPosts() {
		onSearchPosts(controller, this);
	}

	public static void onSearchPosts(final PostListingController controller, final AppCompatActivity activity) {

		final AlertDialog.Builder alertBuilder = new AlertDialog.Builder(activity);
		final EditText editText = (EditText) activity.getLayoutInflater().inflate(R.layout.dialog_editbox, null);

		alertBuilder.setView(editText);
		alertBuilder.setTitle(R.string.action_search);

		alertBuilder.setPositiveButton(R.string.action_search, new DialogInterface.OnClickListener() {
			@Override
			public void onClick(DialogInterface dialog, int which) {

				final String query = editText.getText().toString().toLowerCase().trim();

				final SearchPostListURL url;

				if(controller != null && (controller.isSubreddit() || controller.isSubredditSearchResults())) {
					url = SearchPostListURL.build(controller.subredditCanonicalName(), query);
				} else {
					url = SearchPostListURL.build(null, query);
				}

				final Intent intent = new Intent(activity, PostListingActivity.class);
				intent.setData(url.generateJsonUri());
				activity.startActivity(intent);
			}
		});

		alertBuilder.setNegativeButton(R.string.dialog_cancel, null);

		final AlertDialog alertDialog = alertBuilder.create();
		alertDialog.getWindow().setSoftInputMode(WindowManager.LayoutParams.SOFT_INPUT_STATE_VISIBLE);
		alertDialog.show();
	}

	@Override
	public void onSubscribe() {
		fragment.onSubscribe();
	}

	@Override
	public void onUnsubscribe() {
		fragment.onUnsubscribe();
	}

	@Override
	public void onSidebar() {
		final Intent intent = new Intent(this, HtmlViewActivity.class);
		intent.putExtra("html", fragment.getSubreddit().getSidebarHtml(PrefsUtility.isNightMode(this)));
		intent.putExtra("title", String.format("%s: %s",
				getString(R.string.sidebar_activity_title),
				fragment.getSubreddit().url));
		startActivityForResult(intent, 1);
	}

	@Override
	public void onPin() {

		if(fragment == null) return;

		try {
			PrefsUtility.pref_pinned_subreddits_add(
					this,
					PreferenceManager.getDefaultSharedPreferences(this),
					fragment.getSubreddit().getCanonicalName());

		} catch(RedditSubreddit.InvalidSubredditNameException e) {
			throw new RuntimeException(e);
		}

		invalidateOptionsMenu();
	}

	@Override
	public void onUnpin() {

		if(fragment == null) return;

		try {
			PrefsUtility.pref_pinned_subreddits_remove(
					this,
					PreferenceManager.getDefaultSharedPreferences(this),
					fragment.getSubreddit().getCanonicalName());

		} catch(RedditSubreddit.InvalidSubredditNameException e) {
			throw new RuntimeException(e);
		}

		invalidateOptionsMenu();
	}

	@Override
	public void onBlock() {
		if(fragment == null) return;

		try {
			PrefsUtility.pref_blocked_subreddits_add(
					this,
					PreferenceManager.getDefaultSharedPreferences(this),
					fragment.getSubreddit().getCanonicalName());

		} catch(RedditSubreddit.InvalidSubredditNameException e) {
			throw new RuntimeException(e);
		}

		invalidateOptionsMenu();
	}

	@Override
	public void onUnblock() {
		if(fragment == null) return;

		try {
			PrefsUtility.pref_blocked_subreddits_remove(
					this,
					PreferenceManager.getDefaultSharedPreferences(this),
					fragment.getSubreddit().getCanonicalName());

		} catch(RedditSubreddit.InvalidSubredditNameException e) {
			throw new RuntimeException(e);
		}

		invalidateOptionsMenu();
	}

	@Override
	public boolean onOptionsItemSelected(final MenuItem item) {
		switch(item.getItemId()) {
			case android.R.id.home:
				finish();
				return true;
			default:
				return super.onOptionsItemSelected(item);
		}
	}

	public void onSessionSelected(UUID session, SessionChangeType type) {
		controller.setSession(session);
		requestRefresh(RefreshableFragment.POSTS, false);
	}

	public void onSessionRefreshSelected(SessionChangeType type) {
		onRefreshPosts();
	}

	public void onSessionChanged(UUID session, SessionChangeType type, long timestamp) {
		controller.setSession(session);
	}

	@Override
	public void onBackPressed() {
		if(General.onBackPressed()) super.onBackPressed();
	}

	@Override
	public void onSubredditSubscriptionListUpdated(RedditSubredditSubscriptionManager subredditSubscriptionManager) {
		postInvalidateOptionsMenu();
	}

	@Override
	public void onSubredditSubscriptionAttempted(RedditSubredditSubscriptionManager subredditSubscriptionManager) {
		postInvalidateOptionsMenu();
	}

	@Override
	public void onSubredditUnsubscriptionAttempted(RedditSubredditSubscriptionManager subredditSubscriptionManager) {
		postInvalidateOptionsMenu();
	}

	private void postInvalidateOptionsMenu() {
		runOnUiThread(new Runnable() {
			@Override
			public void run() {
				invalidateOptionsMenu();
			}
		});
	}
}
