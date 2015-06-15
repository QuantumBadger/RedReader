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

import android.content.DialogInterface;
import android.content.Intent;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.graphics.Color;
import android.os.Bundle;
import android.support.v4.app.FragmentManager;
import android.support.v4.app.FragmentTransaction;
import android.view.View;
import android.view.WindowManager;
import com.actionbarsherlock.view.Menu;
import com.actionbarsherlock.view.MenuItem;
import org.holoeverywhere.app.AlertDialog;
import org.holoeverywhere.preference.PreferenceManager;
import org.holoeverywhere.preference.SharedPreferences;
import org.holoeverywhere.widget.EditText;
import org.holoeverywhere.widget.LinearLayout;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.account.RedditAccountChangeListener;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.adapters.MainMenuSelectionListener;
import org.quantumbadger.redreader.cache.CacheManager;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.LinkHandler;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.fragments.*;
import org.quantumbadger.redreader.listingcontrollers.CommentListingController;
import org.quantumbadger.redreader.listingcontrollers.PostListingController;
import org.quantumbadger.redreader.reddit.api.RedditSubredditSubscriptionManager;
import org.quantumbadger.redreader.reddit.prepared.RedditPreparedPost;
import org.quantumbadger.redreader.reddit.things.RedditSubreddit;
import org.quantumbadger.redreader.reddit.url.*;
import org.quantumbadger.redreader.views.RedditPostView;

import java.util.Set;
import java.util.UUID;

public class MainActivity extends RefreshableActivity
		implements MainMenuSelectionListener,
		RedditAccountChangeListener,
		RedditPostView.PostSelectionListener,
		SharedPreferences.OnSharedPreferenceChangeListener,
		OptionsMenuUtility.OptionsMenuSubredditsListener,
		OptionsMenuUtility.OptionsMenuPostsListener,
		OptionsMenuUtility.OptionsMenuCommentsListener,
		SessionChangeListener, RedditSubredditSubscriptionManager.SubredditSubscriptionStateChangeListener {

	private boolean twoPane;

	private MainMenuFragment mainMenuFragment;

	private PostListingController postListingController;
	private PostListingFragment postListingFragment;

	private CommentListingController commentListingController;
	private CommentListingFragment commentListingFragment;

	private boolean isMenuShown = true;

	private SharedPreferences sharedPreferences;

	@Override
	protected void onCreate(final Bundle savedInstanceState) {

		sharedPreferences = PreferenceManager.getDefaultSharedPreferences(this);
		if (savedInstanceState == null) {
			if(PrefsUtility.pref_behaviour_skiptofrontpage(this, sharedPreferences))
				onSelected(SubredditPostListURL.getFrontPage());
		}

		PrefsUtility.applyTheme(this);

		OptionsMenuUtility.fixActionBar(this, getString(R.string.app_name));

		sharedPreferences.registerOnSharedPreferenceChangeListener(this);

		final boolean solidblack = PrefsUtility.appearance_solidblack(this, sharedPreferences)
				&& PrefsUtility.appearance_theme(this, sharedPreferences) == PrefsUtility.AppearanceTheme.NIGHT;

		super.onCreate(savedInstanceState);

		twoPane = General.isTablet(this, sharedPreferences);

		final View layout;

		if(twoPane)
			layout = getLayoutInflater().inflate(R.layout.main_double);
		else
			layout = getLayoutInflater().inflate(R.layout.main_single);

		if(solidblack) layout.setBackgroundColor(Color.BLACK);

		setContentView(layout);

		doRefresh(RefreshableFragment.MAIN, false);

		RedditAccountManager.getInstance(this).addUpdateListener(this);

		final PackageInfo pInfo;
		try {
			pInfo = getPackageManager().getPackageInfo(getPackageName(), 0);
		} catch(PackageManager.NameNotFoundException e) {
			throw new RuntimeException(e);
		}

		final int appVersion = pInfo.versionCode;

		if(!sharedPreferences.contains("firstRunMessageShown")) {

			new AlertDialog.Builder(this)
					.setTitle(R.string.firstrun_login_title)
					.setMessage(R.string.firstrun_login_message)
					.setPositiveButton(R.string.firstrun_login_button_now,
							new DialogInterface.OnClickListener() {
								public void onClick(final DialogInterface dialog, final int which) {
									new AccountListDialog().show(MainActivity.this);
								}
							})
					.setNegativeButton(R.string.firstrun_login_button_later, null)
					.show();

			final SharedPreferences.Editor edit = sharedPreferences.edit();
			edit.putString("firstRunMessageShown", "true");
			edit.commit();

		} else if(sharedPreferences.contains("lastVersion")) {

			final int lastVersion = sharedPreferences.getInt("lastVersion", 0);

			if(lastVersion != appVersion) {

				General.quickToast(this, "Updated to version " + pInfo.versionName);

				sharedPreferences.edit().putInt("lastVersion", appVersion).commit();
				ChangelogDialog.newInstance().show(this);

				if(lastVersion <= 51) {
					// Upgrading from v1.8.6.3 or lower

					final Set<String> existingCommentHeaderItems = PrefsUtility.getStringSet(
							R.string.pref_appearance_comment_header_items_key,
							R.array.pref_appearance_comment_header_items_default,
							this,
							sharedPreferences
					);

					existingCommentHeaderItems.add("gold");

					sharedPreferences.edit().putStringSet(
							R.string.pref_appearance_comment_header_items_key,
							existingCommentHeaderItems
					).commit();

					new Thread() {
						@Override
						public void run() {
							CacheManager.getInstance(MainActivity.this).emptyTheWholeCache();
						}
					}.start();
				}
			}

		} else {
			sharedPreferences.edit().putInt("lastVersion", appVersion).commit();
			ChangelogDialog.newInstance().show(this);
		}

		addSubscriptionListener();

		Boolean startInbox = getIntent().getBooleanExtra("isNewMessage", false);
		if(startInbox) {
			startActivity(new Intent(this, InboxListingActivity.class));
		}
	}

	private void addSubscriptionListener() {
		RedditSubredditSubscriptionManager
				.getSingleton(this, RedditAccountManager.getInstance(this).getDefaultAccount())
				.addListener(this);
	}

	public void onSelected(final MainMenuFragment.MainMenuAction type, final String name) {

		final String username = RedditAccountManager.getInstance(this).getDefaultAccount().username;

		switch(type) {

			case FRONTPAGE:
				onSelected(SubredditPostListURL.getFrontPage());
				break;

			case ALL:
				onSelected(SubredditPostListURL.getAll());
				break;

			case SUBMITTED:
				onSelected(UserPostListingURL.getSubmitted(username));
				break;

			case SAVED:
				onSelected(UserPostListingURL.getSaved(username));
				break;

			case HIDDEN:
				onSelected(UserPostListingURL.getHidden(username));
				break;

			case UPVOTED:
				onSelected(UserPostListingURL.getLiked(username));
				break;

			case DOWNVOTED:
				onSelected(UserPostListingURL.getDisliked(username));
				break;

			case PROFILE:
				LinkHandler.onLinkClicked(this, new UserProfileURL(RedditAccountManager.getInstance(this).getDefaultAccount().username).toString());
				break;

			case CUSTOM: {

				final AlertDialog.Builder alertBuilder = new AlertDialog.Builder(this);
				final LinearLayout layout = (LinearLayout) getLayoutInflater().inflate(R.layout.dialog_editbox);
				final EditText editText = (EditText)layout.findViewById(R.id.dialog_editbox_edittext);

				editText.requestFocus();

				alertBuilder.setView(layout);
				alertBuilder.setTitle(R.string.mainmenu_custom);

				alertBuilder.setPositiveButton(R.string.dialog_go, new DialogInterface.OnClickListener() {
					public void onClick(DialogInterface dialog, int which) {

						final String subredditInput = editText.getText().toString().trim();

						try {
							final String normalizedName = RedditSubreddit.stripRPrefix(subredditInput);
							final RedditURLParser.RedditURL redditURL = SubredditPostListURL.getSubreddit(normalizedName);
							if(redditURL == null || redditURL.pathType() != RedditURLParser.PathType.SubredditPostListingURL) {
								General.quickToast(MainActivity.this, R.string.mainmenu_custom_invalid_name);
							} else {
								onSelected(redditURL.asSubredditPostListURL());
							}
						} catch(RedditSubreddit.InvalidSubredditNameException e){
							General.quickToast(MainActivity.this, R.string.mainmenu_custom_invalid_name);
						}
					}
				});

				alertBuilder.setNegativeButton(R.string.dialog_cancel, null);

				final AlertDialog alertDialog = alertBuilder.create();
				alertDialog.getWindow().setSoftInputMode(WindowManager.LayoutParams.SOFT_INPUT_STATE_VISIBLE);
				alertDialog.show();

				break;
			}

			case INBOX:
				startActivity(new Intent(this, InboxListingActivity.class));
				break;

			case MODMAIL: {
				final Intent intent = new Intent(this, InboxListingActivity.class);
				intent.putExtra("modmail", true);
				startActivity(intent);
				break;
			}
		}
	}

	public void onSelected(final PostListingURL url) {

		if(url == null) {
			return;
		}

		if(twoPane) {

			postListingController = new PostListingController(url);
			requestRefresh(RefreshableFragment.POSTS, false);

		} else {
			final Intent intent = new Intent(this, PostListingActivity.class);
			intent.setData(url.generateJsonUri());
			startActivityForResult(intent, 1);
		}
	}

	public void onRedditAccountChanged() {
		addSubscriptionListener();
		postInvalidateOptionsMenu();
		requestRefresh(RefreshableFragment.ALL, false);
	}

	@Override
	protected void doRefresh(final RefreshableFragment which, final boolean force) {

		if(which == RefreshableFragment.MAIN_RELAYOUT) {

			final FragmentTransaction transaction = getSupportFragmentManager().beginTransaction();

			if(postListingFragment != null) {
				postListingFragment.cancel();
				transaction.remove(postListingFragment);
			}

			if(commentListingFragment != null) {
				transaction.remove(commentListingFragment);
			}

			transaction.commit();
			getSupportFragmentManager().executePendingTransactions(); // may not be necessary...

			mainMenuFragment = null;
			postListingFragment = null;
			commentListingFragment = null;

			twoPane = General.isTablet(this, sharedPreferences);

			if(twoPane)
				setContentView(R.layout.main_double);
			else
				setContentView(R.layout.main_single);

			invalidateOptionsMenu();
			requestRefresh(RefreshableFragment.MAIN, false);

			return;
		}

		if(twoPane) {

			final int postContainer = isMenuShown ? R.id.main_right_frame : R.id.main_left_frame;

			if(isMenuShown && (which == RefreshableFragment.ALL || which == RefreshableFragment.MAIN)) {
				mainMenuFragment = MainMenuFragment.newInstance(force);
				final FragmentTransaction transaction = getSupportFragmentManager().beginTransaction();
				transaction.replace(R.id.main_left_frame, mainMenuFragment, "main_fragment");
				transaction.commit();
			}

			if(postListingController != null && (which == RefreshableFragment.ALL || which == RefreshableFragment.POSTS)) {
				if(force && postListingFragment != null) postListingFragment.cancel();
				postListingFragment = postListingController.get(force);
				final FragmentTransaction transaction = getSupportFragmentManager().beginTransaction();
				transaction.replace(postContainer, postListingFragment, "posts_fragment");
				transaction.commit();
			}

			if(commentListingController != null && (which == RefreshableFragment.ALL || which == RefreshableFragment.COMMENTS)) {
				commentListingFragment = commentListingController.get(force);
				final FragmentTransaction transaction = getSupportFragmentManager().beginTransaction();
				transaction.replace(R.id.main_right_frame, commentListingFragment, "comments_fragment");
				transaction.commit();
			}

		} else {

			if(which == RefreshableFragment.ALL || which == RefreshableFragment.MAIN) {
				mainMenuFragment = MainMenuFragment.newInstance(force);
				final FragmentTransaction transaction = getSupportFragmentManager().beginTransaction();
				transaction.replace(R.id.main_single_frame, mainMenuFragment, "main_fragment");
				transaction.commit();
			}
		}

		invalidateOptionsMenu();
	}

	@Override
	public void onBackPressed() {

		if(!General.onBackPressed()) return;

		if(!twoPane || isMenuShown) {
			super.onBackPressed();
			return;
		}

		isMenuShown = true;

		final FragmentTransaction transaction = getSupportFragmentManager().beginTransaction();

		mainMenuFragment = MainMenuFragment.newInstance(false); // TODO preserve position
		postListingFragment = postListingController.get(false); // TODO preserve position

		transaction.replace(R.id.main_left_frame, mainMenuFragment);
		transaction.replace(R.id.main_right_frame, postListingFragment);
		commentListingFragment = null;

		transaction.commit();

		invalidateOptionsMenu();
	}

	public void onPostCommentsSelected(final RedditPreparedPost post) {

		if(twoPane) {

			commentListingController = new CommentListingController(PostCommentListingURL.forPostId(post.idAlone), this);

			if(isMenuShown) {

				final FragmentManager fm = getSupportFragmentManager();

				fm.beginTransaction().remove(postListingFragment).commit();
				fm.executePendingTransactions();

				final FragmentTransaction transaction = fm.beginTransaction();
				commentListingFragment = commentListingController.get(false);
				transaction.replace(R.id.main_left_frame, postListingFragment); // TODO fix this...
				transaction.replace(R.id.main_right_frame, commentListingFragment);

				mainMenuFragment = null;
				isMenuShown = false;

				transaction.commit();

				invalidateOptionsMenu();

			} else {
				requestRefresh(RefreshableFragment.COMMENTS, false);
			}

		} else {
			LinkHandler.onLinkClicked(this, PostCommentListingURL.forPostId(post.idAlone).toString(), false);
		}
	}

	public void onPostSelected(final RedditPreparedPost post) {
		if(post.isSelf()) {
			onPostCommentsSelected(post);
		} else {
			LinkHandler.onLinkClicked(this, post.url, false, post.src);
		}
	}

	public void onSharedPreferenceChanged(final SharedPreferences prefs, final String key) {

		if(PrefsUtility.isRestartRequired(this, key)) {
			requestRefresh(RefreshableFragment.RESTART, false);
		}

		if(PrefsUtility.isReLayoutRequired(this, key)) {
			requestRefresh(RefreshableFragment.MAIN_RELAYOUT, false);

		} else if(PrefsUtility.isRefreshRequired(this, key)) {
			requestRefresh(RefreshableFragment.ALL, false);
		}
	}

	@Override
	protected void onDestroy() {
		super.onDestroy();
		sharedPreferences.unregisterOnSharedPreferenceChangeListener(this);
	}

	@Override
	public boolean onCreateOptionsMenu(final Menu menu) {

		final boolean postsVisible = postListingFragment != null;
		final boolean commentsVisible = commentListingFragment != null;

		final boolean postsSortable = postListingController != null && postListingController.isSortable();
		final boolean commentsSortable = commentListingController != null && commentListingController.isSortable();

		final RedditAccount user = RedditAccountManager.getInstance(this).getDefaultAccount();
		final RedditSubredditSubscriptionManager.SubredditSubscriptionState subredditSubscriptionState;
		final RedditSubredditSubscriptionManager subredditSubscriptionManager
				= RedditSubredditSubscriptionManager.getSingleton(this, user);

		if(postsVisible
				&& !user.isAnonymous()
				&& postListingController.isSubreddit()
				&& subredditSubscriptionManager.areSubscriptionsReady()
				&& postListingFragment != null
				&& postListingFragment.getSubreddit() != null) {

			subredditSubscriptionState = subredditSubscriptionManager.getSubscriptionState(
					postListingController.subredditCanonicalName());

		} else {
			subredditSubscriptionState = null;
		}

		final String subredditDescription = postListingFragment != null && postListingFragment.getSubreddit() != null
				? postListingFragment.getSubreddit().description_html : null;

		OptionsMenuUtility.prepare(
				this,
				menu,
				isMenuShown,
				postsVisible,
				commentsVisible,
				false,
				postsSortable,
				commentsSortable,
				subredditSubscriptionState,
				postsVisible && subredditDescription != null && subredditDescription.length() > 0,
				true);

		getSupportActionBar().setHomeButtonEnabled(!isMenuShown);
		getSupportActionBar().setDisplayHomeAsUpEnabled(!isMenuShown);

		return true;
	}

	public void onRefreshComments() {
		commentListingController.setSession(null);
		requestRefresh(RefreshableFragment.COMMENTS, true);
	}

	public void onPastComments() {
		final SessionListDialog sessionListDialog = SessionListDialog.newInstance(commentListingController.getUri(), commentListingController.getSession(), SessionChangeListener.SessionChangeType.COMMENTS);
		sessionListDialog.show(this);
	}

	public void onSortSelected(final PostCommentListingURL.Sort order) {
		commentListingController.setSort(order);
		requestRefresh(RefreshableFragment.COMMENTS, false);
	}

	public void onRefreshPosts() {
		postListingController.setSession(null);
		requestRefresh(RefreshableFragment.POSTS, true);
	}

	public void onPastPosts() {
		final SessionListDialog sessionListDialog = SessionListDialog.newInstance(postListingController.getUri(), postListingController.getSession(), SessionChangeListener.SessionChangeType.POSTS);
		sessionListDialog.show(this);
	}

	public void onSubmitPost() {
		final Intent intent = new Intent(this, PostSubmitActivity.class);
		if(postListingController.isSubreddit()) {
			intent.putExtra("subreddit", postListingController.subredditCanonicalName());
		}
		startActivity(intent);
	}

	public void onSortSelected(final PostListingController.Sort order) {
		postListingController.setSort(order);
		requestRefresh(RefreshableFragment.POSTS, false);
	}

	public void onSearchPosts() {
		PostListingActivity.onSearchPosts(postListingController, this);
	}

	@Override
	public void onSubscribe() {
		if(postListingFragment != null) postListingFragment.onSubscribe();
	}

	@Override
	public void onUnsubscribe() {
		if(postListingFragment != null) postListingFragment.onUnsubscribe();
	}

	@Override
	public void onSidebar() {
		final Intent intent = new Intent(this, HtmlViewActivity.class);
		intent.putExtra("html", postListingFragment.getSubreddit().getSidebarHtml(PrefsUtility.isNightMode(this)));
		intent.putExtra("title", String.format("%s: %s",
				getString(R.string.sidebar_activity_title),
				postListingFragment.getSubreddit().url));
		startActivityForResult(intent, 1);
	}

	public void onRefreshSubreddits() {
		requestRefresh(RefreshableFragment.MAIN, true);
	}

	@Override
	public boolean onOptionsItemSelected(final MenuItem item) {
		switch(item.getItemId()) {
			case android.R.id.home:
				onBackPressed();
				return true;
			default:
				return super.onOptionsItemSelected(item);
		}
	}

	public void onSessionSelected(UUID session, SessionChangeType type) {

		switch(type) {
			case POSTS:
				postListingController.setSession(session);
				requestRefresh(RefreshableFragment.POSTS, false);
				break;
			case COMMENTS:
				commentListingController.setSession(session);
				requestRefresh(RefreshableFragment.COMMENTS, false);
				break;
		}
	}

	public void onSessionRefreshSelected(SessionChangeType type) {
		switch(type) {
			case POSTS:
				onRefreshPosts();
				break;
			case COMMENTS:
				onRefreshComments();
				break;
		}
	}

	public void onSessionChanged(UUID session, SessionChangeType type, long timestamp) {

		switch(type) {
			case POSTS:
				if(postListingController != null) postListingController.setSession(session);
				break;
			case COMMENTS:
				if(commentListingController != null) commentListingController.setSession(session);
				break;
		}
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
