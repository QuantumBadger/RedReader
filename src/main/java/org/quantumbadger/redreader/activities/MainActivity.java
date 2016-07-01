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
import android.content.SharedPreferences;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.os.Bundle;
import android.preference.PreferenceManager;
import android.text.Editable;
import android.text.TextWatcher;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.view.WindowManager;
import android.widget.EditText;
import android.widget.FrameLayout;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.account.RedditAccountChangeListener;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.adapters.MainMenuSelectionListener;
import org.quantumbadger.redreader.cache.CacheManager;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.LinkHandler;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.fragments.AccountListDialog;
import org.quantumbadger.redreader.fragments.ChangelogDialog;
import org.quantumbadger.redreader.fragments.CommentListingFragment;
import org.quantumbadger.redreader.fragments.MainMenuFragment;
import org.quantumbadger.redreader.fragments.PostListingFragment;
import org.quantumbadger.redreader.fragments.SessionListDialog;
import org.quantumbadger.redreader.listingcontrollers.CommentListingController;
import org.quantumbadger.redreader.listingcontrollers.PostListingController;
import org.quantumbadger.redreader.reddit.api.RedditSubredditSubscriptionManager;
import org.quantumbadger.redreader.reddit.prepared.RedditPreparedPost;
import org.quantumbadger.redreader.reddit.things.RedditSubreddit;
import org.quantumbadger.redreader.reddit.url.PostCommentListingURL;
import org.quantumbadger.redreader.reddit.url.PostListingURL;
import org.quantumbadger.redreader.reddit.url.RedditURLParser;
import org.quantumbadger.redreader.reddit.url.SubredditPostListURL;
import org.quantumbadger.redreader.reddit.url.UserPostListingURL;
import org.quantumbadger.redreader.reddit.url.UserProfileURL;
import org.quantumbadger.redreader.views.RedditPostView;

import java.util.Set;
import java.util.UUID;

public class MainActivity extends RefreshableActivity
		implements MainMenuSelectionListener,
		RedditAccountChangeListener,
		RedditPostView.PostSelectionListener,
		OptionsMenuUtility.OptionsMenuSubredditsListener,
		OptionsMenuUtility.OptionsMenuPostsListener,
		OptionsMenuUtility.OptionsMenuCommentsListener,
		SessionChangeListener,
		RedditSubredditSubscriptionManager.SubredditSubscriptionStateChangeListener {

	private boolean twoPane;

	private MainMenuFragment mainMenuFragment;

	private PostListingController postListingController;
	private PostListingFragment postListingFragment;

	private CommentListingController commentListingController;
	private CommentListingFragment commentListingFragment;

	private View mainMenuView;
	private View postListingView;
	private View commentListingView;

	private FrameLayout mSinglePane;

	private FrameLayout mLeftPane;
	private FrameLayout mRightPane;

	private boolean isMenuShown = true;

	private SharedPreferences sharedPreferences;

	@Override
	protected void onCreate(final Bundle savedInstanceState) {

		PrefsUtility.applyTheme(this);

		super.onCreate(savedInstanceState);

		sharedPreferences = PreferenceManager.getDefaultSharedPreferences(this);
		if (savedInstanceState == null) {
			if(PrefsUtility.pref_behaviour_skiptofrontpage(this, sharedPreferences))
				onSelected(SubredditPostListURL.getFrontPage());
		}

		getSupportActionBar().setTitle(R.string.app_name);

		twoPane = General.isTablet(this, sharedPreferences);

		doRefresh(RefreshableFragment.MAIN_RELAYOUT, false, null);

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
								@Override
								public void onClick(final DialogInterface dialog, final int which) {
									new AccountListDialog().show(MainActivity.this.getSupportFragmentManager(), null);
								}
							})
					.setNegativeButton(R.string.firstrun_login_button_later, null)
					.show();

			final SharedPreferences.Editor edit = sharedPreferences.edit();
			edit.putString("firstRunMessageShown", "true");
			edit.apply();

		} else if(sharedPreferences.contains("lastVersion")) {

			final int lastVersion = sharedPreferences.getInt("lastVersion", 0);

			if(lastVersion < 63) {
				// Upgrading across the 1.9.0 boundary (when oAuth was introduced)

				new AlertDialog.Builder(this)
						.setTitle(R.string.firstrun_login_title)
						.setMessage(R.string.upgrade_v190_login_message)
						.setPositiveButton(R.string.firstrun_login_button_now,
								new DialogInterface.OnClickListener() {
									@Override
									public void onClick(final DialogInterface dialog, final int which) {
										new AccountListDialog().show(MainActivity.this.getSupportFragmentManager(), null);
									}
								})
						.setNegativeButton(R.string.firstrun_login_button_later, null)
						.show();
			}

			if(lastVersion != appVersion) {

				General.quickToast(this, "Updated to version " + pInfo.versionName);

				sharedPreferences.edit().putInt("lastVersion", appVersion).apply();
				ChangelogDialog.newInstance().show(getSupportFragmentManager(), null);

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
							getString(R.string.pref_appearance_comment_header_items_key),
							existingCommentHeaderItems
					).apply();

					new Thread() {
						@Override
						public void run() {
							CacheManager.getInstance(MainActivity.this).emptyTheWholeCache();
						}
					}.start();
				}
			}

		} else {
			sharedPreferences.edit().putInt("lastVersion", appVersion).apply();
			ChangelogDialog.newInstance().show(getSupportFragmentManager(), null);
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

	public void onSelected(final @MainMenuFragment.MainMenuAction int type, final String name) {

		final String username = RedditAccountManager.getInstance(this).getDefaultAccount().username;

		switch(type) {

			case MainMenuFragment.MENU_MENU_ACTION_FRONTPAGE:
				onSelected(SubredditPostListURL.getFrontPage());
				break;

			case MainMenuFragment.MENU_MENU_ACTION_ALL:
				onSelected(SubredditPostListURL.getAll());
				break;

			case MainMenuFragment.MENU_MENU_ACTION_SUBMITTED:
				onSelected(UserPostListingURL.getSubmitted(username));
				break;

			case MainMenuFragment.MENU_MENU_ACTION_SAVED:
				onSelected(UserPostListingURL.getSaved(username));
				break;

			case MainMenuFragment.MENU_MENU_ACTION_HIDDEN:
				onSelected(UserPostListingURL.getHidden(username));
				break;

			case MainMenuFragment.MENU_MENU_ACTION_UPVOTED:
				onSelected(UserPostListingURL.getLiked(username));
				break;

			case MainMenuFragment.MENU_MENU_ACTION_DOWNVOTED:
				onSelected(UserPostListingURL.getDisliked(username));
				break;

			case MainMenuFragment.MENU_MENU_ACTION_PROFILE:
				LinkHandler.onLinkClicked(this, new UserProfileURL(RedditAccountManager.getInstance(this).getDefaultAccount().username).toString());
				break;

			case MainMenuFragment.MENU_MENU_ACTION_CUSTOM: {

				final AlertDialog.Builder alertBuilder = new AlertDialog.Builder(this);
				final EditText editText = (EditText) getLayoutInflater().inflate(R.layout.dialog_editbox, null);

				editText.addTextChangedListener(new TextWatcher() {
					@Override
					public void beforeTextChanged(CharSequence s, int start, int count, int after) {}

					@Override
					public void onTextChanged(CharSequence s, int start, int before, int count) {}

					@Override
					public void afterTextChanged(Editable s) {
						if (s.toString().contains(" ")) {
							String result = s.toString().replaceAll(" ", "");

							editText.setText(result);
							editText.setSelection(result.length());
						}
					}
				});

				alertBuilder.setView(editText);
				alertBuilder.setTitle(R.string.mainmenu_custom);

				alertBuilder.setPositiveButton(R.string.dialog_go, new DialogInterface.OnClickListener() {
					@Override
					public void onClick(DialogInterface dialog, int which) {

						final String subredditInput = editText.getText().toString().trim().replace(" ", "");

						try {
							final String normalizedName = RedditSubreddit.stripRPrefix(subredditInput);
							final RedditURLParser.RedditURL redditURL = SubredditPostListURL.getSubreddit(normalizedName);
							if(redditURL == null || redditURL.pathType() != RedditURLParser.SUBREDDIT_POST_LISTING_URL) {
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

			case MainMenuFragment.MENU_MENU_ACTION_INBOX:
				startActivity(new Intent(this, InboxListingActivity.class));
				break;

			case MainMenuFragment.MENU_MENU_ACTION_MODMAIL: {
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
	protected void doRefresh(final RefreshableFragment which, final boolean force, final Bundle savedInstanceState) {

		if(which == RefreshableFragment.MAIN_RELAYOUT) {

			mainMenuFragment = null;
			postListingFragment = null;
			commentListingFragment = null;

			mainMenuView = null;
			postListingView = null;
			commentListingView = null;

			if(mLeftPane != null) mLeftPane.removeAllViews();
			if(mRightPane != null) mRightPane.removeAllViews();

			twoPane = General.isTablet(this, sharedPreferences);

			final View layout;

			if(twoPane) {
				layout = getLayoutInflater().inflate(R.layout.main_double, null);
				mLeftPane = (FrameLayout)layout.findViewById(R.id.main_left_frame);
				mRightPane = (FrameLayout)layout.findViewById(R.id.main_right_frame);
				mSinglePane = null;
			} else {
				layout = getLayoutInflater().inflate(R.layout.main_single, null);
				mLeftPane = null;
				mRightPane = null;
				mSinglePane = (FrameLayout)layout.findViewById(R.id.main_single_frame);
			}

			setBaseActivityContentView(layout);

			invalidateOptionsMenu();
			requestRefresh(RefreshableFragment.ALL, false);

			return;
		}

		if(twoPane) {

			final FrameLayout postContainer = isMenuShown ? mRightPane : mLeftPane;

			if(isMenuShown && (which == RefreshableFragment.ALL || which == RefreshableFragment.MAIN)) {
				mainMenuFragment = new MainMenuFragment(this, null, force);
				mainMenuView = mainMenuFragment.getView();
				mLeftPane.removeAllViews();
				mLeftPane.addView(mainMenuView);
			}

			if(postListingController != null && (which == RefreshableFragment.ALL || which == RefreshableFragment.POSTS)) {
				if(force && postListingFragment != null) postListingFragment.cancel();
				postListingFragment = postListingController.get(this, force, null);
				postListingView = postListingFragment.getView();
				postContainer.removeAllViews();
				postContainer.addView(postListingView);
			}

			if(commentListingController != null && (which == RefreshableFragment.ALL || which == RefreshableFragment.COMMENTS)) {
				commentListingFragment = commentListingController.get(this, force, null);
				commentListingView = commentListingFragment.getView();
				mRightPane.removeAllViews();
				mRightPane.addView(commentListingView);
			}

		} else {

			if(which == RefreshableFragment.ALL || which == RefreshableFragment.MAIN) {
				mainMenuFragment = new MainMenuFragment(this, null, force);
				mainMenuView = mainMenuFragment.getView();
				mSinglePane.removeAllViews();
				mSinglePane.addView(mainMenuView);
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

		mainMenuFragment = new MainMenuFragment(this, null, false); // TODO preserve position
		mainMenuView = mainMenuFragment.getView();

		commentListingFragment = null;
		commentListingView = null;

		mLeftPane.removeAllViews();
		mRightPane.removeAllViews();

		mLeftPane.addView(mainMenuView);
		mRightPane.addView(postListingView);

		invalidateOptionsMenu();
	}

	public void onPostCommentsSelected(final RedditPreparedPost post) {

		if(twoPane) {

			commentListingController = new CommentListingController(PostCommentListingURL.forPostId(post.src.getIdAlone()), this);

			if(isMenuShown) {

				commentListingFragment = commentListingController.get(this, false, null);
				commentListingView = commentListingFragment.getView();

				mLeftPane.removeAllViews();
				mRightPane.removeAllViews();

				mLeftPane.addView(postListingView);
				mRightPane.addView(commentListingView);

				mainMenuFragment = null;
				mainMenuView = null;

				isMenuShown = false;

				invalidateOptionsMenu();

			} else {
				requestRefresh(RefreshableFragment.COMMENTS, false);
			}

		} else {
			LinkHandler.onLinkClicked(this, PostCommentListingURL.forPostId(post.src.getIdAlone()).toString(), false);
		}
	}

	public void onPostSelected(final RedditPreparedPost post) {
		if(post.isSelf()) {
			onPostCommentsSelected(post);
		} else {
			LinkHandler.onLinkClicked(this, post.src.getUrl(), false, post.src.getSrc());
		}
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

		Boolean subredditPinState = null;
		Boolean subredditBlockedState = null;

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

		if(postsVisible
				&& postListingController.isSubreddit()
				&& postListingFragment != null
				&& postListingFragment.getSubreddit() != null) {

			try {
				subredditPinState = PrefsUtility.pref_pinned_subreddits_check(
						this,
						sharedPreferences,
						postListingFragment.getSubreddit().getCanonicalName());

				subredditBlockedState = PrefsUtility.pref_blocked_subreddits_check(
						this,
						sharedPreferences,
						postListingFragment.getSubreddit().getCanonicalName());

			} catch(RedditSubreddit.InvalidSubredditNameException e) {
				subredditPinState = null;
				subredditBlockedState = null;
			}
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
				true,
				subredditPinState,
				subredditBlockedState);

		getSupportActionBar().setHomeButtonEnabled(!isMenuShown);
		getSupportActionBar().setDisplayHomeAsUpEnabled(!isMenuShown);

		if(commentListingFragment != null) {
			commentListingFragment.onCreateOptionsMenu(menu);
		}

		return true;
	}

	public void onRefreshComments() {
		commentListingController.setSession(null);
		requestRefresh(RefreshableFragment.COMMENTS, true);
	}

	public void onPastComments() {
		final SessionListDialog sessionListDialog = SessionListDialog.newInstance(commentListingController.getUri(), commentListingController.getSession(), SessionChangeListener.SessionChangeType.COMMENTS);
		sessionListDialog.show(getSupportFragmentManager(), null);
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
		sessionListDialog.show(getSupportFragmentManager(), null);
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

	@Override
	public void onPin() {

		if(postListingFragment == null) return;

		try {
			PrefsUtility.pref_pinned_subreddits_add(
					this,
					sharedPreferences,
					postListingFragment.getSubreddit().getCanonicalName());

		} catch(RedditSubreddit.InvalidSubredditNameException e) {
			throw new RuntimeException(e);
		}

		invalidateOptionsMenu();
	}

	@Override
	public void onUnpin() {

		if(postListingFragment == null) return;

		try {
			PrefsUtility.pref_pinned_subreddits_remove(
					this,
					sharedPreferences,
					postListingFragment.getSubreddit().getCanonicalName());

		} catch(RedditSubreddit.InvalidSubredditNameException e) {
			throw new RuntimeException(e);
		}

		invalidateOptionsMenu();
	}

	@Override
	public void onBlock() {
		if(postListingFragment == null) return;

		try {
			PrefsUtility.pref_blocked_subreddits_add(
					this,
					sharedPreferences,
					postListingFragment.getSubreddit().getCanonicalName());

		} catch(RedditSubreddit.InvalidSubredditNameException e) {
			throw new RuntimeException(e);
		}

		invalidateOptionsMenu();
	}

	@Override
	public void onUnblock() {
		if(postListingFragment == null) return;

		try {
			PrefsUtility.pref_blocked_subreddits_remove(
					this,
					sharedPreferences,
					postListingFragment.getSubreddit().getCanonicalName());

		} catch(RedditSubreddit.InvalidSubredditNameException e) {
			throw new RuntimeException(e);
		}

		invalidateOptionsMenu();
	}

	public void onRefreshSubreddits() {
		requestRefresh(RefreshableFragment.MAIN, true);
	}

	@Override
	public boolean onOptionsItemSelected(final MenuItem item) {

		if(commentListingFragment != null) {
			commentListingFragment.onOptionsItemSelected(item);
		}

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
