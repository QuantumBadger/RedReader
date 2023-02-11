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
import android.content.Intent;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.os.Bundle;
import android.text.Editable;
import android.text.TextWatcher;
import android.util.Log;
import android.view.KeyEvent;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.view.WindowManager;
import android.view.inputmethod.EditorInfo;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.AutoCompleteTextView;
import android.widget.FrameLayout;
import android.widget.Spinner;
import androidx.annotation.NonNull;
import org.apache.commons.lang3.StringUtils;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.account.RedditAccountChangeListener;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.adapters.MainMenuSelectionListener;
import org.quantumbadger.redreader.common.DialogUtils;
import org.quantumbadger.redreader.common.FeatureFlagHandler;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.LinkHandler;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.common.SharedPrefsWrapper;
import org.quantumbadger.redreader.common.collections.CollectionStream;
import org.quantumbadger.redreader.fragments.AccountListDialog;
import org.quantumbadger.redreader.fragments.ChangelogDialog;
import org.quantumbadger.redreader.fragments.CommentListingFragment;
import org.quantumbadger.redreader.fragments.MainMenuFragment;
import org.quantumbadger.redreader.fragments.PostListingFragment;
import org.quantumbadger.redreader.fragments.SessionListDialog;
import org.quantumbadger.redreader.listingcontrollers.CommentListingController;
import org.quantumbadger.redreader.listingcontrollers.PostListingController;
import org.quantumbadger.redreader.reddit.PostCommentSort;
import org.quantumbadger.redreader.reddit.PostSort;
import org.quantumbadger.redreader.reddit.RedditSubredditHistory;
import org.quantumbadger.redreader.reddit.UserCommentSort;
import org.quantumbadger.redreader.reddit.api.RedditSubredditSubscriptionManager;
import org.quantumbadger.redreader.reddit.api.SubredditSubscriptionState;
import org.quantumbadger.redreader.reddit.prepared.RedditPreparedPost;
import org.quantumbadger.redreader.reddit.things.InvalidSubredditNameException;
import org.quantumbadger.redreader.reddit.things.RedditSubreddit;
import org.quantumbadger.redreader.reddit.things.SubredditCanonicalId;
import org.quantumbadger.redreader.reddit.url.PostCommentListingURL;
import org.quantumbadger.redreader.reddit.url.PostListingURL;
import org.quantumbadger.redreader.reddit.url.RedditURLParser;
import org.quantumbadger.redreader.reddit.url.SearchPostListURL;
import org.quantumbadger.redreader.reddit.url.SubredditPostListURL;
import org.quantumbadger.redreader.reddit.url.UserPostListingURL;
import org.quantumbadger.redreader.reddit.url.UserProfileURL;
import org.quantumbadger.redreader.views.RedditPostView;

import java.util.ArrayList;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicReference;

public class MainActivity extends RefreshableActivity
		implements MainMenuSelectionListener,
		RedditAccountChangeListener,
		RedditPostView.PostSelectionListener,
		OptionsMenuUtility.OptionsMenuSubredditsListener,
		OptionsMenuUtility.OptionsMenuPostsListener,
		OptionsMenuUtility.OptionsMenuCommentsListener,
		SessionChangeListener,
		RedditSubredditSubscriptionManager.SubredditSubscriptionStateChangeListener {

	private static final String TAG = "MainActivity";

	private boolean twoPane;

	private MainMenuFragment mainMenuFragment;

	private PostListingController postListingController;
	private PostListingFragment postListingFragment;

	private CommentListingController commentListingController;
	private CommentListingFragment commentListingFragment;

	private View mainMenuView;
	private View postListingView;
	private View commentListingView;

	private FrameLayout mLeftPane;
	private FrameLayout mRightPane;

	private boolean isMenuShown = true;

	private final AtomicReference<RedditSubredditSubscriptionManager.ListenerContext>
			mSubredditSubscriptionListenerContext = new AtomicReference<>(null);

	@Override
	protected boolean baseActivityIsActionBarBackEnabled() {
		return false;
	}

	@Override
	protected boolean baseActivityAllowToolbarHideOnScroll() {
		return !General.isTablet(this);
	}

	@Override
	protected void onCreate(final Bundle savedInstanceState) {

		PrefsUtility.applyTheme(this);

		super.onCreate(savedInstanceState);

		if(!isTaskRoot()
				&& getIntent().hasCategory(Intent.CATEGORY_LAUNCHER)
				&& getIntent().getAction() != null
				&& getIntent().getAction().equals(Intent.ACTION_MAIN)) {

			// Workaround for issue where a new MainActivity is created despite
			// the app already running

			finish();
			return;
		}

		final SharedPrefsWrapper sharedPreferences = General.getSharedPrefs(this);
		twoPane = General.isTablet(this);

		setTitle(R.string.app_name);

		RedditAccountManager.getInstance(this).addUpdateListener(this);

		final PackageInfo pInfo;
		try {
			pInfo = getPackageManager().getPackageInfo(getPackageName(), 0);
		} catch(final PackageManager.NameNotFoundException e) {
			throw new RuntimeException(e);
		}

		final int appVersion = pInfo.versionCode;

		Log.i(TAG, "[Migration] App version: " + appVersion);

		if(!sharedPreferences.contains(FeatureFlagHandler.PREF_FIRST_RUN_MESSAGE_SHOWN)) {

			Log.i(TAG, "[Migration] Showing first run message");

			FeatureFlagHandler.handleFirstInstall(sharedPreferences);

			new AlertDialog.Builder(this)
					.setTitle(R.string.firstrun_login_title)
					.setMessage(R.string.firstrun_login_message)
					.setPositiveButton(
							R.string.firstrun_login_button_now,
							(dialog, which) -> new AccountListDialog().show(
									this.getSupportFragmentManager(),
									null))
					.setNegativeButton(R.string.firstrun_login_button_later, null)
					.show();

			sharedPreferences.edit()
					.putString(FeatureFlagHandler.PREF_FIRST_RUN_MESSAGE_SHOWN, "true")
					.putInt(FeatureFlagHandler.PREF_LAST_VERSION, appVersion)
					.apply();

		} else if(sharedPreferences.contains(FeatureFlagHandler.PREF_LAST_VERSION)) {
			FeatureFlagHandler.handleLegacyUpgrade(this, appVersion, pInfo.versionName);

		} else {
			Log.i(TAG, "[Migration] Last version not set.");
			sharedPreferences.edit()
					.putInt(FeatureFlagHandler.PREF_LAST_VERSION, appVersion)
					.apply();
			ChangelogDialog.newInstance().show(getSupportFragmentManager(), null);
		}

		FeatureFlagHandler.handleUpgrade(this);

		recreateSubscriptionListener();

		final boolean startInbox = getIntent().getBooleanExtra("isNewMessage", false);
		if(startInbox) {
			startActivity(new Intent(this, InboxListingActivity.class));
		}

		doRefresh(RefreshableFragment.MAIN_RELAYOUT, false, null);

		if(savedInstanceState == null
				&& PrefsUtility.pref_behaviour_skiptofrontpage()) {
			onSelected(SubredditPostListURL.getFrontPage());
		}
	}

	private void recreateSubscriptionListener() {

		final RedditSubredditSubscriptionManager.ListenerContext oldContext
				= mSubredditSubscriptionListenerContext.getAndSet(
				RedditSubredditSubscriptionManager
						.getSingleton(
								this,
								RedditAccountManager.getInstance(this)
										.getDefaultAccount())
						.addListener(this));

		if(oldContext != null) {
			oldContext.removeListener();
		}
	}

	@Override
	protected void onDestroy() {
		super.onDestroy();

		final RedditSubredditSubscriptionManager.ListenerContext listenerContext
				= mSubredditSubscriptionListenerContext.get();

		if(listenerContext != null) {
			listenerContext.removeListener();
		}
	}

	@Override
	public void onSelected(final @MainMenuFragment.MainMenuAction int type) {

		final String username = RedditAccountManager.getInstance(this)
				.getDefaultAccount().username;

		switch(type) {

			case MainMenuFragment.MENU_MENU_ACTION_FRONTPAGE:
				onSelected(SubredditPostListURL.getFrontPage());
				break;

			case MainMenuFragment.MENU_MENU_ACTION_POPULAR:
				onSelected(SubredditPostListURL.getPopular());
				break;

			case MainMenuFragment.MENU_MENU_ACTION_RANDOM:
				onSelected(SubredditPostListURL.getRandom());
				break;

			case MainMenuFragment.MENU_MENU_ACTION_RANDOM_NSFW:
				onSelected(SubredditPostListURL.getRandomNsfw());
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
				LinkHandler.onLinkClicked(this, new UserProfileURL(username).toString());
				break;

			case MainMenuFragment.MENU_MENU_ACTION_CUSTOM: {

				final AlertDialog.Builder alertBuilder = new AlertDialog.Builder(this);
				final View root = getLayoutInflater().inflate(
						R.layout.dialog_mainmenu_custom,
						null);

				final Spinner destinationType
						= root.findViewById(R.id.dialog_mainmenu_custom_type);
				final AutoCompleteTextView editText
						= root.findViewById(R.id.dialog_mainmenu_custom_value);

				final String[] typeReturnValues = getResources().getStringArray(
						R.array.mainmenu_custom_destination_type_return);

				if(PrefsUtility.pref_menus_mainmenu_shortcutitems().contains(
						MainMenuFragment.MainMenuShortcutItems.SUBREDDIT_SEARCH)) {

					for(int i = 0; i < typeReturnValues.length; i++) {
						if(typeReturnValues[i].equals("user")) {
							destinationType.setSelection(i);
							break;
						}
					}
				}

				final ArrayList<SubredditCanonicalId> subredditHistory
						= RedditSubredditHistory.getSubredditsSorted(
						RedditAccountManager.getInstance(this).getDefaultAccount());

				final ArrayAdapter<String> autocompleteAdapter = new ArrayAdapter<>(
						this,
						android.R.layout.simple_dropdown_item_1line,
						new CollectionStream<>(subredditHistory)
								.map(SubredditCanonicalId::getDisplayNameLowercase)
								.collect(new ArrayList<>()));

				editText.setAdapter(autocompleteAdapter);
				editText.setOnEditorActionListener((v, actionId, event) -> {
					boolean handled = false;
					if(actionId == EditorInfo.IME_ACTION_GO
							|| event.getKeyCode() == KeyEvent.KEYCODE_ENTER) {
						openCustomLocation(
								typeReturnValues,
								destinationType,
								editText);
						handled = true;
					}
					return handled;
				});

				alertBuilder.setView(root);

				editText.addTextChangedListener(new TextWatcher() {
					@Override
					public void beforeTextChanged(
							final CharSequence s,
							final int start,
							final int count,
							final int after) {}

					@Override
					public void onTextChanged(
							final CharSequence s,
							final int start,
							final int before,
							final int count) {

						if(typeReturnValues[destinationType.getSelectedItemPosition()]
								.equals("search")) {

							return;
						}

						final String value = s.toString();
						String type = null;

						if(value.startsWith("http://") || value.startsWith("https://")) {
							type = "url";

						} else if(value.startsWith("/r/") || value.startsWith("r/")) {
							type = "subreddit";

						} else if(value.startsWith("/u/") || value.startsWith("u/")) {
							type = "user";
						}

						if(type != null) {
							for(int i = 0; i < typeReturnValues.length; i++) {
								if(typeReturnValues[i].equals(type)) {
									destinationType.setSelection(i);
									break;
								}
							}
						}
					}

					@Override
					public void afterTextChanged(final Editable s) {}
				});

				destinationType.setOnItemSelectedListener(new AdapterView.OnItemSelectedListener() {
					@Override
					public void onItemSelected(
							final AdapterView<?> adapterView,
							final View view,
							final int i,
							final long l) {

						final String typeName
								= typeReturnValues[destinationType.getSelectedItemPosition()];

						if("subreddit".equals(typeName)) {
							editText.setAdapter(autocompleteAdapter);
						} else {
							editText.setAdapter(null);
						}
					}

					@Override
					public void onNothingSelected(final AdapterView<?> adapterView) {
						editText.setAdapter(null);
					}
				});

				alertBuilder.setPositiveButton(
						R.string.dialog_go,
						(dialog, which) -> openCustomLocation(
								typeReturnValues,
								destinationType,
								editText));

				alertBuilder.setNegativeButton(R.string.dialog_cancel, null);

				final AlertDialog alertDialog = alertBuilder.create();
				alertDialog.getWindow()
						.setSoftInputMode(WindowManager.LayoutParams.SOFT_INPUT_STATE_VISIBLE);
				alertDialog.show();

				break;
			}

			case MainMenuFragment.MENU_MENU_ACTION_INBOX:
				startActivity(new Intent(this, InboxListingActivity.class));
				break;

			case MainMenuFragment.MENU_MENU_ACTION_SENT_MESSAGES: {
				final Intent intent = new Intent(this, InboxListingActivity.class);
				intent.putExtra("inboxType", "sent");
				startActivity(intent);
				break;
			}

			case MainMenuFragment.MENU_MENU_ACTION_MODMAIL: {
				final Intent intent = new Intent(this, InboxListingActivity.class);
				intent.putExtra("inboxType", "modmail");
				startActivity(intent);
				break;
			}

			case MainMenuFragment.MENU_MENU_ACTION_FIND_SUBREDDIT: {
				startActivity(new Intent(this, SubredditSearchActivity.class));
			}
		}
	}

	private void openCustomLocation(
			final String[] typeReturnValues,
			final Spinner destinationType,
			final AutoCompleteTextView editText) {

		final String typeName
				= typeReturnValues[destinationType.getSelectedItemPosition()];

		switch(typeName) {
			case "subreddit": {

				final String subredditInput = editText.getText()
						.toString()
						.trim()
						.replace(" ", "");

				try {
					final String normalizedName = RedditSubreddit.stripRPrefix(
							subredditInput);
					final RedditURLParser.RedditURL redditURL
							= SubredditPostListURL.getSubreddit(normalizedName);
					if(redditURL == null
							|| redditURL.pathType()
							!= RedditURLParser.SUBREDDIT_POST_LISTING_URL) {
						General.quickToast(this, R.string.mainmenu_custom_invalid_name);
					} else {
						onSelected(redditURL.asSubredditPostListURL());
					}
				} catch(final InvalidSubredditNameException e) {
					General.quickToast(this, R.string.mainmenu_custom_invalid_name);
				}
				break;
			}

			case "user":

				String userInput = editText.getText().toString().trim().replace(" ", "");

				if(!userInput.startsWith("/u/")
						&& !userInput.startsWith("/user/")) {

					if(userInput.startsWith("u/")
							|| userInput.startsWith("user/")) {

						userInput = "/" + userInput;

					} else {
						userInput = "/u/" + userInput;
					}
				}

				LinkHandler.onLinkClicked(this, userInput);

				break;

			case "url": {
				LinkHandler.onLinkClicked(this, editText.getText().toString().trim());
				break;
			}

			case "search": {
				final String query = editText.getText().toString().trim();

				if(StringUtils.isEmpty(query)) {
					General.quickToast(this, R.string.mainmenu_custom_empty_search_query);
					break;
				}

				final SearchPostListURL url = SearchPostListURL.build(null, query);

				final Intent intent = new Intent(this, PostListingActivity.class);
				intent.setData(url.generateJsonUri());
				this.startActivity(intent);
				break;
			}
		}
	}

	@Override
	public void onSelected(final PostListingURL url) {

		if(url == null) {
			return;
		}

		if(twoPane) {

			postListingController = new PostListingController(url, this);
			requestRefresh(RefreshableFragment.POSTS, false);

		} else {
			final Intent intent = new Intent(this, PostListingActivity.class);
			intent.setData(url.generateJsonUri());
			startActivityForResult(intent, 1);
		}
	}

	@Override
	public void onRedditAccountChanged() {
		recreateSubscriptionListener();
		postInvalidateOptionsMenu();
		requestRefresh(RefreshableFragment.ALL, false);
	}

	@Override
	protected void doRefresh(
			final RefreshableFragment which,
			final boolean force,
			final Bundle savedInstanceState) {

		if(which == RefreshableFragment.MAIN_RELAYOUT) {

			mainMenuFragment = null;
			postListingFragment = null;
			commentListingFragment = null;

			mainMenuView = null;
			postListingView = null;
			commentListingView = null;

			if(mLeftPane != null) {
				mLeftPane.removeAllViews();
			}
			if(mRightPane != null) {
				mRightPane.removeAllViews();
			}

			twoPane = General.isTablet(this);

			if(twoPane) {
				final View layout = getLayoutInflater().inflate(R.layout.main_double, null);
				mLeftPane = layout.findViewById(R.id.main_left_frame);
				mRightPane = layout.findViewById(R.id.main_right_frame);
				setBaseActivityListing(layout);

			} else {
				mLeftPane = null;
				mRightPane = null;
			}

			invalidateOptionsMenu();
			requestRefresh(RefreshableFragment.ALL, false);

			return;
		}

		if(twoPane) {

			final FrameLayout postContainer = isMenuShown ? mRightPane : mLeftPane;

			if(isMenuShown && (which == RefreshableFragment.ALL
					|| which == RefreshableFragment.MAIN)) {
				mainMenuFragment = new MainMenuFragment(this, null, force);
				mainMenuView = mainMenuFragment.createCombinedListingAndOverlayView();
				mLeftPane.removeAllViews();
				mLeftPane.addView(mainMenuView);
			}

			if(postListingController != null && (which == RefreshableFragment.ALL
					|| which == RefreshableFragment.POSTS)) {
				if(force && postListingFragment != null) {
					postListingFragment.cancel();
				}
				postListingFragment = postListingController.get(this, force, null);
				postListingView = postListingFragment.createCombinedListingAndOverlayView();
				postContainer.removeAllViews();
				postContainer.addView(postListingView);
			}

			if(commentListingController != null && (which == RefreshableFragment.ALL
					|| which
					== RefreshableFragment.COMMENTS)) {
				commentListingFragment = commentListingController.get(this, force, null);
				commentListingView = commentListingFragment.createCombinedListingAndOverlayView();
				mRightPane.removeAllViews();
				mRightPane.addView(commentListingView);
			}

		} else {

			if(which == RefreshableFragment.ALL || which == RefreshableFragment.MAIN) {
				mainMenuFragment = new MainMenuFragment(this, null, force);
				mainMenuFragment.setBaseActivityContent(this);
			}
		}

		invalidateOptionsMenu();
	}

	@Override
	public void onBackPressed() {

		if(!General.onBackPressed()) {
			return;
		}

		if(!twoPane || isMenuShown) {
			super.onBackPressed();
			return;
		}

		isMenuShown = true;

		mainMenuFragment = new MainMenuFragment(
				this,
				null,
				false); // TODO preserve position
		mainMenuView = mainMenuFragment.createCombinedListingAndOverlayView();

		commentListingFragment = null;
		commentListingView = null;

		mLeftPane.removeAllViews();
		mRightPane.removeAllViews();

		mLeftPane.addView(mainMenuView);
		mRightPane.addView(postListingView);

		showBackButton(false);
		invalidateOptionsMenu();
	}

	@Override
	public void onPostCommentsSelected(final RedditPreparedPost post) {

		if(twoPane) {

			commentListingController
					= new CommentListingController(
					PostCommentListingURL.forPostId(post.src
							.getIdAlone()));
			showBackButton(true);

			if(isMenuShown) {

				commentListingFragment = commentListingController.get(this, false, null);
				commentListingView = commentListingFragment.createCombinedListingAndOverlayView();

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
			LinkHandler.onLinkClicked(
					this,
					PostCommentListingURL.forPostId(post.src.getIdAlone()).toString(),
					false);
		}
	}

	@Override
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

		final boolean postsSortable = postListingController != null
				&& postListingController.isSortable();
		final boolean commentsSortable = commentListingController != null
				&& commentListingController.isSortable();

		final boolean isFrontPage = postListingController != null && postListingController
				.isFrontPage();

		final RedditAccount user = RedditAccountManager.getInstance(this)
				.getDefaultAccount();
		final SubredditSubscriptionState
				subredditSubscriptionState;
		final RedditSubredditSubscriptionManager subredditSubscriptionManager
				= RedditSubredditSubscriptionManager.getSingleton(this, user);

		Boolean subredditPinState = null;
		Boolean subredditBlockedState = null;

		if(postsVisible
				&& !user.isAnonymous()
				&& (postListingController.isSubreddit()
				|| postListingController.isRandomSubreddit())
				&& subredditSubscriptionManager.areSubscriptionsReady()
				&& postListingFragment != null
				&& postListingFragment.getSubreddit() != null) {

			subredditSubscriptionState
					= subredditSubscriptionManager.getSubscriptionState(
					postListingController.subredditCanonicalName());

		} else {
			subredditSubscriptionState = null;
		}

		if(postsVisible
				&& (postListingController.isSubreddit()
				|| postListingController.isRandomSubreddit())
				&& postListingFragment != null
				&& postListingFragment.getSubreddit() != null) {

			try {
				subredditPinState = PrefsUtility.pref_pinned_subreddits_check(
						postListingFragment.getSubreddit().getCanonicalId());

				subredditBlockedState = PrefsUtility.pref_blocked_subreddits_check(
						postListingFragment.getSubreddit().getCanonicalId());

			} catch(final InvalidSubredditNameException e) {
				subredditPinState = null;
				subredditBlockedState = null;
			}
		}

		final String subredditDescription = postListingFragment != null
				&& postListingFragment.getSubreddit() != null
				? postListingFragment.getSubreddit().description_html
				: null;

		OptionsMenuUtility.prepare(
				this,
				menu,
				isMenuShown,
				postsVisible,
				commentsVisible,
				false,
				false,
				false,
				postsSortable,
				commentsSortable,
				isFrontPage,
				subredditSubscriptionState,
				postsVisible
						&& subredditDescription != null
						&& !subredditDescription.isEmpty(),
				true,
				subredditPinState,
				subredditBlockedState);

		if(commentListingFragment != null) {
			commentListingFragment.onCreateOptionsMenu(menu);
		}

		return true;
	}

	@Override
	public void onRefreshComments() {
		commentListingController.setSession(null);
		requestRefresh(RefreshableFragment.COMMENTS, true);
	}

	@Override
	public void onPastComments() {
		final SessionListDialog sessionListDialog = SessionListDialog.newInstance(
				commentListingController.getUri(),
				commentListingController.getSession(),
				SessionChangeListener.SessionChangeType.COMMENTS);
		sessionListDialog.show(getSupportFragmentManager(), null);
	}

	@Override
	public void onSortSelected(final PostCommentSort order) {
		commentListingController.setSort(order);
		requestRefresh(RefreshableFragment.COMMENTS, false);
	}

	@Override
	public void onSortSelected(final UserCommentSort order) {
		commentListingController.setSort(order);
		requestRefresh(RefreshableFragment.COMMENTS, false);
	}

	@Override
	public void onSearchComments() {
		DialogUtils.showSearchDialog(
				this,
				R.string.action_search_comments,
				query -> {
					final Intent searchIntent
							= new Intent(this, CommentListingActivity.class);
					searchIntent.setData(commentListingController.getUri());
					searchIntent.putExtra(
							CommentListingActivity.EXTRA_SEARCH_STRING,
							query);
					startActivity(searchIntent);
				});
	}

	@Override
	public void onRefreshPosts() {
		postListingController.setSession(null);
		requestRefresh(RefreshableFragment.POSTS, true);
	}

	@Override
	public void onPastPosts() {
		final SessionListDialog sessionListDialog = SessionListDialog.newInstance(
				postListingController.getUri(),
				postListingController.getSession(),
				SessionChangeListener.SessionChangeType.POSTS);
		sessionListDialog.show(getSupportFragmentManager(), null);
	}

	@Override
	public void onSubmitPost() {
		final Intent intent = new Intent(this, PostSubmitActivity.class);
		if(postListingController.isSubreddit()) {
			intent.putExtra(
					"subreddit",
					postListingController.subredditCanonicalName().toString());
		}
		startActivity(intent);
	}

	@Override
	public void onSortSelected(final PostSort order) {
		postListingController.setSort(order);
		requestRefresh(RefreshableFragment.POSTS, false);
	}

	@Override
	public void onSearchPosts() {
		PostListingActivity.onSearchPosts(postListingController, this);
	}

	@Override
	public void onSubscribe() {
		if(postListingFragment != null) {
			postListingFragment.onSubscribe();
		}
	}

	@Override
	public void onUnsubscribe() {
		if(postListingFragment != null) {
			postListingFragment.onUnsubscribe();
		}
	}

	@Override
	public void onSidebar() {
		postListingFragment.getSubreddit().showSidebarActivity(this);
	}

	@Override
	public void onPin() {

		if(postListingFragment == null) {
			return;
		}

		try {
			PrefsUtility.pref_pinned_subreddits_add(
					this,
					postListingFragment.getSubreddit().getCanonicalId());

		} catch(final InvalidSubredditNameException e) {
			throw new RuntimeException(e);
		}

		invalidateOptionsMenu();
	}

	@Override
	public void onUnpin() {

		if(postListingFragment == null) {
			return;
		}

		try {
			PrefsUtility.pref_pinned_subreddits_remove(
					this,
					postListingFragment.getSubreddit().getCanonicalId());

		} catch(final InvalidSubredditNameException e) {
			throw new RuntimeException(e);
		}

		invalidateOptionsMenu();
	}

	@Override
	public void onBlock() {
		if(postListingFragment == null) {
			return;
		}

		try {
			PrefsUtility.pref_blocked_subreddits_add(
					this,
					postListingFragment.getSubreddit().getCanonicalId());

		} catch(final InvalidSubredditNameException e) {
			throw new RuntimeException(e);
		}

		invalidateOptionsMenu();
	}

	@Override
	public void onUnblock() {
		if(postListingFragment == null) {
			return;
		}

		try {
			PrefsUtility.pref_blocked_subreddits_remove(
					this,
					postListingFragment.getSubreddit().getCanonicalId());

		} catch(final InvalidSubredditNameException e) {
			throw new RuntimeException(e);
		}

		invalidateOptionsMenu();
	}

	@Override
	public void onRefreshSubreddits() {
		requestRefresh(RefreshableFragment.MAIN, true);
	}

	@Override
	protected void onResume() {
		super.onResume();

		if(mainMenuFragment != null) {
			mainMenuFragment.onUpdateAnnouncement();
		}
	}

	@Override
	public boolean onOptionsItemSelected(@NonNull final MenuItem item) {

		if(commentListingFragment != null) {
			if(commentListingFragment.onOptionsItemSelected(item)) {
				return true;
			}
		}

		switch(item.getItemId()) {
			case android.R.id.home:
				onBackPressed();
				return true;
			default:
				return super.onOptionsItemSelected(item);
		}
	}

	@Override
	public void onSessionSelected(final UUID session, final SessionChangeType type) {

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

	@Override
	public void onSessionRefreshSelected(final SessionChangeType type) {
		switch(type) {
			case POSTS:
				onRefreshPosts();
				break;
			case COMMENTS:
				onRefreshComments();
				break;
		}
	}

	@Override
	public void onSessionChanged(
			final UUID session,
			final SessionChangeType type,
			final long timestamp) {

		switch(type) {
			case POSTS:
				if(postListingController != null) {
					postListingController.setSession(session);
				}
				break;
			case COMMENTS:
				if(commentListingController != null) {
					commentListingController.setSession(session);
				}
				break;
		}
	}

	@Override
	public void onSubredditSubscriptionListUpdated(
			final RedditSubredditSubscriptionManager subredditSubscriptionManager) {
		postInvalidateOptionsMenu();
	}

	@Override
	public void onSubredditSubscriptionAttempted(
			final RedditSubredditSubscriptionManager subredditSubscriptionManager) {
		postInvalidateOptionsMenu();
	}

	@Override
	public void onSubredditUnsubscriptionAttempted(
			final RedditSubredditSubscriptionManager subredditSubscriptionManager) {
		postInvalidateOptionsMenu();
	}

	private void postInvalidateOptionsMenu() {
		runOnUiThread(this::invalidateOptionsMenu);
	}

	private void showBackButton(final boolean isVisible) {
		configBackButton(isVisible, v -> onBackPressed());
	}

	@Override
	public PostSort getPostSort() {
		if(postListingController == null) {
			return null;
		}

		return postListingController.getSort();
	}

	@Override
	public OptionsMenuUtility.Sort getCommentSort() {
		if(commentListingController == null) {
			return null;
		}

		return commentListingController.getSort();
	}

	@Override
	public PostCommentSort getSuggestedCommentSort() {
		if(commentListingFragment == null || commentListingFragment.getPost() == null) {
			return null;
		}

		return commentListingFragment.getPost().src.getSuggestedCommentSort();
	}
}
