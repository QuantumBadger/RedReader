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

package org.saiditnet.redreader.activities;

import android.app.AlertDialog;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.SharedPreferences;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.os.Bundle;
import android.preference.PreferenceManager;
import android.support.annotation.Nullable;
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
import android.widget.TextView;

import org.apache.commons.lang3.StringUtils;
import org.saiditnet.redreader.R;
import org.saiditnet.redreader.account.RedditAccount;
import org.saiditnet.redreader.account.RedditAccountChangeListener;
import org.saiditnet.redreader.account.RedditAccountManager;
import org.saiditnet.redreader.adapters.MainMenuSelectionListener;
import org.saiditnet.redreader.cache.CacheManager;
import org.saiditnet.redreader.common.DialogUtils;
import org.saiditnet.redreader.common.General;
import org.saiditnet.redreader.common.LinkHandler;
import org.saiditnet.redreader.common.PrefsUtility;
import org.saiditnet.redreader.fragments.AccountListDialog;
import org.saiditnet.redreader.fragments.ChangelogDialog;
import org.saiditnet.redreader.fragments.CommentListingFragment;
import org.saiditnet.redreader.fragments.MainMenuFragment;
import org.saiditnet.redreader.fragments.PostListingFragment;
import org.saiditnet.redreader.fragments.SessionListDialog;
import org.saiditnet.redreader.listingcontrollers.CommentListingController;
import org.saiditnet.redreader.listingcontrollers.PostListingController;
import org.saiditnet.redreader.reddit.PostSort;
import org.saiditnet.redreader.reddit.RedditSubredditHistory;
import org.saiditnet.redreader.reddit.api.RedditSubredditSubscriptionManager;
import org.saiditnet.redreader.reddit.prepared.RedditPreparedPost;
import org.saiditnet.redreader.reddit.things.RedditSubreddit;
import org.saiditnet.redreader.reddit.url.PostCommentListingURL;
import org.saiditnet.redreader.reddit.url.PostListingURL;
import org.saiditnet.redreader.reddit.url.RedditURLParser;
import org.saiditnet.redreader.reddit.url.SearchPostListURL;
import org.saiditnet.redreader.reddit.url.SubredditPostListURL;
import org.saiditnet.redreader.reddit.url.UserCommentListingURL;
import org.saiditnet.redreader.reddit.url.UserPostListingURL;
import org.saiditnet.redreader.reddit.url.UserProfileURL;
import org.saiditnet.redreader.views.RedditPostView;

import java.util.Locale;
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
	protected boolean baseActivityIsActionBarBackEnabled() {
		return false;
	}

	@Override
	protected void onCreate(final Bundle savedInstanceState) {

		PrefsUtility.applyTheme(this);

		super.onCreate(savedInstanceState);

		if(!isTaskRoot()
				&& getIntent().hasCategory(Intent.CATEGORY_LAUNCHER)
				&& getIntent().getAction() != null
				&& getIntent().getAction().equals(Intent.ACTION_MAIN)) {

			// Workaround for issue where a new MainActivity is created despite the app already running

			finish();
			return;
		}

		sharedPreferences = PreferenceManager.getDefaultSharedPreferences(this);
		if (savedInstanceState == null) {
			if(PrefsUtility.pref_behaviour_skiptofrontpage(this, sharedPreferences))
				onSelected(SubredditPostListURL.getFrontPage());
		}

		setTitle(R.string.app_name);

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

				if(lastVersion <= 76) {
					// Upgrading from v1.9.6.1 or lower, enable image sharing from post context menu

					final Set<String> existingPostContextItems = PrefsUtility.getStringSet(
							R.string.pref_menus_post_context_items_key,
							R.array.pref_menus_post_context_items_return,
							this,
							sharedPreferences
					);

					existingPostContextItems.add("share_image");

					sharedPreferences.edit().putStringSet(
							getString(R.string.pref_menus_post_context_items_key),
							existingPostContextItems
					).apply();

				}

				if (lastVersion <= 77) {

					// Upgrading from 77/1.9.7 or lower, enable pinning/subscribing/blocking a
					// subreddit and editing self-posts in the post context menu

					final Set<String> existingPostContextItems = PrefsUtility.getStringSet(
							R.string.pref_menus_post_context_items_key,
							R.array.pref_menus_post_context_items_return,
							this,
							sharedPreferences
					);

					existingPostContextItems.add("edit");
					existingPostContextItems.add("pin");
					existingPostContextItems.add("subscribe");
					existingPostContextItems.add("block");

					sharedPreferences.edit().putStringSet(
							getString(R.string.pref_menus_post_context_items_key),
							existingPostContextItems
					).apply();

				}

				if(lastVersion <= 84){

					// Upgrading from 84/1.9.8.5 or lower, change CheckBoxPreferences for
					// Main Menu Shortcuts into new MultiSelectListPreferences

					final Set<String> existingShortcutPreferences = PrefsUtility.getStringSet(
							R.string.pref_menus_mainmenu_shortcutitems_key,
							R.array.pref_menus_mainmenu_shortcutitems_items_default,
							this,
							sharedPreferences
					);

					if(PrefsUtility.pref_show_popular_main_menu(
							this,
							sharedPreferences
					)) existingShortcutPreferences.add("popular");


					if(PrefsUtility.pref_show_random_main_menu(
							this,
							sharedPreferences
					)) existingShortcutPreferences.add("random");

					sharedPreferences.edit().putStringSet(
							getString(R.string.pref_menus_mainmenu_shortcutitems_key),
							existingShortcutPreferences
					).apply();
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

	@Override
	public void onSelected(final @MainMenuFragment.MainMenuAction int type) {

		final String username = RedditAccountManager.getInstance(this).getDefaultAccount().username;

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

			case MainMenuFragment.MENU_MENU_ACTION_SUBSCRIBED:
				onSelected(SubredditPostListURL.getSubscribed());
				break;

			case MainMenuFragment.MENU_MENU_ACTION_CUSTOM: {

				final AlertDialog.Builder alertBuilder = new AlertDialog.Builder(this);
				final View root = getLayoutInflater().inflate(R.layout.dialog_mainmenu_custom, null);

				final Spinner destinationType = (Spinner)root.findViewById(R.id.dialog_mainmenu_custom_type);
				final AutoCompleteTextView editText = (AutoCompleteTextView)root.findViewById(R.id.dialog_mainmenu_custom_value);

				final String[] typeReturnValues
						= getResources().getStringArray(R.array.mainmenu_custom_destination_type_return);

				final ArrayAdapter<String> autocompleteAdapter = new ArrayAdapter<>(
						this,
						android.R.layout.simple_dropdown_item_1line,
						RedditSubredditHistory.getSubredditsSorted(RedditAccountManager.getInstance(this).getDefaultAccount()).toArray(new String[] {}));

				editText.setAdapter(autocompleteAdapter);
				editText.setOnEditorActionListener(new TextView.OnEditorActionListener() {
					@Override
					public boolean onEditorAction(TextView v, int actionId, KeyEvent event) {
						boolean handled = false;
						if(actionId == EditorInfo.IME_ACTION_GO) {
							openCustomLocation(typeReturnValues, destinationType, editText);
							handled = true;
						}
						return handled;
					}
				});

				alertBuilder.setView(root);

				destinationType.setOnItemSelectedListener(new AdapterView.OnItemSelectedListener()
				{
					@Override
					public void onItemSelected(
							final AdapterView<?> adapterView,
							final View view,
							final int i,
							final long l)
					{
						final String typeName = typeReturnValues[destinationType.getSelectedItemPosition()];

						switch(typeName)
						{
							case "subreddit":
								editText.setAdapter(autocompleteAdapter);
								break;

							default:
								editText.setAdapter(null);
								break;
						}
					}

					@Override
					public void onNothingSelected(final AdapterView<?> adapterView)
					{
						editText.setAdapter(null);
					}
				});

				alertBuilder.setPositiveButton(R.string.dialog_go, new DialogInterface.OnClickListener() {
					@Override
					public void onClick(DialogInterface dialog, int which) {
						openCustomLocation(typeReturnValues, destinationType, editText);
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

	private void openCustomLocation(String[] typeReturnValues, Spinner destinationType, AutoCompleteTextView editText) {

		final String typeName = typeReturnValues[destinationType.getSelectedItemPosition()];

		switch(typeName) {
            case "subreddit": {

                final String subredditInput = editText.getText().toString().trim().replace(" ", "");

                try {
                    final String normalizedName = RedditSubreddit.stripRPrefix(subredditInput);
                    final RedditURLParser.RedditURL redditURL = SubredditPostListURL.getSubreddit(normalizedName);
                    if(redditURL == null || redditURL.pathType() != RedditURLParser.SUBREDDIT_POST_LISTING_URL) {
                        General.quickToast(this, R.string.mainmenu_custom_invalid_name);
                    } else {
                        onSelected(redditURL.asSubredditPostListURL());
                    }
                } catch(RedditSubreddit.InvalidSubredditNameException e){
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
				String query = editText.getText().toString().trim();

				if (StringUtils.isEmpty(query)) {
					General.quickToast(this, R.string.mainmenu_custom_empty_search_query);
					break;
				}

				SearchPostListURL url = SearchPostListURL.build(null, query);

				final Intent intent = new Intent(this, PostListingActivity.class);
				intent.setData(url.generateJsonUri());
				this.startActivity(intent);
				break;
			}
        }
	}

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

		showBackButton(false);
		invalidateOptionsMenu();
	}

	public void onPostCommentsSelected(final RedditPreparedPost post) {

		if(twoPane) {

			commentListingController = new CommentListingController(PostCommentListingURL.forPostId(post.src.getIdAlone()), this);
			showBackButton(true);

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

		final boolean isFrontPage = postListingController != null && postListingController.isFrontPage();

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
				false,
				false,
				postsSortable,
				commentsSortable,
				isFrontPage,
				subredditSubscriptionState,
				postsVisible && subredditDescription != null && subredditDescription.length() > 0,
				true,
				subredditPinState,
				subredditBlockedState);

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

	public void onSortSelected(final UserCommentListingURL.Sort order) {
		commentListingController.setSort(order);
		requestRefresh(RefreshableFragment.COMMENTS, false);
	}

	@Override
	public void onSearchComments() {
		DialogUtils.showSearchDialog(this, R.string.action_search_comments, new DialogUtils.OnSearchListener() {
			@Override
			public void onSearch(@Nullable String query) {
				Intent searchIntent = new Intent(MainActivity.this, CommentListingActivity.class);
				searchIntent.setData(commentListingController.getUri());
				searchIntent.putExtra(CommentListingActivity.EXTRA_SEARCH_STRING, query);
				startActivity(searchIntent);
			}
		});
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

	public void onSortSelected(final PostSort order) {
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
		intent.putExtra("title", String.format(
				Locale.US, "%s: %s",
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

	private void showBackButton(boolean isVisible) {
		configBackButton(isVisible, new View.OnClickListener() {
			@Override
			public void onClick(final View v) {
				onBackPressed();
			}
		});
	}
}
