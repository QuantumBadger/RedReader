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
import android.widget.TextView;
import androidx.annotation.Nullable;
import org.apache.commons.lang3.StringUtils;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.account.RedditAccountChangeListener;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.adapters.MainMenuSelectionListener;
import org.quantumbadger.redreader.cache.CacheManager;
import org.quantumbadger.redreader.common.DialogUtils;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.LinkHandler;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.common.collections.CollectionStream;
import org.quantumbadger.redreader.fragments.AccountListDialog;
import org.quantumbadger.redreader.fragments.ChangelogDialog;
import org.quantumbadger.redreader.fragments.CommentListingFragment;
import org.quantumbadger.redreader.fragments.MainMenuFragment;
import org.quantumbadger.redreader.fragments.PostListingFragment;
import org.quantumbadger.redreader.fragments.SessionListDialog;
import org.quantumbadger.redreader.listingcontrollers.CommentListingController;
import org.quantumbadger.redreader.listingcontrollers.PostListingController;
import org.quantumbadger.redreader.reddit.PostSort;
import org.quantumbadger.redreader.reddit.RedditSubredditHistory;
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
import org.quantumbadger.redreader.reddit.url.UserCommentListingURL;
import org.quantumbadger.redreader.reddit.url.UserPostListingURL;
import org.quantumbadger.redreader.reddit.url.UserProfileURL;
import org.quantumbadger.redreader.views.RedditPostView;

import java.util.ArrayList;
import java.util.Set;
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

	private FrameLayout mSinglePane;

	private FrameLayout mLeftPane;
	private FrameLayout mRightPane;

	private boolean isMenuShown = true;

	private SharedPreferences sharedPreferences;

	private final AtomicReference<RedditSubredditSubscriptionManager.ListenerContext>
			mSubredditSubscriptionListenerContext = new AtomicReference<>(null);

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

			// Workaround for issue where a new MainActivity is created despite
			// the app already running

			finish();
			return;
		}

		sharedPreferences = PreferenceManager.getDefaultSharedPreferences(this);
		twoPane = General.isTablet(this, sharedPreferences);

		doRefresh(RefreshableFragment.MAIN_RELAYOUT, false, null);

		if(savedInstanceState == null) {
			if(PrefsUtility.pref_behaviour_skiptofrontpage(this, sharedPreferences)) {
				onSelected(SubredditPostListURL.getFrontPage());
			}
		}

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

		if(!sharedPreferences.contains("firstRunMessageShown")) {

			Log.i(TAG, "[Migration] Showing first run message");

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

			final SharedPreferences.Editor edit = sharedPreferences.edit();
			edit.putString("firstRunMessageShown", "true");
			edit.putInt("lastVersion", appVersion);
			edit.apply();

		} else if(sharedPreferences.contains("lastVersion")) {

			final int lastVersion = sharedPreferences.getInt("lastVersion", 0);

			Log.i(TAG, "[Migration] Last version: " + lastVersion);

			if(lastVersion < 63) {
				// Upgrading across the 1.9.0 boundary (when oAuth was introduced)

				new AlertDialog.Builder(this)
						.setTitle(R.string.firstrun_login_title)
						.setMessage(R.string.upgrade_v190_login_message)
						.setPositiveButton(
								R.string.firstrun_login_button_now,
								(dialog, which) -> new AccountListDialog().show(
										this.getSupportFragmentManager(),
										null))
						.setNegativeButton(R.string.firstrun_login_button_later, null)
						.show();
			}

			if(lastVersion != appVersion) {

				General.quickToast(
						this,
						String.format(
								getString(R.string.upgrade_message),
								pInfo.versionName));

				sharedPreferences.edit().putInt("lastVersion", appVersion).apply();
				ChangelogDialog.newInstance().show(getSupportFragmentManager(), null);

				if(lastVersion <= 51) {
					// Upgrading from v1.8.6.3 or lower

					final Set<String> existingCommentHeaderItems
							= PrefsUtility.getStringSet(
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
							CacheManager.getInstance(MainActivity.this)
									.emptyTheWholeCache();
						}
					}.start();
				}

				if(lastVersion <= 76) {
					// Upgrading from v1.9.6.1 or lower, enable image sharing from post context menu

					final Set<String> existingPostContextItems
							= PrefsUtility.getStringSet(
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

				if(lastVersion <= 77) {

					// Upgrading from 77/1.9.7 or lower, enable pinning/subscribing/blocking a
					// subreddit and editing self-posts in the post context menu

					final Set<String> existingPostContextItems
							= PrefsUtility.getStringSet(
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

				if(lastVersion <= 84) {

					// Upgrading from 84/1.9.8.5 or lower, change CheckBoxPreferences for
					// Main Menu Shortcuts into new MultiSelectListPreferences

					final Set<String> existingShortcutPreferences
							= PrefsUtility.getStringSet(
							R.string.pref_menus_mainmenu_shortcutitems_key,
							R.array.pref_menus_mainmenu_shortcutitems_items_default,
							this,
							sharedPreferences
					);

					if(PrefsUtility.pref_show_popular_main_menu(
							this,
							sharedPreferences
					)) {
						existingShortcutPreferences.add("popular");
					}


					if(PrefsUtility.pref_show_random_main_menu(
							this,
							sharedPreferences
					)) {
						existingShortcutPreferences.add("random");
					}

					sharedPreferences.edit().putStringSet(
							getString(R.string.pref_menus_mainmenu_shortcutitems_key),
							existingShortcutPreferences
					).apply();
				}

				if(lastVersion <= 87) {
					// + Context menu of post header will now appear also on
					// post self-text long click
					// + "Copy Self-Text" context menu item added

					final Set<String> existingPostContextItems
							= PrefsUtility.getStringSet(
							R.string.pref_menus_post_context_items_key,
							R.array.pref_menus_post_context_items_return,
							this,
							sharedPreferences
					);

					existingPostContextItems.add("copy_selftext");

					sharedPreferences.edit().putStringSet(
							getString(R.string.pref_menus_post_context_items_key),
							existingPostContextItems
					).apply();
				}

				if(lastVersion <= 89) {
					//Upgrading from 89/1.9.11 or lower, enable finer control over font scales
					//and set them to match the existing settings
					//The old Inbox Font Scale setting is ignored

					Log.i(TAG, "[Migration] Upgrading from v89");

					final String existingPostFontscalePreference = PrefsUtility.getString(
							R.string.pref_appearance_fontscale_posts_key,
							"-1",
							this,
							sharedPreferences
					);

					final String existingCommentSelfTextFontscalePreference = PrefsUtility
							.getString(
									R.string.pref_appearance_fontscale_bodytext_key,
									"-1",
									this,
									sharedPreferences
							);

					if(existingPostFontscalePreference.equals(
							existingCommentSelfTextFontscalePreference)) {

						Log.i(
								TAG,
								"[Migration] Old font preferences were both "
										+ existingPostFontscalePreference);

						// Avoid setting the global font scale to -1
						if(!existingPostFontscalePreference.equals("-1")) {

							Log.i(TAG, "[Migration] Migrating font preferences");

							sharedPreferences.edit().putString(
									getString(R.string.pref_appearance_fontscale_global_key),
									existingPostFontscalePreference
							).apply();

							sharedPreferences.edit().putString(
									getString(R.string.pref_appearance_fontscale_posts_key),
									"-1"
							).apply();

							sharedPreferences.edit().putString(
									getString(R.string.pref_appearance_fontscale_bodytext_key),
									"-1"
							).apply();
						}

					} else {

						Log.i(TAG, "[Migration] Old font prefs: comments="
								+ existingCommentSelfTextFontscalePreference
								+ ", posts="
								+ existingPostFontscalePreference
								+ ". Migrating.");

						sharedPreferences.edit().putString(
								getString(R.string.pref_appearance_fontscale_post_subtitles_key),
								existingPostFontscalePreference
						).apply();

						sharedPreferences.edit().putString(
								getString(
										R.string.pref_appearance_fontscale_post_header_titles_key),
								existingPostFontscalePreference
						).apply();

						sharedPreferences.edit().putString(
								getString(
										R.string.pref_appearance_fontscale_post_header_subtitles_key
								),
								existingPostFontscalePreference
						).apply();

						sharedPreferences.edit().putString(
								getString(R.string.pref_appearance_fontscale_comment_headers_key),
								existingCommentSelfTextFontscalePreference
						).apply();

						sharedPreferences.edit().putString(
								getString(R.string.pref_appearance_fontscale_linkbuttons_key),
								existingCommentSelfTextFontscalePreference
						).apply();
					}

					//Upgrading from 89/1.9.11 or lower, switch to ListPreference for
					//appearance_thumbnails_show, cache_precache_images, cache_precache_comments

					final String existingThumbnailsShowPreference
							= org.quantumbadger.redreader.common.StringUtils.asciiLowercase(
							PrefsUtility.appearance_thumbnails_show_old(
									this,
									sharedPreferences).toString());

					final String existingPrecacheImagesPreference
							= org.quantumbadger.redreader.common.StringUtils.asciiLowercase(
							PrefsUtility.cache_precache_images_old(
									this,
									sharedPreferences).toString());

					final String existingPrecacheCommentsPreference
							= org.quantumbadger.redreader.common.StringUtils.asciiLowercase(
							PrefsUtility.cache_precache_comments_old(
									this,
									sharedPreferences).toString());

					sharedPreferences.edit().putString(
							getString(R.string.pref_appearance_thumbnails_show_list_key),
							existingThumbnailsShowPreference
					).apply();

					sharedPreferences.edit().putString(
							getString(R.string.pref_cache_precache_images_list_key),
							existingPrecacheImagesPreference
					).apply();

					sharedPreferences.edit().putString(
							getString(R.string.pref_cache_precache_comments_list_key),
							existingPrecacheCommentsPreference
					).apply();
				}

				if(lastVersion <= 92) {
					// Upgrading from 92/1.12 or lower

					// Switch to individual ListPreference's for
					// pref_menus_appbar (formerly pref_menus_optionsmenu_items)

					final Set<String> existingOptionsMenuItems
							= PrefsUtility.getStringSet(
							R.string.pref_menus_optionsmenu_items_key,
							R.array.pref_menus_optionsmenu_items_items_return,
							this,
							sharedPreferences);

					class AppbarItemStrings {
						final int stringRes;
						final String returnValue;

						AppbarItemStrings(final int stringRes, final String returnValue) {
							this.stringRes = stringRes;
							this.returnValue = returnValue;
						}
					}

					final AppbarItemStrings[] appbarItemsPrefStrings
							= new AppbarItemStrings[] {
							new AppbarItemStrings(
									R.string.pref_menus_appbar_accounts_key,
									"accounts"),
							new AppbarItemStrings(
									R.string.pref_menus_appbar_theme_key,
									"theme"),
							new AppbarItemStrings(
									R.string.pref_menus_appbar_close_all_key,
									"close_all"),
							new AppbarItemStrings(
									R.string.pref_menus_appbar_past_key,
									"past"),
							new AppbarItemStrings(
									R.string.pref_menus_appbar_submit_post_key,
									"submit_post"),
							new AppbarItemStrings(
									R.string.pref_menus_appbar_search_key,
									"search"),
							new AppbarItemStrings(
									R.string.pref_menus_appbar_reply_key,
									"reply"),
							new AppbarItemStrings(
									R.string.pref_menus_appbar_pin_key,
									"pin"),
							new AppbarItemStrings(
									R.string.pref_menus_appbar_block_key,
									"block")
					};

					for(final AppbarItemStrings item : appbarItemsPrefStrings) {
						final String showAsAction;

						if(existingOptionsMenuItems.contains(item.returnValue)) {
							showAsAction = "0"; // Show only in three-dot menu
						} else {
							showAsAction = "-1"; // Never show
						}

						sharedPreferences.edit().putString(
								getString(item.stringRes),
								showAsAction
						).apply();
					}

				}
			}

		} else {
			Log.i(TAG, "[Migration] Last version not set.");
			sharedPreferences.edit().putInt("lastVersion", appVersion).apply();
			ChangelogDialog.newInstance().show(getSupportFragmentManager(), null);
		}

		recreateSubscriptionListener();

		final Boolean startInbox = getIntent().getBooleanExtra("isNewMessage", false);
		if(startInbox) {
			startActivity(new Intent(this, InboxListingActivity.class));
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
				editText.setOnEditorActionListener(new TextView.OnEditorActionListener() {
					@Override
					public boolean onEditorAction(
							final TextView v,
							final int actionId,
							final KeyEvent event) {
						boolean handled = false;
						if(actionId == EditorInfo.IME_ACTION_GO ||
							event.getKeyCode() == KeyEvent.KEYCODE_ENTER) {
							openCustomLocation(
									typeReturnValues,
									destinationType,
									editText);
							handled = true;
						}
						return handled;
					}
				});

				alertBuilder.setView(root);

				destinationType.setOnItemSelectedListener(new AdapterView.OnItemSelectedListener() {
					@Override
					public void onItemSelected(
							final AdapterView<?> adapterView,
							final View view,
							final int i,
							final long l) {
						final String typeName
								= typeReturnValues[destinationType.getSelectedItemPosition()];

						switch(typeName) {
							case "subreddit":
								editText.setAdapter(autocompleteAdapter);
								break;

							default:
								editText.setAdapter(null);
								break;
						}
					}

					@Override
					public void onNothingSelected(final AdapterView<?> adapterView) {
						editText.setAdapter(null);
					}
				});

				alertBuilder.setPositiveButton(
						R.string.dialog_go,
						new DialogInterface.OnClickListener() {
							@Override
							public void onClick(final DialogInterface dialog, final int which) {
								openCustomLocation(
										typeReturnValues,
										destinationType,
										editText);
							}
						});

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

			case MainMenuFragment.MENU_MENU_ACTION_MODMAIL: {
				final Intent intent = new Intent(this, InboxListingActivity.class);
				intent.putExtra("modmail", true);
				startActivity(intent);
				break;
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

			if(isMenuShown && (which == RefreshableFragment.ALL
					|| which == RefreshableFragment.MAIN)) {
				mainMenuFragment = new MainMenuFragment(this, null, force);
				mainMenuView = mainMenuFragment.getView();
				mLeftPane.removeAllViews();
				mLeftPane.addView(mainMenuView);
			}

			if(postListingController != null && (which == RefreshableFragment.ALL
					|| which == RefreshableFragment.POSTS)) {
				if(force && postListingFragment != null) {
					postListingFragment.cancel();
				}
				postListingFragment = postListingController.get(this, force, null);
				postListingView = postListingFragment.getView();
				postContainer.removeAllViews();
				postContainer.addView(postListingView);
			}

			if(commentListingController != null && (which == RefreshableFragment.ALL
					|| which
					== RefreshableFragment.COMMENTS)) {
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

	@Override
	public void onPostCommentsSelected(final RedditPreparedPost post) {

		if(twoPane) {

			commentListingController
					= new CommentListingController(
					PostCommentListingURL.forPostId(post.src
							.getIdAlone()),
					this);
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
						this,
						sharedPreferences,
						postListingFragment.getSubreddit().getCanonicalId());

				subredditBlockedState = PrefsUtility.pref_blocked_subreddits_check(
						this,
						sharedPreferences,
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
						&& subredditDescription.length() > 0,
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
	public void onSortSelected(final PostCommentListingURL.Sort order) {
		commentListingController.setSort(order);
		requestRefresh(RefreshableFragment.COMMENTS, false);
	}

	@Override
	public void onSortSelected(final UserCommentListingURL.Sort order) {
		commentListingController.setSort(order);
		requestRefresh(RefreshableFragment.COMMENTS, false);
	}

	@Override
	public void onSearchComments() {
		DialogUtils.showSearchDialog(
				this,
				R.string.action_search_comments,
				new DialogUtils.OnSearchListener() {
					@Override
					public void onSearch(@Nullable final String query) {
						final Intent searchIntent = new Intent(
								MainActivity.this,
								CommentListingActivity.class);
						searchIntent.setData(commentListingController.getUri());
						searchIntent.putExtra(
								CommentListingActivity.EXTRA_SEARCH_STRING,
								query);
						startActivity(searchIntent);
					}
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
					sharedPreferences,
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
					sharedPreferences,
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
					sharedPreferences,
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
					sharedPreferences,
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
		runOnUiThread(new Runnable() {
			@Override
			public void run() {
				invalidateOptionsMenu();
			}
		});
	}

	private void showBackButton(final boolean isVisible) {
		configBackButton(isVisible, new View.OnClickListener() {
			@Override
			public void onClick(final View v) {
				onBackPressed();
			}
		});
	}
}
