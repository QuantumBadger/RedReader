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

import android.app.Activity;
import android.app.AlertDialog;
import android.content.Context;
import android.content.Intent;
import android.graphics.Point;
import android.view.Menu;
import android.view.MenuItem;
import android.view.SubMenu;
import android.view.WindowManager;
import androidx.appcompat.app.AppCompatActivity;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.common.SharedPrefsWrapper;
import org.quantumbadger.redreader.common.StringUtils;
import org.quantumbadger.redreader.common.UnexpectedInternalStateException;
import org.quantumbadger.redreader.fragments.AccountListDialog;
import org.quantumbadger.redreader.reddit.PostSort;
import org.quantumbadger.redreader.reddit.api.SubredditSubscriptionState;
import org.quantumbadger.redreader.reddit.url.PostCommentListingURL;
import org.quantumbadger.redreader.reddit.url.UserCommentListingURL;
import org.quantumbadger.redreader.settings.SettingsActivity;

import java.util.ArrayList;
import java.util.Collections;
import java.util.EnumMap;
import java.util.Map;

public final class OptionsMenuUtility {

	public enum AppbarItemsPref {
		SORT,
		REFRESH,
		PAST,
		SUBMIT_POST,
		PIN,
		SUBSCRIBE,
		BLOCK,
		SIDEBAR,
		ACCOUNTS,
		THEME,
		SETTINGS,
		CLOSE_ALL,
		REPLY,
		SEARCH
	}

	public static final int DO_NOT_SHOW = -1;


	private enum Option {
		ACCOUNTS,
		SETTINGS,
		CLOSE_ALL,
		SUBMIT_POST,
		SEARCH,
		SEARCH_COMMENTS,
		REFRESH_SUBREDDITS,
		REFRESH_POSTS,
		REFRESH_COMMENTS,
		PAST_POSTS,
		THEMES,
		PAST_COMMENTS,
		SUBSCRIBE,
		SUBSCRIBING,
		UNSUBSCRIBING,
		UNSUBSCRIBE,
		SIDEBAR,
		PIN,
		UNPIN,
		BLOCK,
		UNBLOCK
	}

	public static <E extends BaseActivity & OptionsMenuListener> void prepare(
			final E activity,
			final Menu menu,
			final boolean subredditsVisible,
			final boolean postsVisible,
			final boolean commentsVisible,
			final boolean areSearchResults,
			final boolean isUserPostListing,
			final boolean isUserCommentListing,
			final boolean postsSortable,
			final boolean commentsSortable,
			final boolean isFrontPage,
			final SubredditSubscriptionState subredditSubscriptionState,
			final boolean subredditHasSidebar,
			final boolean pastCommentsSupported,
			final Boolean subredditPinned,
			final Boolean subredditBlocked) {

		final SharedPrefsWrapper preferences
				= General.getSharedPrefs(activity);
		final EnumMap<AppbarItemsPref, Integer> appbarItemsPrefs
				= PrefsUtility.pref_menus_appbar_items(activity, preferences);

		if(subredditsVisible && !postsVisible && !commentsVisible) {
			add(
					activity,
					menu,
					Option.REFRESH_SUBREDDITS,
					getOrThrow(appbarItemsPrefs, AppbarItemsPref.REFRESH),
					false);

		} else if(!subredditsVisible && postsVisible && !commentsVisible) {
			if(postsSortable) {

				if(areSearchResults) {
					addAllSearchSorts(
							activity,
							menu,
							getOrThrow(appbarItemsPrefs, AppbarItemsPref.SORT));
				} else {
					addAllPostSorts(
							activity,
							menu,
							getOrThrow(appbarItemsPrefs, AppbarItemsPref.SORT),
							!isUserPostListing,
							isFrontPage);
				}
			}
			add(
					activity,
					menu,
					Option.REFRESH_POSTS,
					getOrThrow(appbarItemsPrefs, AppbarItemsPref.REFRESH),
					false);
			add(
					activity,
					menu,
					Option.PAST_POSTS,
					getOrThrow(appbarItemsPrefs, AppbarItemsPref.PAST),
					false);
			add(
					activity,
					menu,
					Option.SUBMIT_POST,
					getOrThrow(appbarItemsPrefs, AppbarItemsPref.SUBMIT_POST),
					false);
			add(
					activity,
					menu,
					Option.SEARCH,
					getOrThrow(appbarItemsPrefs, AppbarItemsPref.SEARCH),
					false);

			if(subredditPinned != null) {
				if(subredditPinned) {
					add(
							activity,
							menu,
							Option.UNPIN,
							getOrThrow(appbarItemsPrefs, AppbarItemsPref.PIN),
							false);
				} else {
					add(
							activity,
							menu,
							Option.PIN,
							getOrThrow(appbarItemsPrefs, AppbarItemsPref.PIN),
							false);
				}
			}

			if(subredditSubscriptionState != null) {
				addSubscriptionItem(
						activity,
						menu,
						getOrThrow(appbarItemsPrefs, AppbarItemsPref.SUBSCRIBE),
						subredditSubscriptionState);
			}

			if(subredditBlocked != null) {
				if(subredditBlocked) {
					add(
							activity,
							menu,
							Option.UNBLOCK,
							getOrThrow(appbarItemsPrefs, AppbarItemsPref.BLOCK),
							false);
				} else {
					add(
							activity,
							menu,
							Option.BLOCK,
							getOrThrow(appbarItemsPrefs, AppbarItemsPref.BLOCK),
							false);
				}
			}

			if(subredditHasSidebar) {
				add(
						activity,
						menu,
						Option.SIDEBAR,
						getOrThrow(appbarItemsPrefs, AppbarItemsPref.SIDEBAR),
						false);
			}

		} else if(!subredditsVisible && !postsVisible && commentsVisible) {
			if(commentsSortable && !isUserCommentListing) {
				addAllCommentSorts(
						activity,
						menu,
						getOrThrow(appbarItemsPrefs, AppbarItemsPref.SORT));
			} else if(commentsSortable && isUserCommentListing) {
				addAllUserCommentSorts(
						activity,
						menu,
						getOrThrow(appbarItemsPrefs, AppbarItemsPref.SORT));
			}
			add(
					activity,
					menu,
					Option.REFRESH_COMMENTS,
					getOrThrow(appbarItemsPrefs, AppbarItemsPref.REFRESH),
					false);
			add(
					activity,
					menu,
					Option.SEARCH,
					getOrThrow(appbarItemsPrefs, AppbarItemsPref.SEARCH),
					false);
			if(pastCommentsSupported) {
				add(
						activity,
						menu,
						Option.PAST_COMMENTS,
						getOrThrow(appbarItemsPrefs, AppbarItemsPref.PAST),
						false);
			}

		} else {

			if(postsVisible && commentsVisible) {
				if(getOrThrow(appbarItemsPrefs, AppbarItemsPref.SORT) != DO_NOT_SHOW) {
					final SubMenu sortMenu = menu.addSubMenu(
							Menu.NONE,
							AppbarItemsPref.SORT.ordinal(),
							Menu.NONE,
							R.string.options_sort);
					sortMenu.getItem().setIcon(R.drawable.ic_sort_dark);
					sortMenu.getItem()
							.setShowAsAction(handleShowAsActionIfRoom(getOrThrow(
									appbarItemsPrefs,
									AppbarItemsPref.SORT)));

					if(postsSortable) {
						if(areSearchResults) {
							addAllSearchSorts(
									activity,
									sortMenu,
									MenuItem.SHOW_AS_ACTION_NEVER);
						} else {
							addAllPostSorts(
									activity,
									sortMenu,
									MenuItem.SHOW_AS_ACTION_NEVER,
									!isUserPostListing,
									isFrontPage);
						}
					}
					if(commentsSortable) {
						addAllCommentSorts(
								activity,
								sortMenu,
								MenuItem.SHOW_AS_ACTION_NEVER);
					}
				}
			} else if(postsVisible) {
				if(postsSortable) {
					if(areSearchResults) {
						addAllSearchSorts(
								activity,
								menu,
								getOrThrow(appbarItemsPrefs, AppbarItemsPref.SORT));
					} else {
						addAllPostSorts(
								activity,
								menu,
								getOrThrow(appbarItemsPrefs, AppbarItemsPref.SORT),
								!isUserPostListing,
								isFrontPage);
					}
				}
			}

			if(getOrThrow(appbarItemsPrefs, AppbarItemsPref.REFRESH) != DO_NOT_SHOW) {
				final SubMenu refreshMenu = menu.addSubMenu(
						Menu.NONE,
						AppbarItemsPref.REFRESH.ordinal(),
						Menu.NONE,
						R.string.options_refresh);
				refreshMenu.getItem().setIcon(R.drawable.ic_refresh_dark);
				refreshMenu.getItem()
						.setShowAsAction(handleShowAsActionIfRoom(getOrThrow(
								appbarItemsPrefs,
								AppbarItemsPref.REFRESH)));

				if(subredditsVisible) {
					add(activity, refreshMenu, Option.REFRESH_SUBREDDITS);
				}
				if(postsVisible) {
					add(activity, refreshMenu, Option.REFRESH_POSTS);
				}
				if(commentsVisible) {
					add(activity, refreshMenu, Option.REFRESH_COMMENTS);
				}
			}

			if(postsVisible && commentsVisible) {
				if(getOrThrow(appbarItemsPrefs, AppbarItemsPref.PAST) != DO_NOT_SHOW) {
					final SubMenu pastMenu = menu.addSubMenu(
							Menu.NONE,
							AppbarItemsPref.PAST.ordinal(),
							Menu.NONE,
							R.string.options_past);
					pastMenu.getItem().setIcon(R.drawable.ic_time_dark);
					pastMenu.getItem()
							.setShowAsAction(handleShowAsActionIfRoom(getOrThrow(
									appbarItemsPrefs,
									AppbarItemsPref.PAST)));

					add(activity, pastMenu, Option.PAST_POSTS);
					if(pastCommentsSupported) {
						add(activity, pastMenu, Option.PAST_COMMENTS);
					}
				}

				if(getOrThrow(appbarItemsPrefs, AppbarItemsPref.SEARCH) != DO_NOT_SHOW) {
					final SubMenu searchMenu = menu.addSubMenu(
							Menu.NONE,
							AppbarItemsPref.SEARCH.ordinal(),
							1,
							R.string.action_search);
					searchMenu.getItem().setIcon(R.drawable.ic_search_dark);
					searchMenu.getItem()
							.setShowAsAction(handleShowAsActionIfRoom(getOrThrow(
									appbarItemsPrefs,
									AppbarItemsPref.SEARCH)));

					add(activity, searchMenu, Option.SEARCH);
					add(activity, searchMenu, Option.SEARCH_COMMENTS);
				}
			} else if(postsVisible) {
				add(
						activity,
						menu,
						Option.SEARCH,
						getOrThrow(appbarItemsPrefs, AppbarItemsPref.SEARCH),
						false);
				add(
						activity,
						menu,
						Option.PAST_POSTS,
						getOrThrow(appbarItemsPrefs, AppbarItemsPref.PAST),
						false);
			}

			if(postsVisible) {
				add(
						activity,
						menu,
						Option.SUBMIT_POST,
						getOrThrow(appbarItemsPrefs, AppbarItemsPref.SUBMIT_POST),
						false);

				if(subredditPinned != null) {
					if(subredditPinned) {
						add(
								activity,
								menu,
								Option.UNPIN,
								getOrThrow(appbarItemsPrefs, AppbarItemsPref.PIN),
								false);
					} else {
						add(
								activity,
								menu,
								Option.PIN,
								getOrThrow(appbarItemsPrefs, AppbarItemsPref.PIN),
								false);
					}
				}

				if(subredditSubscriptionState != null) {
					addSubscriptionItem(
							activity,
							menu,
							getOrThrow(appbarItemsPrefs, AppbarItemsPref.SUBSCRIBE),
							subredditSubscriptionState);
				}

				if(subredditBlocked != null) {
					if(subredditBlocked) {
						add(
								activity,
								menu,
								Option.UNBLOCK,
								getOrThrow(appbarItemsPrefs, AppbarItemsPref.BLOCK),
								false);
					} else {
						add(
								activity,
								menu,
								Option.BLOCK,
								getOrThrow(appbarItemsPrefs, AppbarItemsPref.BLOCK),
								false);
					}
				}

				if(subredditHasSidebar) {
					add(
							activity,
							menu,
							Option.SIDEBAR,
							getOrThrow(appbarItemsPrefs, AppbarItemsPref.SIDEBAR),
							false);
				}
			}
		}

		addAccounts(
				activity,
				menu,
				getOrThrow(appbarItemsPrefs, AppbarItemsPref.ACCOUNTS));
		add(
				activity,
				menu,
				Option.THEMES,
				getOrThrow(appbarItemsPrefs, AppbarItemsPref.THEME),
				false);

		// Always show settings if the main menu is visible, to prevent user from being
		// locked out of them
		if(subredditsVisible
				&& getOrThrow(appbarItemsPrefs, AppbarItemsPref.SETTINGS)
				== DO_NOT_SHOW) {
			add(activity, menu, Option.SETTINGS, MenuItem.SHOW_AS_ACTION_NEVER, false);
		} else {
			add(
					activity,
					menu,
					Option.SETTINGS,
					getOrThrow(appbarItemsPrefs, AppbarItemsPref.SETTINGS),
					false);
		}

		add(
				activity,
				menu,
				Option.CLOSE_ALL,
				getOrThrow(appbarItemsPrefs, AppbarItemsPref.CLOSE_ALL),
				false);

		pruneMenu(activity, menu, appbarItemsPrefs, !subredditsVisible);
	}

	public static void pruneMenu(
			final Activity activity,
			final Menu menu,
			final Map<AppbarItemsPref, Integer> appbarItemsPrefs,
			final boolean backButtonShown) {

		//Figure out how many buttons can fit
		final Point windowSize = new Point();
		((WindowManager)activity.getSystemService(Context.WINDOW_SERVICE)).getDefaultDisplay()
				.getSize(windowSize);

		final int buttonSize = General.dpToPixels(activity, 48);
		final int backButtonSize = General.dpToPixels(activity, 52);

		int buttonSlotsRemaining = (windowSize.x - (backButtonShown
				? backButtonSize
				: 0)) / buttonSize;

		//Count show-if-room buttons, subtract always-show buttons from
		// total-remaining, see if we MUST show the overflow menu
		int optionalButtonsRequested = 0;
		boolean overflowButtonRequired = false;

		for(int i = 0; i < menu.size(); i++) {
			for(final Map.Entry<AppbarItemsPref, Integer> pair : appbarItemsPrefs.entrySet()) {
				if(pair.getKey().ordinal() == menu.getItem(i).getItemId()) {
					if(pair.getValue() == MenuItem.SHOW_AS_ACTION_ALWAYS) {
						buttonSlotsRemaining--;
					} else if(pair.getValue() == MenuItem.SHOW_AS_ACTION_NEVER) {
						overflowButtonRequired = true;
					} else {
						optionalButtonsRequested++;
					}
				}
			}
		}

		//Reserve space for the overflow button if needed
		if(overflowButtonRequired || optionalButtonsRequested > buttonSlotsRemaining) {
			buttonSlotsRemaining--;
		}

		//Move optional buttons to the overflow menu if there's not enough space, end to start
		if(optionalButtonsRequested > buttonSlotsRemaining) {
			for(int i = menu.size() - 1; i >= 0; i--) {
				for(final Map.Entry<AppbarItemsPref, Integer> pair : appbarItemsPrefs.entrySet()) {
					if(pair.getKey().ordinal() == menu.getItem(i).getItemId()
							&& pair.getValue() == MenuItem.SHOW_AS_ACTION_IF_ROOM) {

						menu.getItem(i).setShowAsAction(MenuItem.SHOW_AS_ACTION_NEVER);
						buttonSlotsRemaining++;
						break;
					}
				}
				if(optionalButtonsRequested <= buttonSlotsRemaining) {
					break;
				}
			}
		}
	}

	private static void addSubscriptionItem(
			final BaseActivity activity, final Menu menu, final int showAsAction,
			final SubredditSubscriptionState subredditSubscriptionState) {

		if(subredditSubscriptionState == null) {
			return;
		}

		switch(subredditSubscriptionState) {
			case NOT_SUBSCRIBED:
				add(activity, menu, Option.SUBSCRIBE, showAsAction, false);
				return;
			case SUBSCRIBED:
				add(activity, menu, Option.UNSUBSCRIBE, showAsAction, false);
				return;
			case SUBSCRIBING:
				add(activity, menu, Option.SUBSCRIBING, showAsAction, false);
				return;
			case UNSUBSCRIBING:
				add(activity, menu, Option.UNSUBSCRIBING, showAsAction, false);
				return;
			default:
				throw new UnexpectedInternalStateException("Unknown subscription state");
		}

	}

	private static void add(
			final BaseActivity activity,
			final Menu menu,
			final Option option) {
		add(activity, menu, option, MenuItem.SHOW_AS_ACTION_NEVER, true);
	}

	private static void add(
			final BaseActivity activity,
			final Menu menu,
			final Option option,
			int showAsAction,
			final boolean longText) {

		if(showAsAction == DO_NOT_SHOW) {
			return;
		} else {
			showAsAction = handleShowAsActionIfRoom(showAsAction);
		}

		switch(option) {

			case ACCOUNTS: {
				final MenuItem accounts = menu.add(
						Menu.NONE,
						AppbarItemsPref.ACCOUNTS.ordinal(),
						longText
							? QuickAccountsSort.MANAGER
							: Menu.NONE,
						activity.getString(longText
								? R.string.options_account_manager
								: R.string.options_accounts))
						.setOnMenuItemClickListener(item -> {
							new AccountListDialog().show(
									activity.getSupportFragmentManager(),
									null);
							return true;
						});

				accounts.setShowAsAction(showAsAction);
				if(longText) {
					if(PrefsUtility.isNightMode(activity)) {
						accounts.setIcon(R.drawable.ic_settings_dark);
					} else {
						accounts.setIcon(R.drawable.ic_settings_light);
					}
				} else {
					accounts.setIcon(R.drawable.ic_accounts_dark);
				}

				break;
			}
			case SETTINGS: {
				final MenuItem settings = menu.add(
						Menu.NONE,
						AppbarItemsPref.SETTINGS.ordinal(),
						Menu.NONE,
						R.string.options_settings)
						.setOnMenuItemClickListener(item -> {
							final Intent intent = new Intent(
									activity,
									SettingsActivity.class);
							activity.startActivityForResult(
									intent,
									1);
							return true;
						});

				settings.setShowAsAction(showAsAction);
				settings.setIcon(R.drawable.ic_settings_dark);

				break;
			}
			case CLOSE_ALL: {
				if(!(activity instanceof MainActivity)) {
					final MenuItem closeAll = menu.add(
							Menu.NONE,
							AppbarItemsPref.CLOSE_ALL.ordinal(),
							Menu.NONE,
							R.string.options_close_all)
							.setOnMenuItemClickListener(item -> {
								activity.closeAllExceptMain();
								return true;
							});

					closeAll.setShowAsAction(showAsAction);
					closeAll.setIcon(R.drawable.ic_action_cross_dark);
				}
				break;
			}
			case THEMES: {
				final MenuItem themes = menu.add(
						Menu.NONE,
						AppbarItemsPref.THEME.ordinal(),
						Menu.NONE,
						R.string.options_theme)
						.setOnMenuItemClickListener(item -> {

							final SharedPrefsWrapper prefs
									= General.getSharedPrefs(activity);
							final PrefsUtility.AppearanceTheme currentTheme
									= PrefsUtility.appearance_theme(activity, prefs);

							final String[] themeNames = activity.getResources()
									.getStringArray(R.array.pref_appearance_theme);

							final String[] themeValues = activity.getResources()
									.getStringArray(R.array.pref_appearance_theme_return);

							int selectedPos = -1;
							for(int i = 0; i < themeValues.length; i++) {
								if(PrefsUtility.AppearanceTheme.valueOf(
										StringUtils.asciiUppercase(themeValues[i]))
										.equals(currentTheme)) {
									selectedPos = i;
									break;
								}
							}

							final AlertDialog.Builder dialog
									= new AlertDialog.Builder(activity);
							dialog.setTitle(R.string.pref_appearance_theme_title);

							dialog.setSingleChoiceItems(
									themeNames,
									selectedPos,
									(dialog1, item1) -> {
										prefs.edit()
												.putString(
														activity.getString(
														R.string.pref_appearance_theme_key),
														themeValues[item1])
												.apply();
										dialog1.dismiss();
									});

							final AlertDialog alert = dialog.create();
							alert.show();
							return true;
						});

				themes.setShowAsAction(showAsAction);
				themes.setIcon(R.drawable.ic_themes_dark);

				break;
			}
			case REFRESH_SUBREDDITS: {
				final MenuItem refreshSubreddits = menu.add(
						Menu.NONE,
						AppbarItemsPref.REFRESH.ordinal(),
						Menu.NONE,
						R.string.options_refresh_subreddits)
						.setOnMenuItemClickListener(item -> {
							((OptionsMenuSubredditsListener)activity)
									.onRefreshSubreddits();
							return true;
						});

				refreshSubreddits.setShowAsAction(showAsAction);
				if(!longText) {
					refreshSubreddits.setIcon(R.drawable.ic_refresh_dark);
				}

				break;
			}
			case REFRESH_POSTS: {
				final MenuItem refreshPosts = menu.add(
						Menu.NONE,
						AppbarItemsPref.REFRESH.ordinal(),
						Menu.NONE,
						R.string.options_refresh_posts)
						.setOnMenuItemClickListener(item -> {
							((OptionsMenuPostsListener)activity).onRefreshPosts();
							return true;
						});

				refreshPosts.setShowAsAction(showAsAction);
				if(!longText) {
					refreshPosts.setIcon(R.drawable.ic_refresh_dark);
				}

				break;
			}
			case SUBMIT_POST: {
				final MenuItem submitPost = menu.add(
						Menu.NONE,
						AppbarItemsPref.SUBMIT_POST.ordinal(),
						Menu.NONE,
						R.string.options_submit_post)
						.setOnMenuItemClickListener(item -> {
							((OptionsMenuPostsListener)activity).onSubmitPost();
							return true;
						});

				submitPost.setShowAsAction(showAsAction);
				submitPost.setIcon(R.drawable.ic_action_send_dark);

				break;
			}
			case SEARCH: {
				final MenuItem search = menu.add(
						Menu.NONE,
						AppbarItemsPref.SEARCH.ordinal(),
						1,
						activity.getString(longText
								? R.string.action_search_posts
								: R.string.action_search))
						.setOnMenuItemClickListener(item -> {
							if(activity instanceof OptionsMenuPostsListener) {
								((OptionsMenuPostsListener)activity).onSearchPosts();
								return true;
							} else if(activity instanceof OptionsMenuCommentsListener) {
								((OptionsMenuCommentsListener)activity).onSearchComments();
								return true;
							} else {
								return false;
							}
						});

				search.setShowAsAction(showAsAction);
				if(!longText) {
					search.setIcon(R.drawable.ic_search_dark);
				}

				break;
			}
			case SEARCH_COMMENTS: {
				final MenuItem searchComments = menu.add(
						Menu.NONE,
						AppbarItemsPref.SEARCH.ordinal(),
						1,
						activity.getString(R.string.action_search_comments))
						.setOnMenuItemClickListener(item -> {
							if(activity instanceof OptionsMenuCommentsListener) {
								((OptionsMenuCommentsListener)activity)
										.onSearchComments();
								return true;
							}
							return false;
						});

				searchComments.setShowAsAction(showAsAction);
				if(!longText) {
					searchComments.setIcon(R.drawable.ic_search_dark);
				}

				break;
			}
			case REFRESH_COMMENTS: {
				final MenuItem refreshComments = menu.add(
						Menu.NONE,
						AppbarItemsPref.REFRESH.ordinal(),
						Menu.NONE,
						R.string.options_refresh_comments)
						.setOnMenuItemClickListener(item -> {
							((OptionsMenuCommentsListener)activity)
									.onRefreshComments();
							return true;
						});

				refreshComments.setShowAsAction(showAsAction);
				if(!longText) {
					refreshComments.setIcon(R.drawable.ic_refresh_dark);
				}

				break;
			}
			case PAST_POSTS: {
				final MenuItem pastPosts = menu.add(
						Menu.NONE,
						AppbarItemsPref.PAST.ordinal(),
						Menu.NONE,
						longText ? R.string.options_past_posts : R.string.options_past)
						.setOnMenuItemClickListener(item -> {
							((OptionsMenuPostsListener)activity).onPastPosts();
							return true;
						});

				if(showAsAction != MenuItem.SHOW_AS_ACTION_NEVER) {
					pastPosts.setShowAsAction(showAsAction);
					pastPosts.setIcon(R.drawable.ic_time_dark);
				}

				break;
			}
			case PAST_COMMENTS: {
				final MenuItem pastComments = menu.add(
						Menu.NONE,
						AppbarItemsPref.PAST.ordinal(),
						Menu.NONE,
						longText ? R.string.options_past_comments : R.string.options_past)
						.setOnMenuItemClickListener(item -> {
							((OptionsMenuCommentsListener)activity)
									.onPastComments();
							return true;
						});

				if(showAsAction != MenuItem.SHOW_AS_ACTION_NEVER) {
					pastComments.setShowAsAction(showAsAction);
					pastComments.setIcon(R.drawable.ic_time_dark);
				}

				break;
			}
			case SUBSCRIBE: {
				final MenuItem subscribe = menu.add(
						Menu.NONE,
						AppbarItemsPref.SUBSCRIBE.ordinal(),
						Menu.NONE,
						R.string.options_subscribe)
						.setOnMenuItemClickListener(item -> {
							((OptionsMenuPostsListener)activity).onSubscribe();
							return true;
						});

				subscribe.setShowAsAction(showAsAction);
				subscribe.setIcon(R.drawable.star_off_dark);

				break;
			}
			case UNSUBSCRIBE: {
				final MenuItem unsubscribe = menu.add(
						Menu.NONE,
						AppbarItemsPref.SUBSCRIBE.ordinal(),
						Menu.NONE,
						R.string.options_unsubscribe)
						.setOnMenuItemClickListener(item -> {
							((OptionsMenuPostsListener)activity).onUnsubscribe();
							return true;
						});

				unsubscribe.setShowAsAction(showAsAction);
				unsubscribe.setIcon(R.drawable.star_dark);

				break;
			}
			case UNSUBSCRIBING: {
				final MenuItem unsubscribing = menu.add(
						Menu.NONE,
						AppbarItemsPref.SUBSCRIBE.ordinal(),
						Menu.NONE,
						R.string.options_unsubscribing).setEnabled(false);

				// TODO Somehow use a ButtonLoadingSpinnerView here or something?
				unsubscribing.setShowAsAction(showAsAction);
				unsubscribing.setIcon(R.drawable.ic_loading_dark);

				break;
			}
			case SUBSCRIBING: {
				final MenuItem subscribing = menu.add(
						Menu.NONE,
						AppbarItemsPref.SUBSCRIBE.ordinal(),
						Menu.NONE,
						R.string.options_subscribing).setEnabled(false);

				// TODO Somehow use a ButtonLoadingSpinnerView here or something?
				subscribing.setShowAsAction(showAsAction);
				subscribing.setIcon(R.drawable.ic_loading_dark);

				break;
			}
			case SIDEBAR: {
				final MenuItem sidebar = menu.add(
						Menu.NONE,
						AppbarItemsPref.SIDEBAR.ordinal(),
						Menu.NONE,
						R.string.options_sidebar)
						.setOnMenuItemClickListener(item -> {
							((OptionsMenuPostsListener)activity).onSidebar();
							return true;
						});

				sidebar.setShowAsAction(showAsAction);
				sidebar.setIcon(R.drawable.ic_action_info_dark);

				break;
			}
			case PIN: {
				final MenuItem pin = menu.add(
						Menu.NONE,
						AppbarItemsPref.PIN.ordinal(),
						Menu.NONE,
						R.string.pin_subreddit)
						.setOnMenuItemClickListener(item -> {
							((OptionsMenuPostsListener)activity).onPin();
							return true;
						});

				pin.setShowAsAction(showAsAction);
				pin.setIcon(R.drawable.pin_off_dark);

				break;
			}
			case UNPIN: {
				final MenuItem unpin = menu.add(
						Menu.NONE,
						AppbarItemsPref.PIN.ordinal(),
						Menu.NONE,
						R.string.unpin_subreddit)
						.setOnMenuItemClickListener(item -> {
							((OptionsMenuPostsListener)activity).onUnpin();
							return true;
						});

				unpin.setShowAsAction(showAsAction);
				unpin.setIcon(R.drawable.pin_dark);

				break;
			}
			case BLOCK: {
				final MenuItem block = menu.add(
						Menu.NONE,
						AppbarItemsPref.BLOCK.ordinal(),
						Menu.NONE,
						R.string.block_subreddit)
						.setOnMenuItemClickListener(item -> {
							((OptionsMenuPostsListener)activity).onBlock();
							return true;
						});

				block.setShowAsAction(showAsAction);
				block.setIcon(R.drawable.ic_block_off_dark);

				break;
			}
			case UNBLOCK: {
				final MenuItem unblock = menu.add(
						Menu.NONE,
						AppbarItemsPref.BLOCK.ordinal(),
						Menu.NONE,
						R.string.unblock_subreddit)
						.setOnMenuItemClickListener(item -> {
							((OptionsMenuPostsListener)activity).onUnblock();
							return true;
						});

				unblock.setShowAsAction(showAsAction);
				unblock.setIcon(R.drawable.ic_block_dark);

				break;
			}
			default:
				BugReportActivity.handleGlobalError(
						activity,
						"Unknown menu option added");
		}
	}

	private static void addAllPostSorts(
			final AppCompatActivity activity,
			final Menu menu,
			final int showAsAction,
			final boolean includeRising,
			final boolean includeBest) {

		if(showAsAction == DO_NOT_SHOW) {
			return;
		}

		final SubMenu sortPosts = menu.addSubMenu(
				Menu.NONE,
				AppbarItemsPref.SORT.ordinal(),
				Menu.NONE,
				R.string.options_sort_posts);

		if(showAsAction != MenuItem.SHOW_AS_ACTION_NEVER) {
			sortPosts.getItem().setIcon(R.drawable.ic_sort_dark);
			sortPosts.getItem().setShowAsAction(handleShowAsActionIfRoom(showAsAction));
		}

		addSort(activity, sortPosts, R.string.sort_posts_hot, PostSort.HOT);
		addSort(activity, sortPosts, R.string.sort_posts_new, PostSort.NEW);
		if(includeRising) {
			addSort(activity, sortPosts, R.string.sort_posts_rising, PostSort.RISING);
		}

		final SubMenu sortsPostsControversial =
				sortPosts.addSubMenu(R.string.sort_posts_controversial);

		addSort(
				activity,
				sortsPostsControversial,
				R.string.sort_posts_controversial_hour,
				PostSort.CONTROVERSIAL_HOUR);
		addSort(
				activity,
				sortsPostsControversial,
				R.string.sort_posts_controversial_today,
				PostSort.CONTROVERSIAL_DAY);
		addSort(
				activity,
				sortsPostsControversial,
				R.string.sort_posts_controversial_week,
				PostSort.CONTROVERSIAL_WEEK);
		addSort(
				activity,
				sortsPostsControversial,
				R.string.sort_posts_controversial_month,
				PostSort.CONTROVERSIAL_MONTH);
		addSort(
				activity,
				sortsPostsControversial,
				R.string.sort_posts_controversial_year,
				PostSort.CONTROVERSIAL_YEAR);
		addSort(
				activity, sortsPostsControversial,
				R.string.sort_posts_controversial_all,
				PostSort.CONTROVERSIAL_ALL);

		if(includeBest) {
			addSort(activity, sortPosts, R.string.sort_posts_best, PostSort.BEST);
		}

		final SubMenu sortPostsTop = sortPosts.addSubMenu(R.string.sort_posts_top);

		addSort(activity, sortPostsTop, R.string.sort_posts_top_hour, PostSort.TOP_HOUR);
		addSort(activity, sortPostsTop, R.string.sort_posts_top_today, PostSort.TOP_DAY);
		addSort(activity, sortPostsTop, R.string.sort_posts_top_week, PostSort.TOP_WEEK);
		addSort(
				activity,
				sortPostsTop,
				R.string.sort_posts_top_month,
				PostSort.TOP_MONTH);
		addSort(activity, sortPostsTop, R.string.sort_posts_top_year, PostSort.TOP_YEAR);
		addSort(activity, sortPostsTop, R.string.sort_posts_top_all, PostSort.TOP_ALL);
	}

	private static void addAllSearchSorts(
			final AppCompatActivity activity,
			final Menu menu,
			final int showAsAction) {

		if(showAsAction == DO_NOT_SHOW) {
			return;
		}

		final SubMenu sortPosts = menu.addSubMenu(
				Menu.NONE,
				AppbarItemsPref.SORT.ordinal(),
				Menu.NONE,
				R.string.options_sort_posts);

		if(showAsAction != MenuItem.SHOW_AS_ACTION_NEVER) {
			sortPosts.getItem().setIcon(R.drawable.ic_sort_dark);
			sortPosts.getItem().setShowAsAction(handleShowAsActionIfRoom(showAsAction));
		}

		final SubMenu sortPostsRelevance = sortPosts.addSubMenu(R.string.sort_posts_relevance);

		addSort(
				activity,
				sortPostsRelevance,
				R.string.sort_posts_relevance_hour,
				PostSort.RELEVANCE_HOUR);
		addSort(
				activity,
				sortPostsRelevance,
				R.string.sort_posts_relevance_today,
				PostSort.RELEVANCE_DAY);
		addSort(
				activity,
				sortPostsRelevance,
				R.string.sort_posts_relevance_week,
				PostSort.RELEVANCE_WEEK);
		addSort(
				activity,
				sortPostsRelevance,
				R.string.sort_posts_relevance_month,
				PostSort.RELEVANCE_MONTH);
		addSort(
				activity,
				sortPostsRelevance,
				R.string.sort_posts_relevance_year,
				PostSort.RELEVANCE_YEAR);
		addSort(
				activity,
				sortPostsRelevance,
				R.string.sort_posts_relevance_all,
				PostSort.RELEVANCE_ALL);

		final SubMenu sortPostsNew = sortPosts.addSubMenu(R.string.sort_posts_new);

		addSort(activity, sortPostsNew, R.string.sort_posts_new_hour, PostSort.NEW_HOUR);
		addSort(activity, sortPostsNew, R.string.sort_posts_new_today, PostSort.NEW_DAY);
		addSort(activity, sortPostsNew, R.string.sort_posts_new_week, PostSort.NEW_WEEK);
		addSort(activity, sortPostsNew, R.string.sort_posts_new_month, PostSort.NEW_MONTH);
		addSort(activity, sortPostsNew, R.string.sort_posts_new_year, PostSort.NEW_YEAR);
		addSort(activity, sortPostsNew, R.string.sort_posts_new_all, PostSort.NEW_ALL);

		final SubMenu sortPostsHot = sortPosts.addSubMenu(R.string.sort_posts_hot);

		addSort(activity, sortPostsHot, R.string.sort_posts_hot_hour, PostSort.HOT_HOUR);
		addSort(activity, sortPostsHot, R.string.sort_posts_hot_today, PostSort.HOT_DAY);
		addSort(activity, sortPostsHot, R.string.sort_posts_hot_week, PostSort.HOT_WEEK);
		addSort(activity, sortPostsHot, R.string.sort_posts_hot_month, PostSort.HOT_MONTH);
		addSort(activity, sortPostsHot, R.string.sort_posts_hot_year, PostSort.HOT_YEAR);
		addSort(activity, sortPostsHot, R.string.sort_posts_hot_all, PostSort.HOT_ALL);

		final SubMenu sortPostsTop = sortPosts.addSubMenu(R.string.sort_posts_top);

		addSort(activity, sortPostsTop, R.string.sort_posts_top_hour, PostSort.TOP_HOUR);
		addSort(activity, sortPostsTop, R.string.sort_posts_top_today, PostSort.TOP_DAY);
		addSort(activity, sortPostsTop, R.string.sort_posts_top_week, PostSort.TOP_WEEK);
		addSort(activity, sortPostsTop, R.string.sort_posts_top_month, PostSort.TOP_MONTH);
		addSort(activity, sortPostsTop, R.string.sort_posts_top_year, PostSort.TOP_YEAR);
		addSort(activity, sortPostsTop, R.string.sort_posts_top_all, PostSort.TOP_ALL);

		final SubMenu sortPostsComments = sortPosts.addSubMenu(R.string.sort_posts_comments);

		addSort(
				activity,
				sortPostsComments,
				R.string.sort_posts_comments_hour,
				PostSort.COMMENTS_HOUR);
		addSort(
				activity,
				sortPostsComments,
				R.string.sort_posts_comments_today,
				PostSort.COMMENTS_DAY);
		addSort(
				activity,
				sortPostsComments,
				R.string.sort_posts_comments_week,
				PostSort.COMMENTS_WEEK);
		addSort(
				activity,
				sortPostsComments,
				R.string.sort_posts_comments_month,
				PostSort.COMMENTS_MONTH);
		addSort(
				activity,
				sortPostsComments,
				R.string.sort_posts_comments_year,
				PostSort.COMMENTS_YEAR);
		addSort(
				activity,
				sortPostsComments,
				R.string.sort_posts_comments_all,
				PostSort.COMMENTS_ALL);
	}

	private static void addSort(
			final AppCompatActivity activity,
			final Menu menu,
			final int name,
			final PostSort order) {

		menu.add(activity.getString(name))
				.setOnMenuItemClickListener(item -> {
					((OptionsMenuPostsListener)activity).onSortSelected(order);
					return true;
				});
	}

	private static void addAllCommentSorts(
			final AppCompatActivity activity,
			final Menu menu,
			final int showAsAction) {

		if(showAsAction == DO_NOT_SHOW) {
			return;
		}

		final SubMenu sortComments = menu.addSubMenu(
				Menu.NONE,
				AppbarItemsPref.SORT.ordinal(),
				Menu.NONE,
				R.string.options_sort_comments);

		if(showAsAction != MenuItem.SHOW_AS_ACTION_NEVER) {
			sortComments.getItem().setIcon(R.drawable.ic_sort_dark);
			sortComments.getItem()
					.setShowAsAction(handleShowAsActionIfRoom(showAsAction));
		}

		addSort(
				activity,
				sortComments,
				R.string.sort_comments_best,
				PostCommentListingURL.Sort.BEST);
		addSort(
				activity,
				sortComments,
				R.string.sort_comments_hot,
				PostCommentListingURL.Sort.HOT);
		addSort(
				activity,
				sortComments,
				R.string.sort_comments_new,
				PostCommentListingURL.Sort.NEW);
		addSort(
				activity,
				sortComments,
				R.string.sort_comments_old,
				PostCommentListingURL.Sort.OLD);
		addSort(
				activity,
				sortComments,
				R.string.sort_comments_controversial,
				PostCommentListingURL.Sort.CONTROVERSIAL);
		addSort(
				activity,
				sortComments,
				R.string.sort_comments_top,
				PostCommentListingURL.Sort.TOP);
		addSort(
				activity,
				sortComments,
				R.string.sort_comments_qa,
				PostCommentListingURL.Sort.QA);
	}

	private static void addSort(
			final AppCompatActivity activity,
			final Menu menu,
			final int name,
			final PostCommentListingURL.Sort order) {

		menu.add(activity.getString(name))
				.setOnMenuItemClickListener(item -> {
					((OptionsMenuCommentsListener)activity).onSortSelected(order);
					return true;
				});
	}

	private static void addAllUserCommentSorts(
			final AppCompatActivity activity,
			final Menu menu,
			final int showAsAction) {

		if(showAsAction == DO_NOT_SHOW) {
			return;
		}

		final SubMenu sortComments = menu.addSubMenu(
				Menu.NONE,
				AppbarItemsPref.SORT.ordinal(),
				Menu.NONE,
				R.string.options_sort_comments);

		if(showAsAction != MenuItem.SHOW_AS_ACTION_NEVER) {
			sortComments.getItem().setIcon(R.drawable.ic_sort_dark);
			sortComments.getItem()
					.setShowAsAction(handleShowAsActionIfRoom(showAsAction));
		}

		addSort(
				activity,
				sortComments,
				R.string.sort_comments_hot,
				UserCommentListingURL.Sort.HOT);
		addSort(
				activity,
				sortComments,
				R.string.sort_comments_new,
				UserCommentListingURL.Sort.NEW);

		final SubMenu sortCommentsControversial
				= sortComments.addSubMenu(R.string.sort_comments_controversial);

		addSort(
				activity,
				sortCommentsControversial,
				R.string.sort_posts_controversial_hour,
				UserCommentListingURL.Sort.CONTROVERSIAL_HOUR);
		addSort(
				activity,
				sortCommentsControversial,
				R.string.sort_posts_controversial_today,
				UserCommentListingURL.Sort.CONTROVERSIAL_DAY);
		addSort(
				activity,
				sortCommentsControversial,
				R.string.sort_posts_controversial_week,
				UserCommentListingURL.Sort.CONTROVERSIAL_WEEK);
		addSort(
				activity,
				sortCommentsControversial,
				R.string.sort_posts_controversial_month,
				UserCommentListingURL.Sort.CONTROVERSIAL_MONTH);
		addSort(
				activity,
				sortCommentsControversial,
				R.string.sort_posts_controversial_year,
				UserCommentListingURL.Sort.CONTROVERSIAL_YEAR);
		addSort(
				activity,
				sortCommentsControversial,
				R.string.sort_posts_controversial_all,
				UserCommentListingURL.Sort.CONTROVERSIAL_ALL);

		final SubMenu sortCommentsTop
				= sortComments.addSubMenu(R.string.sort_comments_top);

		addSort(
				activity,
				sortCommentsTop,
				R.string.sort_posts_top_hour,
				UserCommentListingURL.Sort.TOP_HOUR);
		addSort(
				activity,
				sortCommentsTop,
				R.string.sort_posts_top_today,
				UserCommentListingURL.Sort.TOP_DAY);
		addSort(
				activity,
				sortCommentsTop,
				R.string.sort_posts_top_week,
				UserCommentListingURL.Sort.TOP_WEEK);
		addSort(
				activity,
				sortCommentsTop,
				R.string.sort_posts_top_month,
				UserCommentListingURL.Sort.TOP_MONTH);
		addSort(
				activity,
				sortCommentsTop,
				R.string.sort_posts_top_year,
				UserCommentListingURL.Sort.TOP_YEAR);
		addSort(
				activity,
				sortCommentsTop,
				R.string.sort_posts_top_all,
				UserCommentListingURL.Sort.TOP_ALL);
	}

	private static void addSort(
			final AppCompatActivity activity,
			final Menu menu,
			final int name,
			final UserCommentListingURL.Sort order) {

		menu.add(activity.getString(name))
				.setOnMenuItemClickListener(menuItem -> {
					((OptionsMenuCommentsListener)activity).onSortSelected(order);
					return true;
				});
	}

	private static class QuickAccountsSort {
		//Constants for sorting the quick accounts submenu properly
		//Real accounts first, then Anonymous, then the account dialog
		static final int ACCOUNT = 2;
		static final int ANONYMOUS = 3;
		static final int MANAGER = 4;
	}

	private static void addAccounts(
			final BaseActivity activity,
			final Menu menu,
			final int showAsAction) {

		if(showAsAction == DO_NOT_SHOW) {
			return;
		}

		final RedditAccountManager accountManager = RedditAccountManager.getInstance(activity);
		final ArrayList<RedditAccount> accountsList = accountManager.getAccounts();

		if(PrefsUtility.pref_menus_quick_account_switcher(
				activity,
				General.getSharedPrefs(activity))
						&& accountsList.size() > 1) {

			//Quick account switcher is on, create its SubMenu and add it to the main menu
			final int accountsGroup = 1;

			final SubMenu accountsMenu = menu.addSubMenu(
					Menu.NONE,
					AppbarItemsPref.ACCOUNTS.ordinal(),
					Menu.NONE,
					R.string.options_accounts);
			accountsMenu.getItem().setShowAsAction(handleShowAsActionIfRoom(showAsAction));
			accountsMenu.getItem().setIcon(R.drawable.ic_accounts_dark);

			// Sort the accounts so they don't move around
			Collections.sort(accountsList, (o1, o2) -> o1.username.compareTo(o2.username));

			//Add a MenuItem for each account, always putting Anonymous after the real accounts
			//Each account gets a radio button to show which one is active
			for(final RedditAccount account : accountsList) {
				final MenuItem accountsMenuItem = accountsMenu.add(
						accountsGroup,
						Menu.NONE,
						account.isAnonymous()
								? QuickAccountsSort.ANONYMOUS
								: QuickAccountsSort.ACCOUNT,
						account.isAnonymous()
								? activity.getString(R.string.accounts_anon)
								: account.username)
						.setOnMenuItemClickListener(item -> {
							accountManager.setDefaultAccount(account);
							return true;
						});

				if(account.equals(accountManager.getDefaultAccount())) {
					accountsMenuItem.setChecked(true);
				}
			}

			accountsMenu.setGroupCheckable(accountsGroup,true, true);

			//Add a MenuItem for the full account dialog, so it's still accessible for changes
			add(activity, accountsMenu, Option.ACCOUNTS);

		} else {

			//Quick account switcher is off, just make the button go straight to the dialog
			add(activity, menu, Option.ACCOUNTS, showAsAction, false);
		}
	}

	public static int handleShowAsActionIfRoom(final int showAsAction) {

		if(showAsAction == MenuItem.SHOW_AS_ACTION_IF_ROOM) {
			return MenuItem.SHOW_AS_ACTION_ALWAYS;
		}

		return showAsAction;
	}

	// Avoids IDE warnings about null pointers
	public static int getOrThrow(
			final Map<AppbarItemsPref, Integer> appbarItemsPref,
			final AppbarItemsPref key) {

		final Integer value = appbarItemsPref.get(key);

		if(value == null) {
			throw new RuntimeException("appbarItemsPref value is null");
		}

		return value;
	}

	private interface OptionsMenuListener {
	}

	public interface OptionsMenuSubredditsListener extends OptionsMenuListener {
		void onRefreshSubreddits();
	}

	public interface OptionsMenuPostsListener extends OptionsMenuListener {
		void onRefreshPosts();

		void onPastPosts();

		void onSubmitPost();

		void onSortSelected(PostSort order);

		void onSearchPosts();

		void onSubscribe();

		void onUnsubscribe();

		void onSidebar();

		void onPin();

		void onUnpin();

		void onBlock();

		void onUnblock();
	}

	public interface OptionsMenuCommentsListener extends OptionsMenuListener {
		void onRefreshComments();

		void onPastComments();

		void onSortSelected(PostCommentListingURL.Sort order);

		void onSortSelected(UserCommentListingURL.Sort order);

		void onSearchComments();
	}
}
