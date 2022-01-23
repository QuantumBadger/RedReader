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
import androidx.annotation.StringRes;
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
import org.quantumbadger.redreader.reddit.PostCommentSort;
import org.quantumbadger.redreader.reddit.PostSort;
import org.quantumbadger.redreader.reddit.UserCommentSort;
import org.quantumbadger.redreader.reddit.api.SubredditSubscriptionState;
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

		final EnumMap<AppbarItemsPref, Integer> appbarItemsPrefs
				= PrefsUtility.pref_menus_appbar_items();

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
					if(PrefsUtility.isNightMode()) {
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
									= PrefsUtility.appearance_theme();

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

	public interface Sort {
		String name();

		@StringRes int getMenuTitle();

		void onSortSelected(final AppCompatActivity activity);
	}

	//The sorts of a SortGroup should always be of the same "base type" (e.g. only top post sorts).
	private static class SortGroup {
		final Sort[] sorts;
		@StringRes final int subMenuTitle;

		SortGroup(final Sort[] sorts, final int subMenuTitle) {
			this.sorts = sorts;
			this.subMenuTitle = subMenuTitle;
		}

		boolean equalsBaseAndType(final Sort sort) {
			if(!sort.getClass().equals(sorts[0].getClass())) {
				return false;
			}

			final String baseSort1 = sorts[0].name().split("_")[0];
			final String baseSort2 = sort.name().split("_")[0];

			return baseSort1.equals(baseSort2);
		}
	}

	final static SortGroup CONTROVERSIAL_SORTS = new SortGroup(
			new PostSort[] {
					PostSort.CONTROVERSIAL_HOUR,
					PostSort.CONTROVERSIAL_DAY,
					PostSort.CONTROVERSIAL_WEEK,
					PostSort.CONTROVERSIAL_MONTH,
					PostSort.CONTROVERSIAL_YEAR,
					PostSort.CONTROVERSIAL_ALL},
			R.string.sort_posts_controversial);

	final static SortGroup TOP_SORTS = new SortGroup(
			new PostSort[] {
					PostSort.TOP_HOUR,
					PostSort.TOP_DAY,
					PostSort.TOP_WEEK,
					PostSort.TOP_MONTH,
					PostSort.TOP_YEAR,
					PostSort.TOP_ALL},
			R.string.sort_posts_top);

	final static SortGroup RELEVANCE_SORTS = new SortGroup(
			new PostSort[] {
					PostSort.RELEVANCE_HOUR,
					PostSort.RELEVANCE_DAY,
					PostSort.RELEVANCE_WEEK,
					PostSort.RELEVANCE_MONTH,
					PostSort.RELEVANCE_YEAR,
					PostSort.RELEVANCE_ALL},
			R.string.sort_posts_relevance);

	final static SortGroup NEW_SORTS = new SortGroup(
			new PostSort[] {
					PostSort.NEW_HOUR,
					PostSort.NEW_DAY,
					PostSort.NEW_WEEK,
					PostSort.NEW_MONTH,
					PostSort.NEW_YEAR,
					PostSort.NEW_ALL},
			R.string.sort_posts_new);

	final static SortGroup HOT_SORTS = new SortGroup(
			new PostSort[] {
					PostSort.HOT_HOUR,
					PostSort.HOT_DAY,
					PostSort.HOT_WEEK,
					PostSort.HOT_MONTH,
					PostSort.HOT_YEAR,
					PostSort.HOT_ALL},
			R.string.sort_posts_hot);

	final static SortGroup COMMENTS_SORTS = new SortGroup(
			new PostSort[] {
					PostSort.COMMENTS_HOUR,
					PostSort.COMMENTS_DAY,
					PostSort.COMMENTS_WEEK,
					PostSort.COMMENTS_MONTH,
					PostSort.COMMENTS_YEAR,
					PostSort.COMMENTS_ALL},
			R.string.sort_posts_comments);

	private static void addAllPostSorts(
			final AppCompatActivity activity,
			final Menu menu,
			final int showAsAction,
			final boolean includeRising,
			final boolean includeBest) {

		if(showAsAction == DO_NOT_SHOW) {
			return;
		}

		final SubMenu sortPosts = addSortSubMenu(menu, R.string.options_sort_posts, showAsAction);

		addSort(activity, sortPosts, PostSort.HOT);
		addSort(activity, sortPosts, PostSort.NEW);

		if(includeRising) {
			addSort(activity, sortPosts, PostSort.RISING);
		}

		addSortsToNewSubmenu(activity, sortPosts, CONTROVERSIAL_SORTS);

		if(includeBest) {
			addSort(activity, sortPosts, PostSort.BEST);
		}

		addSortsToNewSubmenu(activity, sortPosts, TOP_SORTS);

		sortPosts.setGroupCheckable(Menu.NONE, true, true);
	}

	private static void addAllSearchSorts(
			final AppCompatActivity activity,
			final Menu menu,
			final int showAsAction) {

		if(showAsAction == DO_NOT_SHOW) {
			return;
		}

		final SubMenu sortPosts = addSortSubMenu(menu, R.string.options_sort_posts, showAsAction);

		addSortsToNewSubmenu(activity, sortPosts, RELEVANCE_SORTS);
		addSortsToNewSubmenu(activity, sortPosts, NEW_SORTS);
		addSortsToNewSubmenu(activity, sortPosts, HOT_SORTS);
		addSortsToNewSubmenu(activity, sortPosts, TOP_SORTS);
		addSortsToNewSubmenu(activity, sortPosts, COMMENTS_SORTS);

		sortPosts.setGroupCheckable(Menu.NONE, true, true);
	}

	private static void addAllCommentSorts(
			final AppCompatActivity activity,
			final Menu menu,
			final int showAsAction) {

		if(showAsAction == DO_NOT_SHOW) {
			return;
		}

		final SubMenu sortComments = addSortSubMenu(
				menu,
				R.string.options_sort_comments,
				showAsAction);

		final PostCommentSort[] postCommentSorts = {
				PostCommentSort.BEST,
				PostCommentSort.HOT,
				PostCommentSort.NEW,
				PostCommentSort.OLD,
				PostCommentSort.CONTROVERSIAL,
				PostCommentSort.TOP,
				PostCommentSort.QA
		};

		for(final PostCommentSort sort : postCommentSorts) {
			addSort(activity, sortComments, sort);
		}

		sortComments.setGroupCheckable(Menu.NONE, true, true);
	}

	final static SortGroup CONTROVERSIAL_COMMENT_SORTS = new SortGroup(
			new UserCommentSort[] {
					UserCommentSort.CONTROVERSIAL_HOUR,
					UserCommentSort.CONTROVERSIAL_DAY,
					UserCommentSort.CONTROVERSIAL_WEEK,
					UserCommentSort.CONTROVERSIAL_MONTH,
					UserCommentSort.CONTROVERSIAL_YEAR,
					UserCommentSort.CONTROVERSIAL_ALL},
			R.string.sort_comments_controversial);

	final static SortGroup TOP_COMMENT_SORTS = new SortGroup(
			new UserCommentSort[] {
					UserCommentSort.TOP_HOUR,
					UserCommentSort.TOP_DAY,
					UserCommentSort.TOP_WEEK,
					UserCommentSort.TOP_MONTH,
					UserCommentSort.TOP_YEAR,
					UserCommentSort.TOP_ALL},
			R.string.sort_comments_top);

	private static void addAllUserCommentSorts(
			final AppCompatActivity activity,
			final Menu menu,
			final int showAsAction) {

		if(showAsAction == DO_NOT_SHOW) {
			return;
		}

		final SubMenu sortComments = addSortSubMenu(
				menu,
				R.string.options_sort_comments,
				showAsAction);

		addSort(activity, sortComments, UserCommentSort.HOT);
		addSort(activity, sortComments, UserCommentSort.NEW);

		addSortsToNewSubmenu(activity, sortComments, CONTROVERSIAL_COMMENT_SORTS);
		addSortsToNewSubmenu(activity, sortComments, TOP_COMMENT_SORTS);

		sortComments.setGroupCheckable(Menu.NONE, true, true);
	}

	private static void addSort(
			final AppCompatActivity activity,
			final Menu menu,
			final Sort order) {

		@StringRes final int menuTitle;
		if(activity instanceof OptionsMenuCommentsListener
				&& ((OptionsMenuCommentsListener)activity).getSuggestedCommentSort() != null
				&& ((OptionsMenuCommentsListener)activity).getSuggestedCommentSort()
				.equals(order)) {
			menuTitle = ((PostCommentSort)order).getSuggestedTitle();
		} else {
			menuTitle = order.getMenuTitle();
		}

		final MenuItem menuItem = menu.add(activity.getString(menuTitle))
				.setOnMenuItemClickListener(item -> {
					order.onSortSelected(activity);
					return true;
				});

		if(activity instanceof OptionsMenuPostsListener
				&& ((OptionsMenuPostsListener)activity).getPostSort() != null
				&& ((OptionsMenuPostsListener)activity).getPostSort().equals(order)) {
			menuItem.setChecked(true);
		} else if(activity instanceof OptionsMenuCommentsListener
				&& ((OptionsMenuCommentsListener)activity).getCommentSort() != null
				&& ((OptionsMenuCommentsListener)activity).getCommentSort().equals(order)) {
			menuItem.setChecked(true);
		}
	}

	private static void addSortsToNewSubmenu(
			final AppCompatActivity activity,
			final Menu menu,
			final SortGroup sortGroup) {

		final SubMenu subMenu = menu.addSubMenu(activity.getString(sortGroup.subMenuTitle));

		for(final Sort sort : sortGroup.sorts) {
			addSort(activity, subMenu, sort);
		}

		subMenu.setGroupCheckable(Menu.NONE, true, true);

		final Sort activeSort;
		if(sortGroup.sorts instanceof PostSort[]) {
			activeSort = ((OptionsMenuPostsListener)activity).getPostSort();
		} else {
			activeSort = ((OptionsMenuCommentsListener)activity).getCommentSort();
		}

		if(sortGroup.equalsBaseAndType(activeSort)) {
			menu.getItem(menu.size() - 1).setChecked(true);
		}
	}

	private static SubMenu addSortSubMenu(
			final Menu menu,
			@StringRes final int subMenuTitle,
			final int showAsAction) {

		final SubMenu sortMenu = menu.addSubMenu(
				Menu.NONE,
				AppbarItemsPref.SORT.ordinal(),
				Menu.NONE,
				subMenuTitle);

		if(showAsAction != MenuItem.SHOW_AS_ACTION_NEVER) {
			sortMenu.getItem().setIcon(R.drawable.ic_sort_dark);
			sortMenu.getItem().setShowAsAction(handleShowAsActionIfRoom(showAsAction));
		}

		return sortMenu;
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

		if(PrefsUtility.pref_menus_quick_account_switcher()
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

		PostSort getPostSort();
	}

	public interface OptionsMenuCommentsListener extends OptionsMenuListener {
		void onRefreshComments();

		void onPastComments();

		void onSortSelected(PostCommentSort order);

		void onSortSelected(UserCommentSort order);

		void onSearchComments();

		Sort getCommentSort();

		PostCommentSort getSuggestedCommentSort();
	}
}
