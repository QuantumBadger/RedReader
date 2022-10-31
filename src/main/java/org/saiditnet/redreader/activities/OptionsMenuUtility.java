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
import android.preference.PreferenceManager;
import android.support.v7.app.AppCompatActivity;
import android.view.Menu;
import android.view.MenuItem;
import android.view.SubMenu;
import org.saiditnet.redreader.R;
import org.saiditnet.redreader.common.General;
import org.saiditnet.redreader.common.PrefsUtility;
import org.saiditnet.redreader.common.UnexpectedInternalStateException;
import org.saiditnet.redreader.fragments.AccountListDialog;
import org.saiditnet.redreader.reddit.PostSort;
import org.saiditnet.redreader.reddit.api.RedditSubredditSubscriptionManager;
import org.saiditnet.redreader.reddit.url.PostCommentListingURL;
import org.saiditnet.redreader.reddit.url.UserCommentListingURL;
import org.saiditnet.redreader.settings.SettingsActivity;

import java.util.EnumSet;

public final class OptionsMenuUtility {

	public enum OptionsMenuItemsPref {
		ACCOUNTS, THEME, CLOSE_ALL, PAST, SUBMIT_POST, SEARCH, REPLY, PIN, BLOCK
	}

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
			final E activity, final Menu menu,
			final boolean subredditsVisible, final boolean postsVisible, final boolean commentsVisible,
			final boolean areSearchResults, final boolean isUserPostListing,
			final boolean isUserCommentListing, final boolean postsSortable, final boolean commentsSortable,
			final boolean isFrontPage,
			final RedditSubredditSubscriptionManager.SubredditSubscriptionState subredditSubscriptionState,
			final boolean subredditHasSidebar,
			final boolean pastCommentsSupported,
			final Boolean subredditPinned,
			final Boolean subredditBlocked) {

		final SharedPreferences preferences = PreferenceManager.getDefaultSharedPreferences(activity);
		final EnumSet<OptionsMenuItemsPref> optionsMenuItemsPrefs = PrefsUtility.pref_menus_optionsmenu_items(activity, preferences);

		if(subredditsVisible && !postsVisible && !commentsVisible) {
			add(activity, menu, Option.REFRESH_SUBREDDITS, false);

		} else if(!subredditsVisible && postsVisible && !commentsVisible) {
			if(postsSortable) {

				if (areSearchResults)
					addAllSearchSorts(activity, menu, true);
				else
					addAllPostSorts(activity, menu, true, !isUserPostListing, isFrontPage);
			}
			add(activity, menu, Option.REFRESH_POSTS, false);
			if(optionsMenuItemsPrefs.contains(OptionsMenuItemsPref.PAST)) add(activity, menu, Option.PAST_POSTS, false);
			if(optionsMenuItemsPrefs.contains(OptionsMenuItemsPref.SUBMIT_POST)) add(activity, menu, Option.SUBMIT_POST, false);
			if(optionsMenuItemsPrefs.contains(OptionsMenuItemsPref.SEARCH)) add(activity, menu, Option.SEARCH, false);

			if(subredditPinned != null && optionsMenuItemsPrefs.contains(OptionsMenuItemsPref.PIN)) {
				if(subredditPinned) {
					add(activity, menu, Option.UNPIN, false);
				} else {
					add(activity, menu, Option.PIN, false);
				}
			}

			if (subredditBlocked != null && optionsMenuItemsPrefs.contains(OptionsMenuItemsPref.BLOCK)) {
				if(subredditBlocked) {
					add(activity, menu, Option.UNBLOCK, false);
				} else {
					add(activity, menu, Option.BLOCK, false);
				}
			}

			if(subredditSubscriptionState != null) {
				addSubscriptionItem(activity, menu, subredditSubscriptionState);
			}

			if(subredditHasSidebar) add(activity, menu, Option.SIDEBAR, false);

		} else if(!subredditsVisible && !postsVisible && commentsVisible) {
			if(commentsSortable && !isUserCommentListing)
				addAllCommentSorts(activity, menu, true);
			else if(commentsSortable && isUserCommentListing)
				addAllUserCommentSorts(activity, menu, true);
			add(activity, menu, Option.REFRESH_COMMENTS, false);
			if(optionsMenuItemsPrefs.contains(OptionsMenuItemsPref.SEARCH)) add(activity, menu, Option.SEARCH, false);
			if(pastCommentsSupported) {
				if(optionsMenuItemsPrefs.contains(OptionsMenuItemsPref.PAST)) add(activity, menu, Option.PAST_COMMENTS, false);
			}

		} else {

			if(postsVisible && commentsVisible) {

				final SubMenu sortMenu = menu.addSubMenu(R.string.options_sort);
				sortMenu.getItem().setIcon(R.drawable.ic_sort_dark);
				sortMenu.getItem().setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS);

				if(postsSortable) {
					if (areSearchResults)
						addAllSearchSorts(activity, sortMenu, false);
					else
						addAllPostSorts(activity, sortMenu, false, !isUserPostListing, isFrontPage);
				}
				if(commentsSortable) addAllCommentSorts(activity, sortMenu, false);

				if(optionsMenuItemsPrefs.contains(OptionsMenuItemsPref.PAST)) {
					final SubMenu pastMenu = menu.addSubMenu(R.string.options_past);
					add(activity, pastMenu, Option.PAST_POSTS, true);
					if(pastCommentsSupported) {
						add(activity, pastMenu, Option.PAST_COMMENTS, true);
					}
				}
				if(optionsMenuItemsPrefs.contains(OptionsMenuItemsPref.SEARCH)) add(activity, menu, Option.SEARCH_COMMENTS, false);

			} else if(postsVisible) {
				if(postsSortable) {
					if (areSearchResults)
						addAllSearchSorts(activity, menu, true);
					else
						addAllPostSorts(activity, menu, true, !isUserPostListing, isFrontPage);
				}
				add(activity, menu, Option.PAST_POSTS, false);
			}

			final SubMenu refreshMenu = menu.addSubMenu(R.string.options_refresh);
			refreshMenu.getItem().setIcon(R.drawable.ic_refresh_dark);
			refreshMenu.getItem().setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS);

			if(subredditsVisible) add(activity, refreshMenu, Option.REFRESH_SUBREDDITS, true);
			if(postsVisible) {
				add(activity, refreshMenu, Option.REFRESH_POSTS, true);
				if(optionsMenuItemsPrefs.contains(OptionsMenuItemsPref.SUBMIT_POST)) add(activity, menu, Option.SUBMIT_POST, false);
				if(optionsMenuItemsPrefs.contains(OptionsMenuItemsPref.SEARCH)) add(activity, menu, Option.SEARCH, false);

				if(subredditPinned != null && optionsMenuItemsPrefs.contains(OptionsMenuItemsPref.PIN)) {
					if(subredditPinned) {
						add(activity, menu, Option.UNPIN, false);
					} else {
						add(activity, menu, Option.PIN, false);
					}
				}

				if(subredditSubscriptionState != null) {
					addSubscriptionItem(activity, menu, subredditSubscriptionState);
				}

				if(subredditHasSidebar) add(activity, menu, Option.SIDEBAR, false);
			}
			if(commentsVisible) add(activity, refreshMenu, Option.REFRESH_COMMENTS, true);
		}

		if(optionsMenuItemsPrefs.contains(OptionsMenuItemsPref.ACCOUNTS)) add(activity, menu, Option.ACCOUNTS, false);
		if(optionsMenuItemsPrefs.contains(OptionsMenuItemsPref.THEME)) add(activity, menu, Option.THEMES, false);
		add(activity, menu, Option.SETTINGS, false);
		if(optionsMenuItemsPrefs.contains(OptionsMenuItemsPref.CLOSE_ALL)) add(activity, menu, Option.CLOSE_ALL, false);
	}

	private static void addSubscriptionItem(final BaseActivity activity, final Menu menu,
			final RedditSubredditSubscriptionManager.SubredditSubscriptionState subredditSubscriptionState) {

		if(subredditSubscriptionState == null) return;

		switch(subredditSubscriptionState) {
			case NOT_SUBSCRIBED:
				add(activity, menu, Option.SUBSCRIBE, false);
				return;
			case SUBSCRIBED:
				add(activity, menu, Option.UNSUBSCRIBE, false);
				return;
			case SUBSCRIBING:
				add(activity, menu, Option.SUBSCRIBING, false);
				return;
			case UNSUBSCRIBING:
				add(activity, menu, Option.UNSUBSCRIBING, false);
				return;
			default:
				throw new UnexpectedInternalStateException("Unknown subscription state");
		}

	}

	private static void add(final BaseActivity activity, final Menu menu, final Option option, final boolean longText) {

		switch(option) {

			case ACCOUNTS:
				menu.add(activity.getString(R.string.options_accounts)).setOnMenuItemClickListener(new MenuItem.OnMenuItemClickListener() {
					public boolean onMenuItemClick(final MenuItem item) {
						new AccountListDialog().show(activity.getSupportFragmentManager(), null);
						return true;
					}
				});
				break;

			case SETTINGS:
				menu.add(activity.getString(R.string.options_settings)).setOnMenuItemClickListener(new MenuItem.OnMenuItemClickListener() {
					public boolean onMenuItemClick(final MenuItem item) {
						final Intent intent = new Intent(activity, SettingsActivity.class);
						activity.startActivityForResult(intent, 1);
						return true;
					}
				});
				break;

			case CLOSE_ALL:
				if(!(activity instanceof MainActivity)) {
					menu.add(activity.getString(R.string.options_close_all)).setOnMenuItemClickListener(new MenuItem.OnMenuItemClickListener() {
						public boolean onMenuItemClick(final MenuItem item) {
							activity.closeAllExceptMain();
							return true;
						}
					});
				}
				break;

			case THEMES:
				menu.add(activity.getString(R.string.options_theme)).setOnMenuItemClickListener(new MenuItem.OnMenuItemClickListener() {
					public boolean onMenuItemClick(final MenuItem item) {

						final SharedPreferences prefs = PreferenceManager.getDefaultSharedPreferences(activity);
						final PrefsUtility.AppearanceTheme currentTheme = PrefsUtility.appearance_theme(activity, prefs);

						final String[] themeNames = activity.getResources().getStringArray(R.array.pref_appearance_theme);
						final String[] themeValues = activity.getResources().getStringArray(R.array.pref_appearance_theme_return);

						int selectedPos = -1;
						for(int i = 0; i < themeValues.length; i++) {
							if(PrefsUtility.AppearanceTheme.valueOf(General.asciiUppercase(themeValues[i])).equals(currentTheme)) {
								selectedPos = i;
								break;
							}
						}

						final AlertDialog.Builder dialog = new AlertDialog.Builder(activity);
						dialog.setTitle(R.string.pref_appearance_theme_title);

						dialog.setSingleChoiceItems(themeNames, selectedPos, new DialogInterface.OnClickListener() {
							@Override
							public void onClick(DialogInterface dialog, int item) {
								final SharedPreferences.Editor editor = prefs.edit();
								editor.putString(activity.getString(R.string.pref_appearance_theme_key), themeValues[item]);
								editor.apply();
								dialog.dismiss();
							}
						});

						final AlertDialog alert = dialog.create();
						alert.show();
						return true;
					}
				});
				break;

			case REFRESH_SUBREDDITS:
				final MenuItem refreshSubreddits = menu.add(activity.getString(R.string.options_refresh_subreddits))
						.setOnMenuItemClickListener(new MenuItem.OnMenuItemClickListener() {
							public boolean onMenuItemClick(final MenuItem item) {
								((OptionsMenuSubredditsListener) activity).onRefreshSubreddits();
								return true;
							}
						});

				refreshSubreddits.setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS);
				if(!longText) refreshSubreddits.setIcon(R.drawable.ic_refresh_dark);

				break;

			case REFRESH_POSTS:
				final MenuItem refreshPosts = menu.add(activity.getString(R.string.options_refresh_posts))
						.setOnMenuItemClickListener(new MenuItem.OnMenuItemClickListener() {
							public boolean onMenuItemClick(final MenuItem item) {
								((OptionsMenuPostsListener) activity).onRefreshPosts();
								return true;
							}
						});

				refreshPosts.setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS);
				if(!longText) refreshPosts.setIcon(R.drawable.ic_refresh_dark);

				break;

			case SUBMIT_POST:
				menu.add(activity.getString(R.string.options_submit_post))
						.setOnMenuItemClickListener(new MenuItem.OnMenuItemClickListener() {
							public boolean onMenuItemClick(final MenuItem item) {
								((OptionsMenuPostsListener) activity).onSubmitPost();
								return true;
							}
						});

				break;

			case SEARCH:
				menu.add(Menu.NONE, Menu.NONE, 1, activity.getString(R.string.action_search))
						.setOnMenuItemClickListener(new MenuItem.OnMenuItemClickListener() {
							public boolean onMenuItemClick(final MenuItem item) {
								if (activity instanceof OptionsMenuPostsListener) {
									((OptionsMenuPostsListener) activity).onSearchPosts();
									return true;
								} else if (activity instanceof OptionsMenuCommentsListener) {
									((OptionsMenuCommentsListener) activity).onSearchComments();
									return true;
								} else {
									return false;
								}
							}
						});

				break;

			case SEARCH_COMMENTS:
				menu.add(Menu.NONE, Menu.NONE, 1, activity.getString(R.string.action_search_comments))
						.setOnMenuItemClickListener(new MenuItem.OnMenuItemClickListener() {
							@Override
							public boolean onMenuItemClick(MenuItem item) {
								if (activity instanceof OptionsMenuCommentsListener) {
									((OptionsMenuCommentsListener) activity).onSearchComments();
									return true;
								}
								return false;
							}
						});
				break;

			case REFRESH_COMMENTS:
				final MenuItem refreshComments = menu.add(activity.getString(R.string.options_refresh_comments))
						.setOnMenuItemClickListener(new MenuItem.OnMenuItemClickListener() {
							public boolean onMenuItemClick(final MenuItem item) {
								((OptionsMenuCommentsListener) activity).onRefreshComments();
								return true;
							}
						});

				refreshComments.setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS);
				if(!longText) refreshComments.setIcon(R.drawable.ic_refresh_dark);

				break;

			case PAST_POSTS:
				menu.add(activity.getString(longText ? R.string.options_past_posts : R.string.options_past))
						.setOnMenuItemClickListener(new MenuItem.OnMenuItemClickListener() {
							public boolean onMenuItemClick(final MenuItem item) {
								((OptionsMenuPostsListener)activity).onPastPosts();
								return true;
							}
						});
				break;

			case PAST_COMMENTS:
				menu.add(activity.getString(longText ? R.string.options_past_comments : R.string.options_past))
						.setOnMenuItemClickListener(new MenuItem.OnMenuItemClickListener() {
							public boolean onMenuItemClick(final MenuItem item) {
								((OptionsMenuCommentsListener) activity).onPastComments();
								return true;
							}
						});
				break;

			case SUBSCRIBE:
				menu.add(activity.getString(R.string.options_subscribe))
						.setOnMenuItemClickListener(new MenuItem.OnMenuItemClickListener() {
							public boolean onMenuItemClick(final MenuItem item) {
								((OptionsMenuPostsListener)activity).onSubscribe();
								return true;
							}
						});
				break;

			case UNSUBSCRIBE:
				menu.add(activity.getString(R.string.options_unsubscribe))
					.setOnMenuItemClickListener(new MenuItem.OnMenuItemClickListener() {
						public boolean onMenuItemClick(final MenuItem item) {
							((OptionsMenuPostsListener) activity).onUnsubscribe();
							return true;
						}
					});
				break;

			case UNSUBSCRIBING:
				menu.add(activity.getString(R.string.options_unsubscribing)).setEnabled(false);
				break;

			case SUBSCRIBING:
				menu.add(activity.getString(R.string.options_subscribing)).setEnabled(false);
				break;

			case SIDEBAR:
				menu.add(activity.getString(R.string.options_sidebar)).setOnMenuItemClickListener(new MenuItem.OnMenuItemClickListener() {
					public boolean onMenuItemClick(final MenuItem item) {
						((OptionsMenuPostsListener) activity).onSidebar();
						return true;
					}
				});
				break;

			case PIN:
				menu.add(activity.getString(R.string.pin_subreddit)).setOnMenuItemClickListener(new MenuItem.OnMenuItemClickListener() {
					public boolean onMenuItemClick(final MenuItem item) {
						((OptionsMenuPostsListener) activity).onPin();
						return true;
					}
				});
				break;

			case UNPIN:
				menu.add(activity.getString(R.string.unpin_subreddit)).setOnMenuItemClickListener(new MenuItem.OnMenuItemClickListener() {
					public boolean onMenuItemClick(final MenuItem item) {
						((OptionsMenuPostsListener)activity).onUnpin();
						return true;
					}
				});
				break;

			case BLOCK:
				menu.add(activity.getString(R.string.block_subreddit)).setOnMenuItemClickListener(new MenuItem.OnMenuItemClickListener() {
					public boolean onMenuItemClick(final MenuItem item) {
						((OptionsMenuPostsListener)activity).onBlock();
						return true;
					}
				});

				break;

			case UNBLOCK:
				menu.add(activity.getString(R.string.unblock_subreddit)).setOnMenuItemClickListener(new MenuItem.OnMenuItemClickListener() {
					public boolean onMenuItemClick(final MenuItem item) {
						((OptionsMenuPostsListener)activity).onUnblock();
						return true;
					}
				});

				break;

			default:
				BugReportActivity.handleGlobalError(activity, "Unknown menu option added");
		}
	}

	private static void addAllPostSorts(
			final AppCompatActivity activity,
			final Menu menu,
			final boolean icon,
			final boolean includeRising,
			final boolean includeBest) {

		final SubMenu sortPosts = menu.addSubMenu(R.string.options_sort_posts);

		if(icon) {
			sortPosts.getItem().setIcon(R.drawable.ic_sort_dark);
			sortPosts.getItem().setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS);
		}

		addSort(activity, sortPosts, R.string.sort_posts_hot, PostSort.HOT);
		addSort(activity, sortPosts, R.string.sort_posts_new, PostSort.NEW);
		// if(includeRising)
		// 	addSort(activity, sortPosts, R.string.sort_posts_rising, PostSort.RISING);
		addSort(activity, sortPosts, R.string.sort_posts_controversial, PostSort.CONTROVERSIAL);
		// if(includeBest)
		// 	addSort(activity, sortPosts, R.string.sort_posts_best, PostSort.BEST);

		final SubMenu sortPostsTop = sortPosts.addSubMenu(R.string.sort_posts_top);

		addSort(activity, sortPostsTop, R.string.sort_posts_top_hour, PostSort.TOP_HOUR);
		addSort(activity, sortPostsTop, R.string.sort_posts_top_today, PostSort.TOP_DAY);
		addSort(activity, sortPostsTop, R.string.sort_posts_top_week, PostSort.TOP_WEEK);
		addSort(activity, sortPostsTop, R.string.sort_posts_top_month, PostSort.TOP_MONTH);
		addSort(activity, sortPostsTop, R.string.sort_posts_top_year, PostSort.TOP_YEAR);
		addSort(activity, sortPostsTop, R.string.sort_posts_top_all, PostSort.TOP_ALL);
	}

	private static void addAllSearchSorts(final AppCompatActivity activity, final Menu menu, final boolean icon) {

		final SubMenu sortPosts = menu.addSubMenu(R.string.options_sort_posts);

		if(icon) {
			sortPosts.getItem().setIcon(R.drawable.ic_sort_dark);
			sortPosts.getItem().setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS);
		}

		addSort(activity, sortPosts, R.string.sort_posts_relevance, PostSort.RELEVANCE);
		addSort(activity, sortPosts, R.string.sort_posts_new, PostSort.NEW);
		addSort(activity, sortPosts, R.string.sort_posts_hot, PostSort.HOT);
		addSort(activity, sortPosts, R.string.sort_posts_top, PostSort.TOP);
		addSort(activity, sortPosts, R.string.sort_posts_comments, PostSort.COMMENTS);
	}

	private static void addSort(final AppCompatActivity activity, final Menu menu, final int name, final PostSort order) {

		menu.add(activity.getString(name)).setOnMenuItemClickListener(new MenuItem.OnMenuItemClickListener() {
			public boolean onMenuItemClick(final MenuItem item) {
				((OptionsMenuPostsListener)activity).onSortSelected(order);
				return true;
			}
		});
	}

	private static void addAllCommentSorts(final AppCompatActivity activity, final Menu menu, final boolean icon) {

		final SubMenu sortComments = menu.addSubMenu(R.string.options_sort_comments);

		if(icon) {
			sortComments.getItem().setIcon(R.drawable.ic_sort_dark);
			sortComments.getItem().setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS);
		}

		// addSort(activity, sortComments, R.string.sort_comments_best, PostCommentListingURL.Sort.BEST);
		addSort(activity, sortComments, R.string.sort_comments_hot, PostCommentListingURL.Sort.HOT);
		addSort(activity, sortComments, R.string.sort_comments_new, PostCommentListingURL.Sort.NEW);
		addSort(activity, sortComments, R.string.sort_comments_old, PostCommentListingURL.Sort.OLD);
		addSort(activity, sortComments, R.string.sort_comments_controversial, PostCommentListingURL.Sort.CONTROVERSIAL);
		addSort(activity, sortComments, R.string.sort_comments_top, PostCommentListingURL.Sort.TOP);
		addSort(activity, sortComments, R.string.sort_comments_qa, PostCommentListingURL.Sort.QA);
	}

	private static void addSort(final AppCompatActivity activity, final Menu menu, final int name, final PostCommentListingURL.Sort order) {

		menu.add(activity.getString(name)).setOnMenuItemClickListener(new MenuItem.OnMenuItemClickListener() {
			public boolean onMenuItemClick(final MenuItem item) {
				((OptionsMenuCommentsListener)activity).onSortSelected(order);
				return true;
			}
		});
	}

	private static void addAllUserCommentSorts(final AppCompatActivity activity, final Menu menu, final boolean icon) {

		final SubMenu sortComments = menu.addSubMenu(R.string.options_sort_comments);

		if(icon) {
			sortComments.getItem().setIcon(R.drawable.ic_sort_dark);
			sortComments.getItem().setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS);
		}

		addSort(activity, sortComments, R.string.sort_comments_hot, UserCommentListingURL.Sort.HOT);
		addSort(activity, sortComments, R.string.sort_comments_new, UserCommentListingURL.Sort.NEW);
		addSort(activity, sortComments, R.string.sort_comments_controversial, UserCommentListingURL.Sort.CONTROVERSIAL);

		final SubMenu sortCommentsTop = sortComments.addSubMenu(R.string.sort_comments_top);

		addSort(activity, sortCommentsTop, R.string.sort_posts_top_hour, UserCommentListingURL.Sort.TOP_HOUR);
		addSort(activity, sortCommentsTop, R.string.sort_posts_top_today, UserCommentListingURL.Sort.TOP_DAY);
		addSort(activity, sortCommentsTop, R.string.sort_posts_top_week, UserCommentListingURL.Sort.TOP_WEEK);
		addSort(activity, sortCommentsTop, R.string.sort_posts_top_month, UserCommentListingURL.Sort.TOP_MONTH);
		addSort(activity, sortCommentsTop, R.string.sort_posts_top_year, UserCommentListingURL.Sort.TOP_YEAR);
		addSort(activity, sortCommentsTop, R.string.sort_posts_top_all, UserCommentListingURL.Sort.TOP_ALL);
	}

	private static void addSort(final AppCompatActivity activity, final Menu menu, final int name, final UserCommentListingURL.Sort order) {

		menu.add(activity.getString(name)).setOnMenuItemClickListener(new MenuItem.OnMenuItemClickListener() {
			@Override
			public boolean onMenuItemClick(MenuItem menuItem) {
				((OptionsMenuCommentsListener)activity).onSortSelected(order);
				return true;
			}
		});
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
