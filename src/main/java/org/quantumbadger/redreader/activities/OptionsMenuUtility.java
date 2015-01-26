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
import android.content.res.TypedArray;
import android.graphics.Color;
import android.graphics.drawable.ColorDrawable;
import com.actionbarsherlock.view.Menu;
import com.actionbarsherlock.view.MenuItem;
import com.actionbarsherlock.view.SubMenu;
import org.holoeverywhere.app.Activity;
import org.holoeverywhere.app.AlertDialog;
import org.holoeverywhere.preference.PreferenceManager;
import org.holoeverywhere.preference.SharedPreferences;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.common.BetterSSB;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.common.UnexpectedInternalStateException;
import org.quantumbadger.redreader.fragments.AccountListDialog;
import org.quantumbadger.redreader.listingcontrollers.PostListingController;
import org.quantumbadger.redreader.reddit.api.RedditSubredditSubscriptionManager;
import org.quantumbadger.redreader.reddit.url.PostCommentListingURL;
import org.quantumbadger.redreader.settings.SettingsActivity;

public final class OptionsMenuUtility {

	private static enum Option {
		ACCOUNTS,
		SETTINGS,
		SUBMIT_POST,
		SEARCH,
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
		SIDEBAR
	}

	public static <E extends Activity & OptionsMenuListener> void prepare(
			final E activity, final Menu menu,
			final boolean subredditsVisible, final boolean postsVisible, final boolean commentsVisible,
			final boolean areSearchResults,
			final boolean postsSortable, final boolean commentsSortable,
			final RedditSubredditSubscriptionManager.SubredditSubscriptionState subredditSubscriptionState,
			final boolean subredditHasSidebar,
			final boolean pastCommentsSupported) {

		if(subredditsVisible && !postsVisible && !commentsVisible) {
			add(activity, menu, Option.REFRESH_SUBREDDITS, false);

		} else if(!subredditsVisible && postsVisible && !commentsVisible) {
			if(postsSortable) {
				if (areSearchResults)
					addAllSearchSorts(activity, menu, true);
				else
					addAllPostSorts(activity, menu, true);
			}
			add(activity, menu, Option.REFRESH_POSTS, false);
			add(activity, menu, Option.PAST_POSTS, false);
			add(activity, menu, Option.SUBMIT_POST, false);
			add(activity, menu, Option.SEARCH, false);
			if(subredditSubscriptionState != null) {
				addSubscriptionItem(activity, menu, subredditSubscriptionState);
				if(subredditHasSidebar) add(activity, menu, Option.SIDEBAR, false);
			}

		} else if(!subredditsVisible && !postsVisible && commentsVisible) {
			if(commentsSortable) addAllCommentSorts(activity, menu, true);
			add(activity, menu, Option.REFRESH_COMMENTS, false);
			if(pastCommentsSupported) {
				add(activity, menu, Option.PAST_COMMENTS, false);
			}

		} else {

			if(postsVisible && commentsVisible) {

				final SubMenu sortMenu = menu.addSubMenu(R.string.options_sort);
				sortMenu.getItem().setIcon(R.drawable.ic_action_sort);
				sortMenu.getItem().setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS);

				if(postsSortable) {
					if (areSearchResults)
						addAllSearchSorts(activity, sortMenu, false);
					else
						addAllPostSorts(activity, sortMenu, false);
				}
				if(commentsSortable) addAllCommentSorts(activity, sortMenu, false);

				final SubMenu pastMenu = menu.addSubMenu(R.string.options_past);
				add(activity, pastMenu, Option.PAST_POSTS, true);
				if(pastCommentsSupported) {
					add(activity, pastMenu, Option.PAST_COMMENTS, true);
				}

			} else if(postsVisible) {
				if(postsSortable) {
					if (areSearchResults)
						addAllSearchSorts(activity, menu, true);
					else
						addAllPostSorts(activity, menu, true);
				}
				add(activity, menu, Option.PAST_POSTS, false);
			}

			final SubMenu refreshMenu = menu.addSubMenu(R.string.options_refresh);
			refreshMenu.getItem().setIcon(R.drawable.ic_navigation_refresh);
			refreshMenu.getItem().setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS);

			if(subredditsVisible) add(activity, refreshMenu, Option.REFRESH_SUBREDDITS, true);
			if(postsVisible) {
				add(activity, refreshMenu, Option.REFRESH_POSTS, true);
				add(activity, menu, Option.SUBMIT_POST, false);
				add(activity, menu, Option.SEARCH, false);
				if(subredditSubscriptionState != null) {
					addSubscriptionItem(activity, menu, subredditSubscriptionState);
					if(subredditHasSidebar) add(activity, menu, Option.SIDEBAR, false);
				}
			}
			if(commentsVisible) add(activity, refreshMenu, Option.REFRESH_COMMENTS, true);
		}

		add(activity, menu, Option.ACCOUNTS, false);
		add(activity, menu, Option.THEMES, false);
		add(activity, menu, Option.SETTINGS, false);
	}

	private static void addSubscriptionItem(final Activity activity, final Menu menu,
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

	private static void add(final Activity activity, final Menu menu, final Option option, final boolean longText) {

		switch(option) {

			case ACCOUNTS:
				menu.add(activity.getString(R.string.options_accounts)).setOnMenuItemClickListener(new MenuItem.OnMenuItemClickListener() {
					public boolean onMenuItemClick(final MenuItem item) {
						new AccountListDialog().show(activity);
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

			case THEMES:
				menu.add(activity.getString(R.string.options_theme)).setOnMenuItemClickListener(new MenuItem.OnMenuItemClickListener() {
					public boolean onMenuItemClick(final MenuItem item) {

						final SharedPreferences prefs = PreferenceManager.getDefaultSharedPreferences(activity);
						final PrefsUtility.AppearanceTheme currentTheme = PrefsUtility.appearance_theme(activity, prefs);

						final String[] themeNames = activity.getResources().getStringArray(R.array.pref_appearance_theme);
						final String[] themeValues = activity.getResources().getStringArray(R.array.pref_appearance_theme_return);

						int selectedPos = -1;
						for(int i = 0; i < themeValues.length; i++) {
							if(PrefsUtility.AppearanceTheme.valueOf(themeValues[i].toUpperCase()).equals(currentTheme)) {
								selectedPos = i;
								break;
							}
						}

						final AlertDialog.Builder dialog = new AlertDialog.Builder(activity);
						dialog.setTitle(R.string.pref_appearance_theme_title);

						dialog.setSingleChoiceItems(themeNames, selectedPos, new DialogInterface.OnClickListener() {
							public void onClick(DialogInterface dialog, int item) {
								final SharedPreferences.Editor editor = prefs.edit();
								editor.putString(activity.getString(R.string.pref_appearance_theme_key), themeValues[item]);
								editor.commit();
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
				if(!longText) refreshSubreddits.setIcon(R.drawable.ic_navigation_refresh);

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
				if(!longText) refreshPosts.setIcon(R.drawable.ic_navigation_refresh);

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
				menu.add(activity.getString(R.string.action_search))
						.setOnMenuItemClickListener(new MenuItem.OnMenuItemClickListener() {
							public boolean onMenuItemClick(final MenuItem item) {
								((OptionsMenuPostsListener) activity).onSearchPosts();
								return true;
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
				if(!longText) refreshComments.setIcon(R.drawable.ic_navigation_refresh);

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
								((OptionsMenuCommentsListener)activity).onPastComments();
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
							((OptionsMenuPostsListener)activity).onUnsubscribe();
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
						((OptionsMenuPostsListener)activity).onSidebar();
						return true;
					}
				});
				break;

			default:
				BugReportActivity.handleGlobalError(activity, "Unknown menu option added");
		}
	}

	private static void addAllPostSorts(final Activity activity, final Menu menu, final boolean icon) {

		final SubMenu sortPosts = menu.addSubMenu(R.string.options_sort_posts);

		if(icon) {
			sortPosts.getItem().setIcon(R.drawable.ic_action_sort);
			sortPosts.getItem().setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS);
		}

		addSort(activity, sortPosts, R.string.sort_posts_hot, PostListingController.Sort.HOT);
		addSort(activity, sortPosts, R.string.sort_posts_new, PostListingController.Sort.NEW);
		addSort(activity, sortPosts, R.string.sort_posts_rising, PostListingController.Sort.RISING);
		addSort(activity, sortPosts, R.string.sort_posts_controversial, PostListingController.Sort.CONTROVERSIAL);

		final SubMenu sortPostsTop = sortPosts.addSubMenu(R.string.sort_posts_top);

		addSort(activity, sortPostsTop, R.string.sort_posts_top_hour, PostListingController.Sort.TOP_HOUR);
		addSort(activity, sortPostsTop, R.string.sort_posts_top_today, PostListingController.Sort.TOP_DAY);
		addSort(activity, sortPostsTop, R.string.sort_posts_top_week, PostListingController.Sort.TOP_WEEK);
		addSort(activity, sortPostsTop, R.string.sort_posts_top_month, PostListingController.Sort.TOP_MONTH);
		addSort(activity, sortPostsTop, R.string.sort_posts_top_year, PostListingController.Sort.TOP_YEAR);
		addSort(activity, sortPostsTop, R.string.sort_posts_top_all, PostListingController.Sort.TOP_ALL);
	}

	private static void addAllSearchSorts(final Activity activity, final Menu menu, final boolean icon) {

		final SubMenu sortPosts = menu.addSubMenu(R.string.options_sort_posts);

		if(icon) {
			sortPosts.getItem().setIcon(R.drawable.ic_action_sort);
			sortPosts.getItem().setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS);
		}

		addSort(activity, sortPosts, R.string.sort_posts_relevance, PostListingController.Sort.RELEVANCE);
		addSort(activity, sortPosts, R.string.sort_posts_new, PostListingController.Sort.NEW);
		addSort(activity, sortPosts, R.string.sort_posts_hot, PostListingController.Sort.HOT);
		addSort(activity, sortPosts, R.string.sort_posts_top, PostListingController.Sort.TOP);
		addSort(activity, sortPosts, R.string.sort_posts_comments, PostListingController.Sort.COMMENTS);
	}

	private static void addSort(final Activity activity, final Menu menu, final int name, final PostListingController.Sort order) {

		menu.add(activity.getString(name)).setOnMenuItemClickListener(new MenuItem.OnMenuItemClickListener() {
			public boolean onMenuItemClick(final MenuItem item) {
				((OptionsMenuPostsListener)activity).onSortSelected(order);
				return true;
			}
		});
	}

	private static void addAllCommentSorts(final Activity activity, final Menu menu, final boolean icon) {

		final SubMenu sortComments = menu.addSubMenu(R.string.options_sort_comments);

		if(icon) {
			sortComments.getItem().setIcon(R.drawable.ic_action_sort);
			sortComments.getItem().setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS);
		}

		addSort(activity, sortComments, R.string.sort_comments_best, PostCommentListingURL.Sort.BEST);
		addSort(activity, sortComments, R.string.sort_comments_hot, PostCommentListingURL.Sort.HOT);
		addSort(activity, sortComments, R.string.sort_comments_new, PostCommentListingURL.Sort.NEW);
		addSort(activity, sortComments, R.string.sort_comments_old, PostCommentListingURL.Sort.OLD);
		addSort(activity, sortComments, R.string.sort_comments_controversial, PostCommentListingURL.Sort.CONTROVERSIAL);
		addSort(activity, sortComments, R.string.sort_comments_top, PostCommentListingURL.Sort.TOP);
	}

	private static void addSort(final Activity activity, final Menu menu, final int name, final PostCommentListingURL.Sort order) {

		menu.add(activity.getString(name)).setOnMenuItemClickListener(new MenuItem.OnMenuItemClickListener() {
			public boolean onMenuItemClick(final MenuItem item) {
				((OptionsMenuCommentsListener)activity).onSortSelected(order);
				return true;
			}
		});
	}

	private static interface OptionsMenuListener {}

	public static interface OptionsMenuSubredditsListener extends OptionsMenuListener {
		public void onRefreshSubreddits();
	}

	public static interface OptionsMenuPostsListener extends OptionsMenuListener {
		public void onRefreshPosts();
		public void onPastPosts();
		public void onSubmitPost();
		public void onSortSelected(PostListingController.Sort order);
		public void onSearchPosts();
		public void onSubscribe();
		public void onUnsubscribe();
		public void onSidebar();
	}

	public static interface OptionsMenuCommentsListener extends OptionsMenuListener {
		public void onRefreshComments();
		public void onPastComments();
		public void onSortSelected(PostCommentListingURL.Sort order);
	}

	public static void fixActionBar(final Activity activity, final String title) {
		final TypedArray attr = activity.obtainStyledAttributes(new int[] {R.attr.rrActionBarCol});
		final int actionbarCol = attr.getColor(0, 0);
		activity.getSupportActionBar().setBackgroundDrawable(new ColorDrawable(actionbarCol));

		final BetterSSB sb = new BetterSSB();
		sb.append(title, BetterSSB.FOREGROUND_COLOR, Color.WHITE, 0, 1f);
		activity.getSupportActionBar().setTitle(sb.get());
	}
}
