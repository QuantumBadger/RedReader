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

package org.quantumbadger.redreader.common;

import android.app.Activity;
import android.content.Context;
import android.content.SharedPreferences;
import android.content.res.Resources;
import android.preference.PreferenceManager;
import android.support.annotation.NonNull;
import android.util.DisplayMetrics;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.activities.OptionsMenuUtility;
import org.quantumbadger.redreader.fragments.MainMenuFragment;
import org.quantumbadger.redreader.io.WritableHashSet;
import org.quantumbadger.redreader.reddit.prepared.RedditPreparedPost;
import org.quantumbadger.redreader.reddit.things.RedditSubreddit;
import org.quantumbadger.redreader.reddit.url.PostCommentListingURL;

import java.util.ArrayList;
import java.util.Collections;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;

public final class PrefsUtility {

	private static <E> Set<E> setFromArray(E[] data) {
		final HashSet<E> result = new HashSet<>(data.length);
		Collections.addAll(result, data);
		return result;
	}

	private static String getString(final int id, final String defaultValue, final Context context, final SharedPreferences sharedPreferences) {
		return sharedPreferences.getString(context.getString(id), defaultValue);
	}

	public static Set<String> getStringSet(final int id, final int defaultArrayRes, final Context context, final SharedPreferences sharedPreferences) {
		return sharedPreferences.getStringSet(context.getString(id), setFromArray(context.getResources().getStringArray(defaultArrayRes)));
	}

	private static boolean getBoolean(final int id, final boolean defaultValue, final Context context, final SharedPreferences sharedPreferences) {
		return sharedPreferences.getBoolean(context.getString(id), defaultValue);
	}

	private static long getLong(final int id, final long defaultValue, final Context context, final SharedPreferences sharedPreferences) {
		return sharedPreferences.getLong(context.getString(id), defaultValue);
	}

	public static boolean isReLayoutRequired(final Context context, final String key) {
		return context.getString(R.string.pref_appearance_twopane_key).equals(key)
				|| context.getString(R.string.pref_appearance_theme_key).equals(key)
				|| context.getString(R.string.pref_menus_mainmenu_useritems_key).equals(key);
	}

	public static boolean isRefreshRequired(final Context context, final String key) {
		return key.startsWith("pref_appearance")
				|| key.equals(context.getString(R.string.pref_behaviour_fling_post_left_key))
				|| key.equals(context.getString(R.string.pref_behaviour_fling_post_right_key))
				|| key.equals(context.getString(R.string.pref_behaviour_nsfw_key))
				|| key.equals(context.getString(R.string.pref_behaviour_postcount_key))
				|| key.equals(context.getString(R.string.pref_behaviour_comment_min_key));
	}

	public static boolean isRestartRequired(Context context, String key) {
		return context.getString(R.string.pref_appearance_theme_key).equals(key)
				|| context.getString(R.string.pref_appearance_langforce_key).equals(key)
				|| context.getString(R.string.pref_behaviour_bezel_toolbar_swipezone_key).equals(key)
				|| context.getString(R.string.pref_appearance_hide_username_main_menu_key).equals(key);
	}

	///////////////////////////////
	// pref_appearance
	///////////////////////////////

	// pref_appearance_twopane

	public static enum AppearanceTwopane {
		NEVER, AUTO, FORCE
	}

	public static AppearanceTwopane appearance_twopane(final Context context, final SharedPreferences sharedPreferences) {
		return AppearanceTwopane.valueOf(General.asciiUppercase(getString(R.string.pref_appearance_twopane_key, "auto", context, sharedPreferences)));
	}

	public static enum AppearanceTheme {
		RED, GREEN, BLUE, LTBLUE, ORANGE, GRAY, NIGHT
	}

	public static boolean isNightMode(final Context context) {
		return appearance_theme(context, PreferenceManager.getDefaultSharedPreferences(context)) == AppearanceTheme.NIGHT;
	}

	public static AppearanceTheme appearance_theme(final Context context, final SharedPreferences sharedPreferences) {
		return AppearanceTheme.valueOf(General.asciiUppercase(getString(R.string.pref_appearance_theme_key, "red", context, sharedPreferences)));
	}

	public static void applyTheme(@NonNull final Activity activity) {

		final SharedPreferences prefs = PreferenceManager.getDefaultSharedPreferences(activity);

		final AppearanceTheme theme = appearance_theme(activity, prefs);

		switch(theme) {
			case RED:
				activity.setTheme(R.style.RR_Light_Red);
				break;

			case GREEN:
				activity.setTheme(R.style.RR_Light_Green);
				break;

			case BLUE:
				activity.setTheme(R.style.RR_Light_Blue);
				break;

			case LTBLUE:
				activity.setTheme(R.style.RR_Light_LtBlue);
				break;

			case ORANGE:
				activity.setTheme(R.style.RR_Light_Orange);
				break;

			case GRAY:
				activity.setTheme(R.style.RR_Light_Gray);
				break;

			case NIGHT:
				activity.setTheme(R.style.RR_Dark);
				break;
		}

		applyLanguage(activity, prefs);
	}

	public static void applySettingsTheme(@NonNull final Activity activity) {

		final SharedPreferences prefs = PreferenceManager.getDefaultSharedPreferences(activity);
		activity.setTheme(R.style.RR_Settings);
		applyLanguage(activity, prefs);
	}

	private static void applyLanguage(final Activity activity, final SharedPreferences prefs) {

		final String lang = getString(R.string.pref_appearance_langforce_key, "auto", activity, prefs);

		for(final Resources res : new Resources[] {
			activity.getResources(),
			activity.getApplication().getResources()
		}) {

			final DisplayMetrics dm = res.getDisplayMetrics();
			final android.content.res.Configuration conf = res.getConfiguration();

			if(!lang.equals("auto")) {
				conf.locale = new Locale(lang);
			} else {
				conf.locale = Locale.getDefault();
			}

			res.updateConfiguration(conf, dm);
		}
	}

	public static enum AppearanceThumbnailsShow {
		NEVER, WIFIONLY, ALWAYS
	}

	public static AppearanceThumbnailsShow appearance_thumbnails_show(final Context context, final SharedPreferences sharedPreferences) {

		if(!getBoolean(R.string.pref_appearance_thumbnails_show_key, true, context,  sharedPreferences)) {
			return AppearanceThumbnailsShow.NEVER;
		} else if(getBoolean(R.string.pref_appearance_thumbnails_wifionly_key, false, context, sharedPreferences)) {
			return AppearanceThumbnailsShow.WIFIONLY;
		} else {
			return AppearanceThumbnailsShow.ALWAYS;
		}
	}

	public static boolean appearance_thumbnails_nsfw_show(final Context context, final SharedPreferences sharedPreferences) {
		return getBoolean(R.string.pref_appearance_thumbnails_nsfw_show_key, false, context, sharedPreferences);
	}

	public static float appearance_fontscale_comments(final Context context, final SharedPreferences sharedPreferences) {
		return Float.valueOf(getString(R.string.pref_appearance_fontscale_comments_key, "1", context,  sharedPreferences));
	}

	public static float appearance_fontscale_inbox(final Context context, final SharedPreferences sharedPreferences) {
		return Float.valueOf(getString(R.string.pref_appearance_fontscale_inbox_key, "1", context,  sharedPreferences));
	}

	public static float appearance_fontscale_posts(final Context context, final SharedPreferences sharedPreferences) {
		return Float.valueOf(getString(R.string.pref_appearance_fontscale_posts_key, "1", context,  sharedPreferences));
	}

	public static boolean pref_appearance_hide_username_main_menu(final Context context, final SharedPreferences sharedPreferences) {
		return getBoolean(R.string.pref_appearance_hide_username_main_menu_key, false, context, sharedPreferences);
	}

	public static boolean pref_appearance_linkbuttons(final Context context, final SharedPreferences sharedPreferences) {
		return getBoolean(R.string.pref_appearance_linkbuttons_key, true, context, sharedPreferences);
	}

	public static boolean pref_appearance_indentlines(final Context context, final SharedPreferences sharedPreferences) {
		return getBoolean(R.string.pref_appearance_indentlines_key, false, context, sharedPreferences);
	}

	public static enum AppearanceCommentHeaderItem {
		AUTHOR, FLAIR, SCORE, AGE, GOLD
	}

	public static EnumSet<AppearanceCommentHeaderItem> appearance_comment_header_items(final Context context, final SharedPreferences sharedPreferences) {

		final Set<String> strings = getStringSet(R.string.pref_appearance_comment_header_items_key, R.array.pref_appearance_comment_header_items_default, context, sharedPreferences);

		final EnumSet<AppearanceCommentHeaderItem> result = EnumSet.noneOf(AppearanceCommentHeaderItem.class);
		for(String s : strings) {

			if(s.equalsIgnoreCase("ups_downs")) continue;

			try {
				result.add(AppearanceCommentHeaderItem.valueOf(s.toUpperCase()));
			} catch(IllegalArgumentException e) {
				// Ignore -- this option no longer exists
			}
		}

		return result;
	}

	///////////////////////////////
	// pref_behaviour
	///////////////////////////////

	public static boolean pref_behaviour_skiptofrontpage(final Context context, final SharedPreferences sharedPreferences) {
		return getBoolean(R.string.pref_behaviour_skiptofrontpage_key, false, context, sharedPreferences);
	}

	public static boolean pref_behaviour_useinternalbrowser(final Context context, final SharedPreferences sharedPreferences) {
		return getBoolean(R.string.pref_behaviour_useinternalbrowser_key, true, context, sharedPreferences);
	}

	public static boolean pref_behaviour_notifications(final Context context, final SharedPreferences sharedPreferences) {
		return getBoolean(R.string.pref_behaviour_notifications_key, true, context, sharedPreferences);
	}

	public static int pref_behaviour_bezel_toolbar_swipezone_dp(final Context context, final SharedPreferences sharedPreferences) {
		try {
			return Integer.parseInt(getString(R.string.pref_behaviour_bezel_toolbar_swipezone_key, "10", context, sharedPreferences));
		} catch(Throwable e) {
			return 10;
		}
	}

	public static int pref_behaviour_gallery_swipe_length_dp(final Context context, final SharedPreferences sharedPreferences) {
		try {
			return Integer.parseInt(getString(R.string.pref_behaviour_gallery_swipe_length_key, "150", context, sharedPreferences));
		} catch(Throwable e) {
			return 150;
		}
	}

	public static Integer pref_behaviour_comment_min(final Context context, final SharedPreferences sharedPreferences) {
		Integer defaultValue = -4;

		final String value = getString(R.string.pref_behaviour_comment_min_key, defaultValue.toString(), context, sharedPreferences);

		if(value == null || value.trim().isEmpty()) {
			return null;
		}

		try {
			return Integer.parseInt(value);
		} catch(Throwable e) {
			return defaultValue;
		}
	}

	// pref_behaviour_imageview_mode

	public enum ImageViewMode {
		INTERNAL_OPENGL,
		INTERNAL_BROWSER,
		EXTERNAL_BROWSER
	}

	public static ImageViewMode pref_behaviour_imageview_mode(final Context context, final SharedPreferences sharedPreferences) {
		return ImageViewMode.valueOf(General.asciiUppercase(getString(R.string.pref_behaviour_imageview_mode_key, "internal_opengl", context, sharedPreferences)));
	}

	// pref_behaviour_albumview_mode

	public enum AlbumViewMode {
		INTERNAL_LIST,
		INTERNAL_BROWSER,
		EXTERNAL_BROWSER
	}

	public static AlbumViewMode pref_behaviour_albumview_mode(final Context context, final SharedPreferences sharedPreferences) {
		return AlbumViewMode.valueOf(General.asciiUppercase(getString(R.string.pref_behaviour_albumview_mode_key, "internal_list", context, sharedPreferences)));
	}

	// pref_behaviour_gifview_mode

	public enum GifViewMode {
		INTERNAL_MOVIE,
		INTERNAL_LEGACY,
		INTERNAL_BROWSER,
		EXTERNAL_BROWSER
	}

	public static GifViewMode pref_behaviour_gifview_mode(final Context context, final SharedPreferences sharedPreferences) {
		return GifViewMode.valueOf(General.asciiUppercase(getString(R.string.pref_behaviour_gifview_mode_key, "internal_movie", context, sharedPreferences)));
	}

	// pref_behaviour_videoview_mode

	public enum VideoViewMode {
		INTERNAL_VIDEOVIEW,
		INTERNAL_BROWSER,
		EXTERNAL_BROWSER,
		EXTERNAL_APP_VLC
	}

	public static VideoViewMode pref_behaviour_videoview_mode(final Context context, final SharedPreferences sharedPreferences) {
		return VideoViewMode.valueOf(General.asciiUppercase(getString(R.string.pref_behaviour_videoview_mode_key, "internal_videoview", context, sharedPreferences)));
	}

	// pref_behaviour_fling_post

	public enum PostFlingAction {
		UPVOTE, DOWNVOTE, SAVE, HIDE, COMMENTS, LINK, ACTION_MENU, BROWSER, DISABLED
	}

	public static PostFlingAction pref_behaviour_fling_post_left(final Context context, final SharedPreferences sharedPreferences) {
		return PostFlingAction.valueOf(General.asciiUppercase(getString(R.string.pref_behaviour_fling_post_left_key, "downvote", context, sharedPreferences)));
	}

	public static PostFlingAction pref_behaviour_fling_post_right(final Context context, final SharedPreferences sharedPreferences) {
		return PostFlingAction.valueOf(General.asciiUppercase(getString(R.string.pref_behaviour_fling_post_right_key, "upvote", context, sharedPreferences)));
	}

	public enum CommentAction {
		COLLAPSE, ACTION_MENU, NOTHING
	}

	public static CommentAction pref_behaviour_actions_comment_tap(final Context context, final SharedPreferences sharedPreferences) {
		return CommentAction.valueOf(General.asciiUppercase(getString(R.string.pref_behaviour_actions_comment_tap_key, "collapse", context, sharedPreferences)));
	}

	public static CommentAction pref_behaviour_actions_comment_longclick(final Context context, final SharedPreferences sharedPreferences) {
		return CommentAction.valueOf(General.asciiUppercase(getString(R.string.pref_behaviour_actions_comment_longclick_key, "action_menu", context, sharedPreferences)));
	}

	public static PostCommentListingURL.Sort pref_behaviour_commentsort(final Context context, final SharedPreferences sharedPreferences) {
		return PostCommentListingURL.Sort.valueOf(General.asciiUppercase(getString(R.string.pref_behaviour_commentsort_key, "best", context, sharedPreferences)));
	}

	public static boolean pref_behaviour_nsfw(final Context context, final SharedPreferences sharedPreferences) {
		return getBoolean(R.string.pref_behaviour_nsfw_key, false, context, sharedPreferences);
	}

	public enum PostCount {
		R25, R50, R100, ALL
	}

	public static PostCount pref_behaviour_post_count(final Context context, final SharedPreferences sharedPreferences) {
		return PostCount.valueOf(getString(R.string.pref_behaviour_postcount_key, "ALL", context, sharedPreferences));
	}

	public static enum ScreenOrientation {
		AUTO, PORTRAIT, LANDSCAPE
	}

	public static ScreenOrientation pref_behaviour_screen_orientation(final Context context, final SharedPreferences sharedPreferences) {
		return ScreenOrientation.valueOf(General.asciiUppercase(getString(R.string.pref_behaviour_screenorientation_key, ScreenOrientation.AUTO.name(), context, sharedPreferences)));
	}

	///////////////////////////////
	// pref_cache
	///////////////////////////////

	// pref_cache_maxage

	public static HashMap<Integer, Long> pref_cache_maxage(final Context context, final SharedPreferences sharedPreferences) {

		final HashMap<Integer, Long> result = new HashMap<>();

		final long maxAgeListing = 1000L * 60L * 60L * Long.valueOf(getString(R.string.pref_cache_maxage_listing_key, "168", context, sharedPreferences));
		final long maxAgeThumb = 1000L * 60L * 60L * Long.valueOf(getString(R.string.pref_cache_maxage_thumb_key, "168", context, sharedPreferences));
		final long maxAgeImage = 1000L * 60L * 60L * Long.valueOf(getString(R.string.pref_cache_maxage_image_key, "72", context, sharedPreferences));

		result.put(Constants.FileType.POST_LIST, maxAgeListing);
		result.put(Constants.FileType.COMMENT_LIST, maxAgeListing);
		result.put(Constants.FileType.SUBREDDIT_LIST, maxAgeListing);
		result.put(Constants.FileType.SUBREDDIT_ABOUT, maxAgeListing);
		result.put(Constants.FileType.USER_ABOUT, maxAgeListing);
		result.put(Constants.FileType.INBOX_LIST, maxAgeListing);
		result.put(Constants.FileType.THUMBNAIL, maxAgeThumb);
		result.put(Constants.FileType.IMAGE, maxAgeImage);
		result.put(Constants.FileType.IMAGE_INFO, maxAgeImage);
		result.put(Constants.FileType.CAPTCHA, maxAgeImage);

		return result;
	}

	// pref_cache_precache_images

	public static enum CachePrecacheImages {
		NEVER, WIFIONLY, ALWAYS
	}

	public static CachePrecacheImages cache_precache_images(final Context context, final SharedPreferences sharedPreferences) {

		if(network_tor(context, sharedPreferences)) {
			return CachePrecacheImages.NEVER;
		}

		if(!getBoolean(R.string.pref_cache_precache_images_key, true, context,  sharedPreferences)) {
			return CachePrecacheImages.NEVER;
		} else if(getBoolean(R.string.pref_cache_precache_images_wifionly_key, true, context, sharedPreferences)) {
			return CachePrecacheImages.WIFIONLY;
		} else {
			return CachePrecacheImages.ALWAYS;
		}
	}

	// pref_cache_precache_comments

	public static enum CachePrecacheComments {
		NEVER, WIFIONLY, ALWAYS
	}

	public static CachePrecacheComments cache_precache_comments(final Context context, final SharedPreferences sharedPreferences) {

		if(!getBoolean(R.string.pref_cache_precache_comments_key, true, context,  sharedPreferences)) {
			return CachePrecacheComments.NEVER;
		} else if(getBoolean(R.string.pref_cache_precache_comments_wifionly_key, false, context, sharedPreferences)) {
			return CachePrecacheComments.WIFIONLY;
		} else {
			return CachePrecacheComments.ALWAYS;
		}
	}

	///////////////////////////////
	// pref_network
	///////////////////////////////

	public static boolean network_tor(final Context context, final SharedPreferences sharedPreferences) {
		return getBoolean(R.string.pref_network_tor_key, false, context, sharedPreferences);
	}

	///////////////////////////////
	// pref_menus
	///////////////////////////////

	public static EnumSet<RedditPreparedPost.Action> pref_menus_post_context_items(final Context context, final SharedPreferences sharedPreferences) {

		final Set<String> strings = getStringSet(R.string.pref_menus_post_context_items_key, R.array.pref_menus_post_context_items_return, context, sharedPreferences);

		final EnumSet<RedditPreparedPost.Action> result = EnumSet.noneOf(RedditPreparedPost.Action.class);
		for(String s : strings) result.add(RedditPreparedPost.Action.valueOf(General.asciiUppercase(s)));

		return result;
	}

	public static EnumSet<RedditPreparedPost.Action> pref_menus_post_toolbar_items(final Context context, final SharedPreferences sharedPreferences) {

		final Set<String> strings = getStringSet(R.string.pref_menus_post_toolbar_items_key, R.array.pref_menus_post_toolbar_items_return, context, sharedPreferences);

		final EnumSet<RedditPreparedPost.Action> result = EnumSet.noneOf(RedditPreparedPost.Action.class);
		for(String s : strings) result.add(RedditPreparedPost.Action.valueOf(General.asciiUppercase(s)));

		return result;
	}

	public static EnumSet<MainMenuFragment.MainMenuUserItems> pref_menus_mainmenu_useritems(final Context context, final SharedPreferences sharedPreferences) {

		final Set<String> strings = getStringSet(R.string.pref_menus_mainmenu_useritems_key, R.array.pref_menus_mainmenu_useritems_items_default, context, sharedPreferences);

		final EnumSet<MainMenuFragment.MainMenuUserItems> result = EnumSet.noneOf(MainMenuFragment.MainMenuUserItems.class);
		for(String s : strings) result.add(MainMenuFragment.MainMenuUserItems.valueOf(General.asciiUppercase(s)));

		return result;
	}

	public static EnumSet<OptionsMenuUtility.OptionsMenuItemsPref> pref_menus_optionsmenu_items(final Context context, final SharedPreferences sharedPreferences) {

		final Set<String> strings = getStringSet(R.string.pref_menus_optionsmenu_items_key, R.array.pref_menus_optionsmenu_items_items_default, context, sharedPreferences);

		final EnumSet<OptionsMenuUtility.OptionsMenuItemsPref> result = EnumSet.noneOf(OptionsMenuUtility.OptionsMenuItemsPref.class);
		for(String s : strings) result.add(OptionsMenuUtility.OptionsMenuItemsPref.valueOf(General.asciiUppercase(s)));

		return result;
	}

	///////////////////////////////
	// pref_pinned_subreddits
	///////////////////////////////

	public static List<String> pref_pinned_subreddits(final Context context, final SharedPreferences sharedPreferences) {
		final String value = getString(R.string.pref_pinned_subreddits_key, "", context, sharedPreferences);
		return WritableHashSet.escapedStringToList(value);
	}

	public static void pref_pinned_subreddits_add(
			final Context context,
			final SharedPreferences sharedPreferences,
			final String subreddit) throws RedditSubreddit.InvalidSubredditNameException {

		pref_subreddits_add(context, sharedPreferences, subreddit, R.string.pref_pinned_subreddits_key);
	}

	public static void pref_pinned_subreddits_remove(
			final Context context,
			final SharedPreferences sharedPreferences,
			final String subreddit) throws RedditSubreddit.InvalidSubredditNameException {

		pref_subreddits_remove(context, sharedPreferences, subreddit, R.string.pref_pinned_subreddits_key);
	}

	public static boolean pref_pinned_subreddits_check(
			final Context context,
			final SharedPreferences sharedPreferences,
			final String subreddit) throws RedditSubreddit.InvalidSubredditNameException {

		final List<String> list = pref_pinned_subreddits(context, sharedPreferences);

		for(final String existingSr : list) {
			if(subreddit.toLowerCase().equals(existingSr.toLowerCase())) return true;
		}

		return false;
	}

	///////////////////////////////
	// pref_blocked_subreddits
	///////////////////////////////

	public static List<String> pref_blocked_subreddits(final Context context, final SharedPreferences sharedPreferences) {
		final String value = getString(R.string.pref_blocked_subreddits_key, "", context, sharedPreferences);
		return WritableHashSet.escapedStringToList(value);
	}

	public static void pref_blocked_subreddits_add(
			final Context context,
			final SharedPreferences sharedPreferences,
			final String subreddit) throws RedditSubreddit.InvalidSubredditNameException {

		pref_subreddits_add(context, sharedPreferences, subreddit, R.string.pref_blocked_subreddits_key);

		General.quickToast(context, R.string.block_done);
	}

	public static void pref_blocked_subreddits_remove(
			final Context context,
			final SharedPreferences sharedPreferences,
			final String subreddit) throws RedditSubreddit.InvalidSubredditNameException {

		pref_subreddits_remove(context, sharedPreferences, subreddit, R.string.pref_blocked_subreddits_key);

		General.quickToast(context, R.string.unblock_done);
	}

	public static boolean pref_blocked_subreddits_check(
			final Context context,
			final SharedPreferences sharedPreferences,
			final String subreddit) throws RedditSubreddit.InvalidSubredditNameException {

		final List<String> list = pref_blocked_subreddits(context, sharedPreferences);

		for(final String existingSr : list) {
			if (subreddit.toLowerCase().equals(existingSr.toLowerCase())) return true;
		}

		return false;
	}

	///////////////////////////////
	// Shared pref_subreddits methods
	///////////////////////////////

	private static void pref_subreddits_add(Context context, SharedPreferences sharedPreferences, String subreddit, int prefId) throws RedditSubreddit.InvalidSubredditNameException {
		final String name = RedditSubreddit.getCanonicalName(subreddit);

		final String value = getString(prefId, "", context, sharedPreferences);
		final ArrayList<String> list = WritableHashSet.escapedStringToList(value);
		list.add(name);

		final String result = WritableHashSet.listToEscapedString(list);

		sharedPreferences.edit().putString(context.getString(prefId), result).apply();
	}

	private static void pref_subreddits_remove(Context context, SharedPreferences sharedPreferences, String subreddit, int prefId) throws RedditSubreddit.InvalidSubredditNameException {
		final String name = RedditSubreddit.getCanonicalName(subreddit);

		final String value = getString(prefId, "", context, sharedPreferences);
		final ArrayList<String> list = WritableHashSet.escapedStringToList(value);
		list.add(name);

		final ArrayList<String> result = new ArrayList<>(list.size());
		for(final String existingSr : list) {
			if(!name.toLowerCase().equals(existingSr.toLowerCase())) {
				result.add(existingSr);
			}
		}

		final String resultStr = WritableHashSet.listToEscapedString(result);

		sharedPreferences.edit().putString(context.getString(prefId), resultStr).apply();
	}
}
