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

import android.content.Context;
import android.content.res.Resources;
import android.util.DisplayMetrics;
import org.holoeverywhere.app.Activity;
import org.holoeverywhere.preference.PreferenceManager;
import org.holoeverywhere.preference.SharedPreferences;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.listingcontrollers.CommentListingController;
import org.quantumbadger.redreader.reddit.prepared.RedditPreparedPost;

import java.util.*;

public final class PrefsUtility {

	private static <E> Set<E> setFromArray(E[] data) {
		final HashSet<E> result = new HashSet<E>(data.length);
		Collections.addAll(result, data);
		return result;
	}

	private static String getString(final int id, final String defaultValue, final Context context, final SharedPreferences sharedPreferences) {
		return sharedPreferences.getString(context.getString(id), defaultValue);
	}

	private static Set<String> getStringSet(final int id, final int defaultArrayRes, final Context context, final SharedPreferences sharedPreferences) {
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
				|| context.getString(R.string.pref_appearance_solidblack_key).equals(key);
	}

	public static boolean isRefreshRequired(final Context context, final String key) {
		return key.startsWith("pref_appearance")
				|| key.equals(context.getString(R.string.pref_behaviour_fling_post_left_key))
				|| key.equals(context.getString(R.string.pref_behaviour_fling_post_right_key))
				|| key.equals(context.getString(R.string.pref_behaviour_nsfw_key));
	}

	public static boolean isRestartRequired(Context context, String key) {
		return context.getString(R.string.pref_appearance_theme_key).equals(key)
				|| context.getString(R.string.pref_appearance_solidblack_key).equals(key)
				|| context.getString(R.string.pref_appearance_langforce_key).equals(key);
	}

	///////////////////////////////
	// pref_appearance
	///////////////////////////////

	// pref_appearance_twopane

	public static enum AppearanceTwopane {
		NEVER, AUTO, FORCE
	}

	public static AppearanceTwopane appearance_twopane(final Context context, final SharedPreferences sharedPreferences) {
		return AppearanceTwopane.valueOf(getString(R.string.pref_appearance_twopane_key, "auto", context, sharedPreferences).toUpperCase());
	}

	public static enum AppearanceTheme {
		RED, GREEN, BLUE, LTBLUE, ORANGE, GRAY, NIGHT
	}

	public static AppearanceTheme appearance_theme(final Context context, final SharedPreferences sharedPreferences) {
		return AppearanceTheme.valueOf(getString(R.string.pref_appearance_theme_key, "red", context, sharedPreferences).toUpperCase());
	}

	public static void applyTheme(Activity activity) {

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
				activity.setTheme(R.style.RR_Light_DarkActionBar);
				break;

			case NIGHT:
				activity.setTheme(R.style.RR_Dark);
				break;
		}

		final String lang = getString(R.string.pref_appearance_langforce_key, "auto", activity, prefs);

		if(!lang.equals("auto")) {
			final Resources res = activity.getResources();
			final DisplayMetrics dm = res.getDisplayMetrics();
			final android.content.res.Configuration conf = res.getConfiguration();
			conf.locale = new Locale(lang);
			res.updateConfiguration(conf, dm);
		}
	}

	public static boolean appearance_solidblack(final Context context, final SharedPreferences sharedPreferences) {
		return getBoolean(R.string.pref_appearance_solidblack_key, false, context, sharedPreferences);
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

	public static float appearance_fontscale_posts(final Context context, final SharedPreferences sharedPreferences) {
		return Float.valueOf(getString(R.string.pref_appearance_fontscale_posts_key, "1", context,  sharedPreferences));
	}

	public static boolean appearance_loading_detail(final Context context, final SharedPreferences sharedPreferences) {
		return getBoolean(R.string.pref_appearance_loading_detail_key, false, context, sharedPreferences);
	}

	public static enum AppearanceCommentHeaderItems {
		AUTHOR, FLAIR, SCORE, UPS_DOWNS, AGE
	}

	public static EnumSet<AppearanceCommentHeaderItems> appearance_comment_header_items(final Context context, final SharedPreferences sharedPreferences) {

		final Set<String> strings = getStringSet(R.string.pref_appearance_comment_header_items_key, R.array.pref_appearance_comment_header_items_default, context, sharedPreferences);

		final EnumSet<AppearanceCommentHeaderItems> result = EnumSet.noneOf(AppearanceCommentHeaderItems.class);
		for(String s : strings) result.add(AppearanceCommentHeaderItems.valueOf(s.toUpperCase()));

		return result;
	}

	///////////////////////////////
	// pref_behaviour
	///////////////////////////////

	// pref_behaviour_fling_post

	public static enum PostFlingAction {
		UPVOTE, DOWNVOTE, SAVE, HIDE, COMMENTS, LINK, ACTION_MENU, BROWSER, DISABLED
	}

	public static PostFlingAction pref_behaviour_fling_post_left(final Context context, final SharedPreferences sharedPreferences) {
		return PostFlingAction.valueOf(getString(R.string.pref_behaviour_fling_post_left_key, "downvote", context, sharedPreferences).toUpperCase());
	}

	public static PostFlingAction pref_behaviour_fling_post_right(final Context context, final SharedPreferences sharedPreferences) {
		return PostFlingAction.valueOf(getString(R.string.pref_behaviour_fling_post_right_key, "upvote", context, sharedPreferences).toUpperCase());
	}

	public static enum CommentAction {
		COLLAPSE, ACTION_MENU, NOTHING
	}

	public static CommentAction pref_behaviour_actions_comment_tap(final Context context, final SharedPreferences sharedPreferences) {
		return CommentAction.valueOf(getString(R.string.pref_behaviour_actions_comment_tap_key, "action_menu", context, sharedPreferences).toUpperCase());
	}

	public static CommentListingController.Sort pref_behaviour_commentsort(final Context context, final SharedPreferences sharedPreferences) {
		return CommentListingController.Sort.valueOf(getString(R.string.pref_behaviour_commentsort_key, "best", context, sharedPreferences).toUpperCase());
	}

	public static boolean pref_behaviour_nsfw(final Context context, final SharedPreferences sharedPreferences) {
		return getBoolean(R.string.pref_behaviour_nsfw_key, false, context, sharedPreferences);
	}

	///////////////////////////////
	// pref_cache
	///////////////////////////////

	// pref_cache_maxage

	public static HashMap<Integer, Long> pref_cache_maxage(final Context context, final SharedPreferences sharedPreferences) {

		final HashMap<Integer, Long> result = new HashMap<Integer, Long>();

		final long maxAgeListing = 1000L * 60L * 60L * Long.valueOf(getString(R.string.pref_cache_maxage_listing_key, "168", context, sharedPreferences));
		final long maxAgeThumb = 1000L * 60L * 60L * Long.valueOf(getString(R.string.pref_cache_maxage_thumb_key, "168", context, sharedPreferences));
		final long maxAgeImage = 1000L * 60L * 60L * Long.valueOf(getString(R.string.pref_cache_maxage_image_key, "72", context, sharedPreferences));

		result.put(Constants.FileType.POST_LIST, maxAgeListing);
		result.put(Constants.FileType.COMMENT_LIST, maxAgeListing);
		result.put(Constants.FileType.SUBREDDIT_LIST, maxAgeListing);
		result.put(Constants.FileType.USER_ABOUT, maxAgeListing);
		result.put(Constants.FileType.INBOX_LIST, maxAgeListing);
		result.put(Constants.FileType.THUMBNAIL, maxAgeThumb);
		result.put(Constants.FileType.IMAGE, maxAgeImage);

		return result;
	}

	// pref_cache_precache_images

	public static enum CachePrecacheImages {
		NEVER, WIFIONLY, ALWAYS
	}

	public static CachePrecacheImages cache_precache_images(final Context context, final SharedPreferences sharedPreferences) {

		if(!getBoolean(R.string.pref_cache_precache_images_key, true, context,  sharedPreferences)) {
			return CachePrecacheImages.NEVER;
		} else if(getBoolean(R.string.pref_cache_precache_images_wifionly_key, false, context, sharedPreferences)) {
			return CachePrecacheImages.WIFIONLY;
		} else {
			return CachePrecacheImages.ALWAYS;
		}
	}

	///////////////////////////////
	// pref_menus
	///////////////////////////////

	public static EnumSet<RedditPreparedPost.Action> pref_menus_post_context_items(final Context context, final SharedPreferences sharedPreferences) {

		final Set<String> strings = getStringSet(R.string.pref_menus_post_context_items_key, R.array.pref_menus_post_context_items_return, context, sharedPreferences);

		final EnumSet<RedditPreparedPost.Action> result = EnumSet.noneOf(RedditPreparedPost.Action.class);
		for(String s : strings) result.add(RedditPreparedPost.Action.valueOf(s.toUpperCase()));

		return result;
	}

	public static EnumSet<RedditPreparedPost.Action> pref_menus_post_toolbar_items(final Context context, final SharedPreferences sharedPreferences) {

		final Set<String> strings = getStringSet(R.string.pref_menus_post_toolbar_items_key, R.array.pref_menus_post_toolbar_items_return, context, sharedPreferences);

		final EnumSet<RedditPreparedPost.Action> result = EnumSet.noneOf(RedditPreparedPost.Action.class);
		for(String s : strings) result.add(RedditPreparedPost.Action.valueOf(s.toUpperCase()));

		return result;
	}
}
