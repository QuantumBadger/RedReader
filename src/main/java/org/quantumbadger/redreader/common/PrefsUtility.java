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
import android.util.DisplayMetrics;
import android.view.MenuItem;
import androidx.annotation.NonNull;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.activities.OptionsMenuUtility;
import org.quantumbadger.redreader.adapters.MainMenuListingManager;
import org.quantumbadger.redreader.fragments.MainMenuFragment;
import org.quantumbadger.redreader.io.WritableHashSet;
import org.quantumbadger.redreader.reddit.PostSort;
import org.quantumbadger.redreader.reddit.prepared.RedditPreparedPost;
import org.quantumbadger.redreader.reddit.things.InvalidSubredditNameException;
import org.quantumbadger.redreader.reddit.things.SubredditCanonicalId;
import org.quantumbadger.redreader.reddit.url.PostCommentListingURL;
import org.quantumbadger.redreader.reddit.url.UserCommentListingURL;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.EnumMap;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Set;

public final class PrefsUtility {

	private static <E> Set<E> setFromArray(final E[] data) {
		final HashSet<E> result = new HashSet<>(data.length);
		Collections.addAll(result, data);
		return result;
	}

	public static String getString(
			final int id,
			final String defaultValue,
			final Context context,
			final SharedPreferences sharedPreferences) {
		return sharedPreferences.getString(context.getString(id), defaultValue);
	}

	public static Set<String> getStringSet(
			final int id,
			final int defaultArrayRes,
			final Context context,
			final SharedPreferences sharedPreferences) {
		return sharedPreferences.getStringSet(
				context.getString(id),
				setFromArray(context.getResources().getStringArray(defaultArrayRes)));
	}

	private static boolean getBoolean(
			final int id,
			final boolean defaultValue,
			final Context context,
			final SharedPreferences sharedPreferences) {
		return sharedPreferences.getBoolean(context.getString(id), defaultValue);
	}

	private static long getLong(
			final int id,
			final long defaultValue,
			final Context context,
			final SharedPreferences sharedPreferences) {
		return sharedPreferences.getLong(context.getString(id), defaultValue);
	}

	public static boolean isReLayoutRequired(final Context context, final String key) {
		return context.getString(R.string.pref_appearance_twopane_key).equals(key)
				|| context.getString(R.string.pref_appearance_theme_key).equals(key)
				|| context.getString(R.string.pref_menus_mainmenu_useritems_key)
				.equals(key)
				|| context.getString(R.string.pref_menus_mainmenu_shortcutitems_key)
				.equals(key);
	}

	public static boolean isRefreshRequired(final Context context, final String key) {
		return key.startsWith("pref_appearance")
				|| key.equals(context.getString(R.string.pref_behaviour_fling_post_left_key))
				|| key.equals(context.getString(R.string.pref_behaviour_fling_post_right_key))
				|| key.equals(context.getString(R.string.pref_behaviour_nsfw_key))
				|| key.equals(context.getString(R.string.pref_behaviour_postcount_key))
				|| key.equals(context.getString(R.string.pref_behaviour_comment_min_key))
				|| key.equals(context.getString(R.string.pref_behaviour_pinned_subredditsort_key))
				|| key.equals(context.getString(
				R.string.pref_behaviour_blocked_subredditsort_key))
				|| key.equals(context.getString(
				R.string.pref_appearance_hide_headertoolbar_commentlist_key))
				|| key.equals(context.getString(
				R.string.pref_appearance_hide_headertoolbar_postlist_key));
	}

	public static boolean isRestartRequired(final Context context, final String key) {
		return context.getString(R.string.pref_appearance_theme_key).equals(key)
				|| context.getString(R.string.pref_appearance_navbar_color_key)
				.equals(key)
				|| context.getString(R.string.pref_appearance_langforce_key).equals(key)
				|| context.getString(R.string.pref_behaviour_bezel_toolbar_swipezone_key)
				.equals(key)
				|| context.getString(R.string.pref_appearance_hide_username_main_menu_key)
				.equals(key)
				|| context.getString(R.string.pref_appearance_hide_android_status_key)
				.equals(key)
				|| context.getString(R.string.pref_appearance_comments_show_floating_toolbar_key)
				.equals(key)
				|| context.getString(R.string.pref_behaviour_enable_swipe_refresh_key)
				.equals(key)
				|| context.getString(R.string.pref_menus_show_multireddit_main_menu_key)
				.equals(key)
				|| context.getString(R.string.pref_menus_show_subscribed_subreddits_main_menu_key)
				.equals(key)
				|| context.getString(R.string.pref_appearance_bottom_toolbar_key)
				.equals(key);
	}

	///////////////////////////////
	// pref_appearance
	///////////////////////////////

	// pref_appearance_twopane

	public enum AppearanceTwopane {
		NEVER, AUTO, FORCE
	}

	public static AppearanceTwopane appearance_twopane(
			final Context context,
			final SharedPreferences sharedPreferences) {
		return AppearanceTwopane.valueOf(StringUtils.asciiUppercase(getString(
				R.string.pref_appearance_twopane_key,
				"auto",
				context,
				sharedPreferences)));
	}

	public enum AppearanceTheme {
		RED, GREEN, BLUE, LTBLUE, ORANGE, GRAY, NIGHT, NIGHT_LOWCONTRAST, ULTRABLACK
	}

	public static boolean isNightMode(final Context context) {

		final AppearanceTheme theme = appearance_theme(
				context,
				PreferenceManager.getDefaultSharedPreferences(context));

		return theme == AppearanceTheme.NIGHT
				|| theme == AppearanceTheme.NIGHT_LOWCONTRAST
				|| theme == AppearanceTheme.ULTRABLACK;
	}

	public static AppearanceTheme appearance_theme(
			final Context context,
			final SharedPreferences sharedPreferences) {
		return AppearanceTheme.valueOf(StringUtils.asciiUppercase(getString(
				R.string.pref_appearance_theme_key,
				"red",
				context,
				sharedPreferences)));
	}

	public enum AppearanceNavbarColour {
		BLACK, PRIMARY, PRIMARYDARK
	}

	public static AppearanceNavbarColour appearance_navbar_colour(
			final Context context,
			final SharedPreferences sharedPreferences) {
		return AppearanceNavbarColour.valueOf(StringUtils.asciiUppercase(getString(
				R.string.pref_appearance_navbar_color_key,
				"black",
				context,
				sharedPreferences)));
	}

	public static void applyTheme(@NonNull final Activity activity) {

		final SharedPreferences prefs = PreferenceManager.getDefaultSharedPreferences(
				activity);

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

			case NIGHT_LOWCONTRAST:
				activity.setTheme(R.style.RR_Dark_LowContrast);
				break;

			case ULTRABLACK:
				activity.setTheme(R.style.RR_Dark_UltraBlack);
				break;
		}

		applyLanguage(activity, prefs);
	}

	public static void applySettingsTheme(@NonNull final Activity activity) {

		final SharedPreferences prefs = PreferenceManager.getDefaultSharedPreferences(
				activity);
		activity.setTheme(R.style.RR_Settings);
		applyLanguage(activity, prefs);
	}

	private static void applyLanguage(
			final Activity activity,
			final SharedPreferences prefs) {

		final String lang = getString(
				R.string.pref_appearance_langforce_key,
				"auto",
				activity,
				prefs);

		for(final Resources res : new Resources[] {
				activity.getResources(),
				activity.getApplication().getResources()
		}) {

			final DisplayMetrics dm = res.getDisplayMetrics();
			final android.content.res.Configuration conf = res.getConfiguration();

			if(!lang.equals("auto")) {

				if(lang.contains("-r")) {
					final String[] split = lang.split("-r");
					conf.locale = new Locale(split[0], split[1]);

				} else {
					conf.locale = new Locale(lang);
				}

			} else {
				conf.locale = Locale.getDefault();
			}

			res.updateConfiguration(conf, dm);
		}
	}

	public enum AppearanceThumbnailsShow {
		NEVER, WIFIONLY, ALWAYS
	}

	public static AppearanceThumbnailsShow appearance_thumbnails_show(
			final Context context,
			final SharedPreferences sharedPreferences) {
		return AppearanceThumbnailsShow.valueOf(StringUtils.asciiUppercase(
				getString(
						R.string.pref_appearance_thumbnails_show_list_key,
						"always",
						context,
						sharedPreferences)));
	}

	public static AppearanceThumbnailsShow appearance_thumbnails_show_old(
			final Context context,
			final SharedPreferences sharedPreferences) {

		if(!getBoolean(
				R.string.pref_appearance_thumbnails_show_key,
				true,
				context,
				sharedPreferences)) {
			return AppearanceThumbnailsShow.NEVER;
		} else if(getBoolean(
				R.string.pref_appearance_thumbnails_wifionly_key,
				false,
				context,
				sharedPreferences)) {
			return AppearanceThumbnailsShow.WIFIONLY;
		} else {
			return AppearanceThumbnailsShow.ALWAYS;
		}
	}

	public static boolean appearance_thumbnails_nsfw_show(
			final Context context,
			final SharedPreferences sharedPreferences) {
		return getBoolean(
				R.string.pref_appearance_thumbnails_nsfw_show_key,
				false,
				context,
				sharedPreferences);
	}

	public static float appearance_fontscale_global(
			final Context context,
			final SharedPreferences sharedPreferences) {
		return Float.valueOf(getString(
				R.string.pref_appearance_fontscale_global_key,
				"1",
				context,
				sharedPreferences));
	}

	public static float appearance_fontscale_bodytext(
			final Context context,
			final SharedPreferences sharedPreferences) {
		if(getString(
				R.string.pref_appearance_fontscale_bodytext_key,
				"-1",
				context,
				sharedPreferences).equals("-1")) {
			return appearance_fontscale_global(context, sharedPreferences);
		}
		return Float.valueOf(getString(
				R.string.pref_appearance_fontscale_bodytext_key,
				"-1",
				context,
				sharedPreferences));
	}

	public static float appearance_fontscale_comment_headers(
			final Context context,
			final SharedPreferences sharedPreferences) {
		if(getString(
				R.string.pref_appearance_fontscale_comment_headers_key,
				"-1",
				context,
				sharedPreferences).equals("-1")) {
			return appearance_fontscale_global(context, sharedPreferences);
		}
		return Float.valueOf(getString(
				R.string.pref_appearance_fontscale_comment_headers_key,
				"-1",
				context,
				sharedPreferences));
	}

	public static float appearance_fontscale_linkbuttons(
			final Context context,
			final SharedPreferences sharedPreferences) {
		if(getString(
				R.string.pref_appearance_fontscale_linkbuttons_key,
				"-1",
				context,
				sharedPreferences).equals("-1")) {
			return appearance_fontscale_global(context, sharedPreferences);
		}
		return Float.valueOf(getString(
				R.string.pref_appearance_fontscale_linkbuttons_key,
				"-1",
				context,
				sharedPreferences));
	}

	public static float appearance_fontscale_posts(
			final Context context,
			final SharedPreferences sharedPreferences) {
		if(getString(
				R.string.pref_appearance_fontscale_posts_key,
				"-1",
				context,
				sharedPreferences).equals("-1")) {
			return appearance_fontscale_global(context, sharedPreferences);
		}
		return Float.valueOf(getString(
				R.string.pref_appearance_fontscale_posts_key,
				"-1",
				context,
				sharedPreferences));
	}

	public static float appearance_fontscale_post_subtitles(
			final Context context,
			final SharedPreferences sharedPreferences) {
		if(getString(
				R.string.pref_appearance_fontscale_post_subtitles_key,
				"-1",
				context,
				sharedPreferences).equals("-1")) {
			return appearance_fontscale_global(context, sharedPreferences);
		}
		return Float.valueOf(getString(
				R.string.pref_appearance_fontscale_post_subtitles_key,
				"-1",
				context,
				sharedPreferences));
	}

	public static float appearance_fontscale_post_header_titles(
			final Context context,
			final SharedPreferences sharedPreferences) {
		if(getString(
				R.string.pref_appearance_fontscale_post_header_titles_key,
				"-1",
				context,
				sharedPreferences).equals("-1")) {
			return appearance_fontscale_global(context, sharedPreferences);
		}
		return Float.valueOf(getString(
				R.string.pref_appearance_fontscale_post_header_titles_key,
				"-1",
				context,
				sharedPreferences));
	}

	public static float appearance_fontscale_post_header_subtitles(
			final Context context,
			final SharedPreferences sharedPreferences) {
		if(getString(
				R.string.pref_appearance_fontscale_post_header_subtitles_key,
				"-1",
				context,
				sharedPreferences).equals("-1")) {
			return appearance_fontscale_global(context, sharedPreferences);
		}
		return Float.valueOf(getString(
				R.string.pref_appearance_fontscale_post_header_subtitles_key,
				"-1",
				context,
				sharedPreferences));
	}

	public static boolean pref_appearance_hide_username_main_menu(
			final Context context,
			final SharedPreferences sharedPreferences) {
		return getBoolean(
				R.string.pref_appearance_hide_username_main_menu_key,
				false,
				context,
				sharedPreferences);
	}

	public static boolean pref_show_popular_main_menu(
			final Context context,
			final SharedPreferences sharedPreferences) {
		return getBoolean(
				R.string.pref_menus_show_popular_main_menu_key,
				false,
				context,
				sharedPreferences);
	}

	public static boolean pref_show_random_main_menu(
			final Context context,
			final SharedPreferences sharedPreferences) {
		return getBoolean(
				R.string.pref_menus_show_random_main_menu_key,
				false,
				context,
				sharedPreferences);
	}

	public static boolean pref_show_multireddit_main_menu(
			final Context context,
			final SharedPreferences sharedPreferences) {
		return getBoolean(
				R.string.pref_menus_show_multireddit_main_menu_key,
				true,
				context,
				sharedPreferences);
	}

	public static boolean pref_show_subscribed_subreddits_main_menu(
			final Context context,
			final SharedPreferences sharedPreferences) {
		return getBoolean(
				R.string.pref_menus_show_subscribed_subreddits_main_menu_key,
				true,
				context,
				sharedPreferences);
	}

	public static boolean pref_appearance_show_blocked_subreddits_main_menu(
			final Context context,
			final SharedPreferences sharedPreferences) {
		return getBoolean(
				R.string.pref_appearance_show_blocked_subreddits_main_menu_key,
				false,
				context,
				sharedPreferences);
	}

	public static boolean pref_appearance_linkbuttons(
			final Context context,
			final SharedPreferences sharedPreferences) {
		return getBoolean(
				R.string.pref_appearance_linkbuttons_key,
				true,
				context,
				sharedPreferences);
	}

	public static boolean pref_appearance_hide_android_status(
			final Context context,
			final SharedPreferences sharedPreferences) {
		return getBoolean(
				R.string.pref_appearance_hide_android_status_key,
				false,
				context,
				sharedPreferences);
	}

	public static boolean pref_appearance_link_text_clickable(
			final Context context,
			final SharedPreferences sharedPreferences) {
		return getBoolean(
				R.string.pref_appearance_link_text_clickable_key,
				true,
				context,
				sharedPreferences);
	}

	public static boolean pref_appearance_image_viewer_show_floating_toolbar(
			final Context context,
			final SharedPreferences sharedPreferences) {
		return getBoolean(
				R.string.pref_appearance_image_viewer_show_floating_toolbar_key,
				true,
				context,
				sharedPreferences);
	}

	public static boolean pref_appearance_show_aspect_ratio_indicator(
			final Context context,
			final SharedPreferences sharedPreferences) {
		return getBoolean(
				R.string.pref_appearance_show_aspect_ratio_indicator_key,
				false,
				context,
				sharedPreferences);
	}

	public static boolean pref_appearance_comments_show_floating_toolbar(
			final Context context,
			final SharedPreferences sharedPreferences) {
		return getBoolean(
				R.string.pref_appearance_comments_show_floating_toolbar_key,
				true,
				context,
				sharedPreferences);
	}

	public static boolean pref_appearance_indentlines(
			final Context context,
			final SharedPreferences sharedPreferences) {
		return getBoolean(
				R.string.pref_appearance_indentlines_key,
				false,
				context,
				sharedPreferences);
	}

	public static boolean pref_appearance_left_handed(
			final Context context,
			final SharedPreferences sharedPreferences) {
		return getBoolean(
				R.string.pref_appearance_left_handed_key,
				false,
				context,
				sharedPreferences);
	}

	public static boolean pref_appearance_bottom_toolbar(
			final Context context,
			final SharedPreferences sharedPreferences) {
		return getBoolean(
				R.string.pref_appearance_bottom_toolbar_key,
				false,
				context,
				sharedPreferences);
	}

	public static boolean pref_appearance_hide_headertoolbar_postlist(
			final Context context,
			final SharedPreferences sharedPreferences) {
		return getBoolean(
				R.string.pref_appearance_hide_headertoolbar_postlist_key,
				false,
				context,
				sharedPreferences);
	}

	public static boolean pref_appearance_hide_headertoolbar_commentlist(
			final Context context,
			final SharedPreferences sharedPreferences) {
		return getBoolean(
				R.string.pref_appearance_hide_headertoolbar_commentlist_key,
				false,
				context,
				sharedPreferences);
	}

	public enum AppearancePostSubtitleItem {
		AUTHOR, FLAIR, SCORE, AGE, GOLD, SUBREDDIT, DOMAIN, STICKY, SPOILER, NSFW
	}

	public static EnumSet<AppearancePostSubtitleItem> appearance_post_subtitle_items(
			final Context context,
			final SharedPreferences sharedPreferences) {

		final Set<String> strings = getStringSet(
				R.string.pref_appearance_post_subtitle_items_key,
				R.array.pref_appearance_post_subtitle_items_default,
				context,
				sharedPreferences);

		final EnumSet<AppearancePostSubtitleItem> result = EnumSet.noneOf(
				AppearancePostSubtitleItem.class);
		for(final String s : strings) {
			result.add(AppearancePostSubtitleItem.valueOf(StringUtils.asciiUppercase(s)));
		}

		return result;
	}

	public static int appearance_post_age_units(
			final Context context,
			final SharedPreferences sharedPreferences) {
		try {
			return Integer.parseInt(getString(
					R.string.pref_appearance_post_age_units_key,
					"2",
					context,
					sharedPreferences));
		} catch(final Throwable e) {
			return 2;
		}
	}

	public static boolean appearance_post_subtitle_items_use_different_settings(
			final Context context,
			final SharedPreferences sharedPreferences) {
		return getBoolean(
				R.string.pref_appearance_post_subtitle_items_use_different_settings_key,
				false,
				context,
				sharedPreferences);
	}

	public static EnumSet<AppearancePostSubtitleItem> appearance_post_header_subtitle_items(
			final Context context,
			final SharedPreferences sharedPreferences) {

		final Set<String> strings = getStringSet(
				R.string.pref_appearance_post_header_subtitle_items_key,
				R.array.pref_appearance_post_subtitle_items_default,
				context,
				sharedPreferences);

		final EnumSet<AppearancePostSubtitleItem> result = EnumSet.noneOf(
				AppearancePostSubtitleItem.class);
		for(final String s : strings) {
			result.add(AppearancePostSubtitleItem.valueOf(StringUtils.asciiUppercase(s)));
		}

		return result;
	}

	public static int appearance_post_header_age_units(
			final Context context,
			final SharedPreferences sharedPreferences) {
		try {
			return Integer.parseInt(getString(
					R.string.pref_appearance_post_header_age_units_key,
					"2",
					context,
					sharedPreferences));
		} catch(final Throwable e) {
			return 2;
		}
	}

	public static boolean appearance_post_show_comments_button(
			final Context context,
			final SharedPreferences sharedPreferences) {
		return getBoolean(
				R.string.pref_appearance_post_show_comments_button_key,
				true,
				context,
				sharedPreferences);
	}

	public enum AppearanceCommentHeaderItem {
		AUTHOR, FLAIR, SCORE, AGE, GOLD
	}

	public static EnumSet<AppearanceCommentHeaderItem> appearance_comment_header_items(
			final Context context,
			final SharedPreferences sharedPreferences) {

		final Set<String> strings = getStringSet(
				R.string.pref_appearance_comment_header_items_key,
				R.array.pref_appearance_comment_header_items_default,
				context,
				sharedPreferences);

		final EnumSet<AppearanceCommentHeaderItem> result = EnumSet.noneOf(
				AppearanceCommentHeaderItem.class);
		for(final String s : strings) {

			if(s.equalsIgnoreCase("ups_downs")) {
				continue;
			}

			try {
				result.add(AppearanceCommentHeaderItem.valueOf(StringUtils.asciiUppercase(s)));
			} catch(final IllegalArgumentException e) {
				// Ignore -- this option no longer exists
			}
		}

		return result;
	}

	public static int appearance_comment_age_units(
			final Context context,
			final SharedPreferences sharedPreferences) {
		try {
			return Integer.parseInt(getString(
					R.string.pref_appearance_comment_age_units_key,
					"2",
					context,
					sharedPreferences));
		} catch(final Throwable e) {
			return 2;
		}
	}

	public enum CommentAgeMode {
		ABSOLUTE, RELATIVE_POST, RELATIVE_PARENT
	}

	public static CommentAgeMode appearance_comment_age_mode(
			final Context context,
			final SharedPreferences sharedPreferences) {
		return CommentAgeMode.valueOf(StringUtils.asciiUppercase(getString(
				R.string.pref_appearance_comment_age_mode_key,
				"absolute",
				context,
				sharedPreferences)));
	}

	///////////////////////////////
	// pref_behaviour
	///////////////////////////////

	public static boolean pref_behaviour_skiptofrontpage(
			final Context context,
			final SharedPreferences sharedPreferences) {
		return getBoolean(
				R.string.pref_behaviour_skiptofrontpage_key,
				false,
				context,
				sharedPreferences);
	}

	public static boolean pref_behaviour_useinternalbrowser(
			final Context context,
			final SharedPreferences sharedPreferences) {
		return getBoolean(
				R.string.pref_behaviour_useinternalbrowser_key,
				true,
				context,
				sharedPreferences);
	}

	public static boolean pref_behaviour_usecustomtabs(
			final Context context,
			final SharedPreferences sharedPreferences) {
		return getBoolean(
				R.string.pref_behaviour_usecustomtabs_key,
				false,
				context,
				sharedPreferences);
	}

	public static boolean pref_behaviour_notifications(
			final Context context,
			final SharedPreferences sharedPreferences) {
		return getBoolean(
				R.string.pref_behaviour_notifications_key,
				true,
				context,
				sharedPreferences);
	}

	public static boolean pref_behaviour_enable_swipe_refresh(
			final Context context,
			final SharedPreferences sharedPreferences) {
		return getBoolean(
				R.string.pref_behaviour_enable_swipe_refresh_key,
				true,
				context,
				sharedPreferences);
	}

	public static boolean pref_behaviour_video_playback_controls(
			final Context context,
			final SharedPreferences sharedPreferences) {
		return getBoolean(
				R.string.pref_behaviour_video_playback_controls_key,
				false,
				context,
				sharedPreferences);
	}

	public static boolean pref_behaviour_video_mute_default(
			final Context context,
			final SharedPreferences sharedPreferences) {
		return getBoolean(
				R.string.pref_behaviour_video_mute_default_key,
				true,
				context,
				sharedPreferences);
	}

	public static boolean pref_behaviour_video_zoom_default(
			final Context context,
			final SharedPreferences sharedPreferences) {
		return getBoolean(R.string.pref_behaviour_video_zoom_default_key,
				false,
				context,
				sharedPreferences);
	}

	public static int pref_behaviour_bezel_toolbar_swipezone_dp(
			final Context context,
			final SharedPreferences sharedPreferences) {
		try {
			return Integer.parseInt(getString(
					R.string.pref_behaviour_bezel_toolbar_swipezone_key,
					"10",
					context,
					sharedPreferences));
		} catch(final Throwable e) {
			return 10;
		}
	}

	public static int pref_behaviour_gallery_swipe_length_dp(
			final Context context,
			final SharedPreferences sharedPreferences) {
		try {
			return Integer.parseInt(getString(
					R.string.pref_behaviour_gallery_swipe_length_key,
					"150",
					context,
					sharedPreferences));
		} catch(final Throwable e) {
			return 150;
		}
	}

	public static Integer pref_behaviour_comment_min(
			final Context context,
			final SharedPreferences sharedPreferences) {
		final Integer defaultValue = -4;

		final String value = getString(
				R.string.pref_behaviour_comment_min_key,
				defaultValue.toString(),
				context,
				sharedPreferences);

		if(value == null || value.trim().isEmpty()) {
			return null;
		}

		try {
			return Integer.parseInt(value);
		} catch(final Throwable e) {
			return defaultValue;
		}
	}

	// pref_behaviour_imageview_mode

	public enum ImageViewMode {
		INTERNAL_OPENGL(true),
		INTERNAL_BROWSER(false),
		EXTERNAL_BROWSER(false);

		public final boolean downloadInApp;

		ImageViewMode(final boolean downloadInApp) {
			this.downloadInApp = downloadInApp;
		}
	}

	public static ImageViewMode pref_behaviour_imageview_mode(
			final Context context,
			final SharedPreferences sharedPreferences) {
		return ImageViewMode.valueOf(StringUtils.asciiUppercase(getString(
				R.string.pref_behaviour_imageview_mode_key,
				"internal_opengl",
				context,
				sharedPreferences)));
	}

	// pref_behaviour_albumview_mode

	public enum AlbumViewMode {
		INTERNAL_LIST,
		INTERNAL_BROWSER,
		EXTERNAL_BROWSER
	}

	public static AlbumViewMode pref_behaviour_albumview_mode(
			final Context context,
			final SharedPreferences sharedPreferences) {
		return AlbumViewMode.valueOf(StringUtils.asciiUppercase(getString(
				R.string.pref_behaviour_albumview_mode_key,
				"internal_list",
				context,
				sharedPreferences)));
	}

	// pref_behaviour_gifview_mode

	public enum GifViewMode {
		INTERNAL_MOVIE(true),
		INTERNAL_LEGACY(true),
		INTERNAL_BROWSER(false),
		EXTERNAL_BROWSER(false);

		public final boolean downloadInApp;

		GifViewMode(final boolean downloadInApp) {
			this.downloadInApp = downloadInApp;
		}
	}

	public static GifViewMode pref_behaviour_gifview_mode(
			final Context context,
			final SharedPreferences sharedPreferences) {
		return GifViewMode.valueOf(StringUtils.asciiUppercase(getString(
				R.string.pref_behaviour_gifview_mode_key,
				"internal_movie",
				context,
				sharedPreferences)));
	}

	// pref_behaviour_videoview_mode

	public enum VideoViewMode {
		INTERNAL_VIDEOVIEW(true),
		INTERNAL_BROWSER(false),
		EXTERNAL_BROWSER(false),
		EXTERNAL_APP_VLC(true);

		public final boolean downloadInApp;

		VideoViewMode(final boolean downloadInApp) {
			this.downloadInApp = downloadInApp;
		}
	}

	public static VideoViewMode pref_behaviour_videoview_mode(
			final Context context,
			final SharedPreferences sharedPreferences) {
		return VideoViewMode.valueOf(StringUtils.asciiUppercase(getString(
				R.string.pref_behaviour_videoview_mode_key,
				"internal_videoview",
				context,
				sharedPreferences)));
	}

	// pref_behaviour_fling_post

	public enum PostFlingAction {
		UPVOTE, DOWNVOTE, SAVE, HIDE, COMMENTS, LINK, ACTION_MENU, BROWSER, BACK, DISABLED
	}

	public static PostFlingAction pref_behaviour_fling_post_left(
			final Context context,
			final SharedPreferences sharedPreferences) {
		return PostFlingAction.valueOf(StringUtils.asciiUppercase(getString(
				R.string.pref_behaviour_fling_post_left_key,
				"downvote",
				context,
				sharedPreferences)));
	}

	public static PostFlingAction pref_behaviour_fling_post_right(
			final Context context,
			final SharedPreferences sharedPreferences) {
		return PostFlingAction.valueOf(StringUtils.asciiUppercase(getString(
				R.string.pref_behaviour_fling_post_right_key,
				"upvote",
				context,
				sharedPreferences)));
	}

	public enum SelfpostAction {
		COLLAPSE, NOTHING
	}

	public static SelfpostAction pref_behaviour_self_post_tap_actions(
			final Context context,
			final SharedPreferences sharedPreferences) {
		return SelfpostAction.valueOf(StringUtils.asciiUppercase(getString(
				R.string.pref_behaviour_self_post_tap_actions_key,
				"collapse",
				context,
				sharedPreferences)));
	}

	// pref_behaviour_fling_comment

	public enum CommentFlingAction {
		UPVOTE,
		DOWNVOTE,
		SAVE,
		REPLY,
		USER_PROFILE,
		COLLAPSE,
		ACTION_MENU,
		PROPERTIES,
		BACK,
		DISABLED
	}

	public static CommentFlingAction pref_behaviour_fling_comment_left(
			final Context context,
			final SharedPreferences sharedPreferences) {
		return CommentFlingAction.valueOf(StringUtils.asciiUppercase(getString(
				R.string.pref_behaviour_fling_comment_left_key,
				"downvote",
				context,
				sharedPreferences)));
	}

	public static CommentFlingAction pref_behaviour_fling_comment_right(
			final Context context,
			final SharedPreferences sharedPreferences) {
		return CommentFlingAction.valueOf(StringUtils.asciiUppercase(getString(
				R.string.pref_behaviour_fling_comment_right_key,
				"upvote",
				context,
				sharedPreferences)));
	}

	public enum CommentAction {
		COLLAPSE, ACTION_MENU, NOTHING
	}

	public static CommentAction pref_behaviour_actions_comment_tap(
			final Context context,
			final SharedPreferences sharedPreferences) {
		return CommentAction.valueOf(StringUtils.asciiUppercase(getString(
				R.string.pref_behaviour_actions_comment_tap_key,
				"collapse",
				context,
				sharedPreferences)));
	}

	public static CommentAction pref_behaviour_actions_comment_longclick(
			final Context context,
			final SharedPreferences sharedPreferences) {
		return CommentAction.valueOf(StringUtils.asciiUppercase(getString(
				R.string.pref_behaviour_actions_comment_longclick_key,
				"action_menu",
				context,
				sharedPreferences)));
	}

	public static boolean pref_behaviour_sharing_share_text(
			final Context context,
			final SharedPreferences sharedPreferences) {
		return getBoolean(
				R.string.pref_behaviour_sharing_share_text_key,
				true,
				context,
				sharedPreferences);
	}

	public static boolean pref_behaviour_sharing_include_desc(
			final Context context,
			final SharedPreferences sharedPreferences) {
		return getBoolean(
				R.string.pref_behaviour_sharing_include_desc_key,
				true,
				context,
				sharedPreferences);
	}

	public static boolean pref_behaviour_sharing_dialog(
			final Context context,
			final SharedPreferences sharedPreferences) {
		return getBoolean(
				R.string.pref_behaviour_sharing_share_dialog_key,
				false,
				context,
				sharedPreferences);
	}

	public static String pref_behaviour_sharing_dialog_data_get(
			final Context context,
			final SharedPreferences sharedPreferences) {
		return getString(
				R.string.pref_behaviour_sharing_share_dialog_data,
				"",
				context,
				sharedPreferences);
	}

	public static void pref_behaviour_sharing_dialog_data_set(
			final Context context,
			final SharedPreferences sharedPreferences,
			final String appNames) {
		sharedPreferences.edit()
				.putString(
						context.getString(R.string.pref_behaviour_sharing_share_dialog_data),
						appNames)
				.apply();
	}

	public static PostSort pref_behaviour_postsort(
			final Context context,
			final SharedPreferences sharedPreferences) {
		return PostSort.valueOf(StringUtils.asciiUppercase(getString(
				R.string.pref_behaviour_postsort_key,
				"hot",
				context,
				sharedPreferences)));
	}

	public static PostSort pref_behaviour_user_postsort(
			final Context context,
			final SharedPreferences sharedPreferences) {
		return PostSort.valueOf(StringUtils.asciiUppercase(getString(
				R.string.pref_behaviour_user_postsort_key,
				"new",
				context,
				sharedPreferences)));
	}

	public static PostSort pref_behaviour_multi_postsort(
			final Context context,
			final SharedPreferences sharedPreferences) {
		return PostSort.valueOf(StringUtils.asciiUppercase(getString(
				R.string.pref_behaviour_multi_postsort_key,
				"hot",
				context,
				sharedPreferences)));
	}

	public static PostCommentListingURL.Sort pref_behaviour_commentsort(
			final Context context,
			final SharedPreferences sharedPreferences) {
		return PostCommentListingURL.Sort.valueOf(StringUtils.asciiUppercase(getString(
				R.string.pref_behaviour_commentsort_key,
				"best",
				context,
				sharedPreferences)));
	}

	public static UserCommentListingURL.Sort pref_behaviour_user_commentsort(
			final Context context,
			final SharedPreferences sharedPreferences) {
		return UserCommentListingURL.Sort.valueOf(StringUtils.asciiUppercase(getString(
				R.string.pref_behaviour_user_commentsort_key,
				"new",
				context,
				sharedPreferences)));
	}

	public enum PinnedSubredditSort {
		NAME, DATE
	}

	public static PinnedSubredditSort pref_behaviour_pinned_subredditsort(
			final Context context,
			final SharedPreferences sharedPreferences) {
		return PinnedSubredditSort.valueOf(StringUtils.asciiUppercase(getString(
				R.string.pref_behaviour_pinned_subredditsort_key,
				"name",
				context,
				sharedPreferences)));
	}

	public enum BlockedSubredditSort {
		NAME, DATE
	}

	public static BlockedSubredditSort pref_behaviour_blocked_subredditsort(
			final Context context,
			final SharedPreferences sharedPreferences) {
		return BlockedSubredditSort.valueOf(StringUtils.asciiUppercase(getString(
				R.string.pref_behaviour_blocked_subredditsort_key,
				"name",
				context,
				sharedPreferences)));
	}

	public static boolean pref_behaviour_nsfw(
			final Context context,
			final SharedPreferences sharedPreferences) {
		return getBoolean(
				R.string.pref_behaviour_nsfw_key,
				false,
				context,
				sharedPreferences);
	}

	//Show Visited Posts? True hides them.
	// See strings.xml, prefs_behaviour.xml, PostListingFragment.java
	public static boolean pref_behaviour_hide_read_posts(
			final Context context,
			final SharedPreferences sharedPreferences) {
		return getBoolean(
				R.string.pref_behaviour_hide_read_posts_key,
				false,
				context,
				sharedPreferences);
	}

	public static boolean pref_behaviour_share_permalink(
			final Context context,
			final SharedPreferences sharedPreferences) {
		return getBoolean(
				R.string.pref_behaviour_share_permalink_key,
				false,
				context,
				sharedPreferences);
	}

	public enum PostCount {
		R25, R50, R100, ALL
	}

	public static PostCount pref_behaviour_post_count(
			final Context context,
			final SharedPreferences sharedPreferences) {
		return PostCount.valueOf(getString(
				R.string.pref_behaviour_postcount_key,
				"ALL",
				context,
				sharedPreferences));
	}

	public enum ScreenOrientation {
		AUTO, PORTRAIT, LANDSCAPE
	}

	public static ScreenOrientation pref_behaviour_screen_orientation(
			final Context context,
			final SharedPreferences sharedPreferences) {
		return ScreenOrientation.valueOf(StringUtils.asciiUppercase(getString(
				R.string.pref_behaviour_screenorientation_key,
				ScreenOrientation.AUTO.name(),
				context,
				sharedPreferences)));
	}

	///////////////////////////////
	// pref_cache
	///////////////////////////////

	// pref_cache_location

	public static String pref_cache_location(
			final Context context,
			final SharedPreferences sharedPreferences) {
		File defaultCacheDir = context.getExternalCacheDir();
		if(defaultCacheDir == null) {
			defaultCacheDir = context.getCacheDir();
		}
		return getString(R.string.pref_cache_location_key,
				defaultCacheDir.getAbsolutePath(),
				context, sharedPreferences);
	}

	public static void pref_cache_location(
			final Context context,
			final SharedPreferences sharedPreferences, final String path) {
		sharedPreferences.edit()
				.putString(context.getString(R.string.pref_cache_location_key), path)
				.apply();
	}

	public static long pref_cache_rerequest_postlist_age_ms(
			final Context context,
			final SharedPreferences sharedPreferences) {
		try {
			final int hours = Integer.parseInt(
					getString(
							R.string.pref_cache_rerequest_postlist_age_key,
							"1",
							context,
							sharedPreferences));

			return General.hoursToMs(hours);

		} catch(final Throwable e) {
			return 1;
		}
	}

	// pref_cache_maxage

	public static HashMap<Integer, Long> pref_cache_maxage(
			final Context context,
			final SharedPreferences sharedPreferences) {

		final HashMap<Integer, Long> result = new HashMap<>();

		final long maxAgeListing = 1000L
				* 60L
				* 60L
				* Long.valueOf(getString(
				R.string.pref_cache_maxage_listing_key,
				"168",
				context,
				sharedPreferences));
		final long maxAgeThumb = 1000L
				* 60L
				* 60L
				* Long.valueOf(getString(
				R.string.pref_cache_maxage_thumb_key,
				"168",
				context,
				sharedPreferences));
		final long maxAgeImage = 1000L
				* 60L
				* 60L
				* Long.valueOf(getString(
				R.string.pref_cache_maxage_image_key,
				"72",
				context,
				sharedPreferences));

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

	public static Long pref_cache_maxage_entry(
			final Context context,
			final SharedPreferences sharedPreferences) {
		return 1000L
				* 60L
				* 60L
				* Long.valueOf(getString(
				R.string.pref_cache_maxage_entry_key,
				"168",
				context,
				sharedPreferences));
	}

	// pref_cache_precache_images

	public enum CachePrecacheImages {
		NEVER, WIFIONLY, ALWAYS
	}

	public static CachePrecacheImages cache_precache_images(
			final Context context,
			final SharedPreferences sharedPreferences) {
		return CachePrecacheImages.valueOf(StringUtils.asciiUppercase(
				getString(
						R.string.pref_cache_precache_images_list_key,
						"wifionly",
						context,
						sharedPreferences)));
	}

	public static CachePrecacheImages cache_precache_images_old(
			final Context context,
			final SharedPreferences sharedPreferences) {

		if(network_tor(context, sharedPreferences)) {
			return CachePrecacheImages.NEVER;
		}

		if(!getBoolean(
				R.string.pref_cache_precache_images_key,
				true,
				context,
				sharedPreferences)) {
			return CachePrecacheImages.NEVER;
		} else if(getBoolean(
				R.string.pref_cache_precache_images_wifionly_key,
				true,
				context,
				sharedPreferences)) {
			return CachePrecacheImages.WIFIONLY;
		} else {
			return CachePrecacheImages.ALWAYS;
		}
	}

	// pref_cache_precache_comments

	public enum CachePrecacheComments {
		NEVER, WIFIONLY, ALWAYS
	}

	public static CachePrecacheComments cache_precache_comments(
			final Context context,
			final SharedPreferences sharedPreferences) {
		return CachePrecacheComments.valueOf(StringUtils.asciiUppercase(
				getString(
						R.string.pref_cache_precache_comments_list_key,
						"always",
						context,
						sharedPreferences)));
	}

	public static CachePrecacheComments cache_precache_comments_old(
			final Context context,
			final SharedPreferences sharedPreferences) {

		if(!getBoolean(
				R.string.pref_cache_precache_comments_key,
				true,
				context,
				sharedPreferences)) {
			return CachePrecacheComments.NEVER;
		} else if(getBoolean(
				R.string.pref_cache_precache_comments_wifionly_key,
				false,
				context,
				sharedPreferences)) {
			return CachePrecacheComments.WIFIONLY;
		} else {
			return CachePrecacheComments.ALWAYS;
		}
	}

	///////////////////////////////
	// pref_network
	///////////////////////////////

	public static boolean network_tor(
			final Context context,
			final SharedPreferences sharedPreferences) {
		return getBoolean(
				R.string.pref_network_tor_key,
				false,
				context,
				sharedPreferences);
	}

	///////////////////////////////
	// pref_menus
	///////////////////////////////

	public static EnumSet<RedditPreparedPost.Action> pref_menus_post_context_items(
			final Context context,
			final SharedPreferences sharedPreferences) {

		final Set<String> strings = getStringSet(
				R.string.pref_menus_post_context_items_key,
				R.array.pref_menus_post_context_items_return,
				context,
				sharedPreferences);

		final EnumSet<RedditPreparedPost.Action> result = EnumSet.noneOf(
				RedditPreparedPost.Action.class);
		for(final String s : strings) {
			result.add(RedditPreparedPost.Action.valueOf(StringUtils.asciiUppercase(s)));
		}

		return result;
	}

	public static EnumSet<RedditPreparedPost.Action> pref_menus_post_toolbar_items(
			final Context context,
			final SharedPreferences sharedPreferences) {

		final Set<String> strings = getStringSet(
				R.string.pref_menus_post_toolbar_items_key,
				R.array.pref_menus_post_toolbar_items_return,
				context,
				sharedPreferences);

		final EnumSet<RedditPreparedPost.Action> result = EnumSet.noneOf(
				RedditPreparedPost.Action.class);
		for(final String s : strings) {
			result.add(RedditPreparedPost.Action.valueOf(StringUtils.asciiUppercase(s)));
		}

		return result;
	}

	public static EnumSet<LinkHandler.LinkAction> pref_menus_link_context_items(
			final Context context,
			final SharedPreferences sharedPreferences) {

		final Set<String> strings = getStringSet(
				R.string.pref_menus_link_context_items_key,
				R.array.pref_menus_link_context_items_return,
				context,
				sharedPreferences);

		final EnumSet<LinkHandler.LinkAction> result
				= EnumSet.noneOf(LinkHandler.LinkAction.class);
		for(final String s : strings) {
			result.add(LinkHandler.LinkAction.valueOf(StringUtils.asciiUppercase(s)));
		}

		return result;
	}

	public static EnumSet<MainMenuListingManager.SubredditAction>
	pref_menus_subreddit_context_items(
			final Context context,
			final SharedPreferences sharedPreferences) {

		final Set<String> strings = getStringSet(
				R.string.pref_menus_subreddit_context_items_key,
				R.array.pref_menus_subreddit_context_items_return,
				context,
				sharedPreferences);

		final EnumSet<MainMenuListingManager.SubredditAction> result = EnumSet.noneOf(
				MainMenuListingManager.SubredditAction.class);
		for(final String s : strings) {
			result.add(MainMenuListingManager.SubredditAction.valueOf(StringUtils.asciiUppercase(
					s)));
		}

		return result;
	}

	public static EnumSet<MainMenuFragment.MainMenuUserItems> pref_menus_mainmenu_useritems(
			final Context context,
			final SharedPreferences sharedPreferences) {

		final Set<String> strings = getStringSet(
				R.string.pref_menus_mainmenu_useritems_key,
				R.array.pref_menus_mainmenu_useritems_items_default,
				context,
				sharedPreferences);

		final EnumSet<MainMenuFragment.MainMenuUserItems> result = EnumSet.noneOf(
				MainMenuFragment.MainMenuUserItems.class);
		for(final String s : strings) {
			result.add(MainMenuFragment.MainMenuUserItems.valueOf(StringUtils.asciiUppercase(s)));
		}

		return result;
	}

	public static EnumSet<MainMenuFragment.MainMenuShortcutItems> pref_menus_mainmenu_shortcutitems(
			final Context context,
			final SharedPreferences sharedPreferences) {

		final Set<String> strings = getStringSet(
				R.string.pref_menus_mainmenu_shortcutitems_key,
				R.array.pref_menus_mainmenu_shortcutitems_items_default,
				context,
				sharedPreferences);

		final EnumSet<MainMenuFragment.MainMenuShortcutItems> result = EnumSet.noneOf(
				MainMenuFragment.MainMenuShortcutItems.class);
		for(final String s : strings) {
			result.add(MainMenuFragment.MainMenuShortcutItems.valueOf(StringUtils.asciiUppercase(
					s)));
		}

		return result;
	}

	private static class AppbarItemInfo {
		final OptionsMenuUtility.AppbarItemsPref itemPref;
		final int stringRes, defaultValue;

		AppbarItemInfo(
				final OptionsMenuUtility.AppbarItemsPref itemPref,
				final int stringRes,
				final int defaultValue) {
			this.itemPref = itemPref;
			this.stringRes = stringRes;
			this.defaultValue = defaultValue;
		}
	}

	public static EnumMap<OptionsMenuUtility.AppbarItemsPref, Integer> pref_menus_appbar_items(
			final Context context,
			final SharedPreferences sharedPreferences) {

		final AppbarItemInfo[] appbarItemsInfo = new AppbarItemInfo[] {
				new AppbarItemInfo(
						OptionsMenuUtility.AppbarItemsPref.SORT,
						R.string.pref_menus_appbar_sort_key,
						MenuItem.SHOW_AS_ACTION_ALWAYS),
				new AppbarItemInfo(
						OptionsMenuUtility.AppbarItemsPref.REFRESH,
						R.string.pref_menus_appbar_refresh_key,
						MenuItem.SHOW_AS_ACTION_ALWAYS),
				new AppbarItemInfo(
						OptionsMenuUtility.AppbarItemsPref.PAST,
						R.string.pref_menus_appbar_past_key,
						MenuItem.SHOW_AS_ACTION_NEVER),
				new AppbarItemInfo(
						OptionsMenuUtility.AppbarItemsPref.SUBMIT_POST,
						R.string.pref_menus_appbar_submit_post_key,
						MenuItem.SHOW_AS_ACTION_NEVER),
				new AppbarItemInfo(
						OptionsMenuUtility.AppbarItemsPref.PIN,
						R.string.pref_menus_appbar_pin_key,
						MenuItem.SHOW_AS_ACTION_NEVER),
				new AppbarItemInfo(
						OptionsMenuUtility.AppbarItemsPref.SUBSCRIBE,
						R.string.pref_menus_appbar_subscribe_key,
						MenuItem.SHOW_AS_ACTION_NEVER),
				new AppbarItemInfo(
						OptionsMenuUtility.AppbarItemsPref.BLOCK,
						R.string.pref_menus_appbar_block_key,
						MenuItem.SHOW_AS_ACTION_NEVER),
				new AppbarItemInfo(
						OptionsMenuUtility.AppbarItemsPref.SIDEBAR,
						R.string.pref_menus_appbar_sidebar_key,
						MenuItem.SHOW_AS_ACTION_NEVER),
				new AppbarItemInfo(
						OptionsMenuUtility.AppbarItemsPref.ACCOUNTS,
						R.string.pref_menus_appbar_accounts_key,
						MenuItem.SHOW_AS_ACTION_NEVER),
				new AppbarItemInfo(
						OptionsMenuUtility.AppbarItemsPref.THEME,
						R.string.pref_menus_appbar_theme_key,
						MenuItem.SHOW_AS_ACTION_NEVER),
				new AppbarItemInfo(
						OptionsMenuUtility.AppbarItemsPref.SETTINGS,
						R.string.pref_menus_appbar_settings_key,
						MenuItem.SHOW_AS_ACTION_NEVER),
				new AppbarItemInfo(
						OptionsMenuUtility.AppbarItemsPref.CLOSE_ALL,
						R.string.pref_menus_appbar_close_all_key,
						OptionsMenuUtility.DO_NOT_SHOW),
				new AppbarItemInfo(
						OptionsMenuUtility.AppbarItemsPref.REPLY,
						R.string.pref_menus_appbar_reply_key,
						MenuItem.SHOW_AS_ACTION_NEVER),
				new AppbarItemInfo(
						OptionsMenuUtility.AppbarItemsPref.SEARCH,
						R.string.pref_menus_appbar_search_key,
						MenuItem.SHOW_AS_ACTION_NEVER)
		};


		final EnumMap<OptionsMenuUtility.AppbarItemsPref, Integer> appbarItemsPrefs
				= new EnumMap<>(OptionsMenuUtility.AppbarItemsPref.class);

		for(final AppbarItemInfo item : appbarItemsInfo) {
			try {
				appbarItemsPrefs.put(item.itemPref, Integer.parseInt(getString(
						item.stringRes,
						Integer.toString(item.defaultValue),
						context,
						sharedPreferences)));
			} catch(final NumberFormatException | NullPointerException e) {
				appbarItemsPrefs.put(item.itemPref, item.defaultValue);
			}
		}

		return appbarItemsPrefs;
	}

	///////////////////////////////
	// pref_pinned_subreddits
	///////////////////////////////

	public static List<SubredditCanonicalId> pref_pinned_subreddits(
			final Context context,
			final SharedPreferences sharedPreferences) {
		return pref_subreddits_list(
				context,
				sharedPreferences,
				R.string.pref_pinned_subreddits_key);
	}

	public static void pref_pinned_subreddits_add(
			final Context context,
			final SharedPreferences sharedPreferences,
			final SubredditCanonicalId subreddit) {

		pref_subreddits_add(
				context,
				sharedPreferences,
				subreddit,
				R.string.pref_pinned_subreddits_key);

		General.quickToast(context, context.getApplicationContext().getString(
				R.string.pin_successful,
				subreddit.toString()));
	}

	public static void pref_pinned_subreddits_remove(
			final Context context,
			final SharedPreferences sharedPreferences,
			final SubredditCanonicalId subreddit) {

		pref_subreddits_remove(
				context,
				sharedPreferences,
				subreddit,
				R.string.pref_pinned_subreddits_key);

		General.quickToast(context, context.getApplicationContext().getString(
				R.string.unpin_successful,
				subreddit.toString()));
	}

	public static boolean pref_pinned_subreddits_check(
			final Context context,
			final SharedPreferences sharedPreferences,
			final SubredditCanonicalId id) {

		return pref_pinned_subreddits(context, sharedPreferences).contains(id);
	}

	///////////////////////////////
	// pref_blocked_subreddits
	///////////////////////////////

	public static List<SubredditCanonicalId> pref_blocked_subreddits(
			final Context context,
			final SharedPreferences sharedPreferences) {

		return pref_subreddits_list(
				context,
				sharedPreferences,
				R.string.pref_blocked_subreddits_key);
	}

	public static void pref_blocked_subreddits_add(
			final Context context,
			final SharedPreferences sharedPreferences,
			final SubredditCanonicalId subreddit) {

		pref_subreddits_add(
				context,
				sharedPreferences,
				subreddit,
				R.string.pref_blocked_subreddits_key);

		General.quickToast(context, R.string.block_done);
	}

	public static void pref_blocked_subreddits_remove(
			final Context context,
			final SharedPreferences sharedPreferences,
			final SubredditCanonicalId subreddit) {

		pref_subreddits_remove(
				context,
				sharedPreferences,
				subreddit,
				R.string.pref_blocked_subreddits_key);

		General.quickToast(context, R.string.unblock_done);
	}

	public static boolean pref_blocked_subreddits_check(
			final Context context,
			final SharedPreferences sharedPreferences,
			final SubredditCanonicalId subreddit) {

		return pref_blocked_subreddits(context, sharedPreferences).contains(subreddit);
	}

	///////////////////////////////
	// Shared pref_subreddits methods
	///////////////////////////////

	private static void pref_subreddits_add(
			final Context context,
			final SharedPreferences sharedPreferences,
			final SubredditCanonicalId subreddit,
			final int prefId) {

		final String value = getString(prefId, "", context, sharedPreferences);
		final ArrayList<String> list = WritableHashSet.escapedStringToList(value);

		if(!list.contains(subreddit.toString())) {
			list.add(subreddit.toString());
			final String result = WritableHashSet.listToEscapedString(list);
			sharedPreferences.edit().putString(context.getString(prefId), result).apply();
		}
	}

	private static void pref_subreddits_remove(
			final Context context,
			final SharedPreferences sharedPreferences,
			final SubredditCanonicalId subreddit,
			final int prefId) {

		final String value = getString(prefId, "", context, sharedPreferences);
		final ArrayList<String> list = WritableHashSet.escapedStringToList(value);

		final Iterator<String> iterator = list.iterator();

		while(iterator.hasNext()) {

			final String id = iterator.next();

			if(id.equals(subreddit.toString())) {
				iterator.remove();
				break;
			}
		}

		final String resultStr = WritableHashSet.listToEscapedString(list);

		sharedPreferences.edit().putString(context.getString(prefId), resultStr).apply();
	}

	public static List<SubredditCanonicalId> pref_subreddits_list(
			final Context context,
			final SharedPreferences sharedPreferences,
			final int prefId) {

		final String value = getString(prefId, "", context, sharedPreferences);
		final ArrayList<String> list = WritableHashSet.escapedStringToList(value);

		final ArrayList<SubredditCanonicalId> result = new ArrayList<>(list.size());

		try {
			for(final String str : list) {
				result.add(new SubredditCanonicalId(str));
			}
		} catch(final InvalidSubredditNameException e) {
			throw new RuntimeException(e);
		}

		return result;
	}
}
