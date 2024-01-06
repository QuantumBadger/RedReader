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
import android.content.res.Resources;
import android.os.Build;
import android.util.DisplayMetrics;
import android.view.MenuItem;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.annotation.StringRes;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.activities.OptionsMenuUtility;
import org.quantumbadger.redreader.adapters.MainMenuListingManager;
import org.quantumbadger.redreader.common.time.TimeDuration;
import org.quantumbadger.redreader.fragments.MainMenuFragment;
import org.quantumbadger.redreader.io.WritableHashSet;
import org.quantumbadger.redreader.reddit.PostCommentSort;
import org.quantumbadger.redreader.reddit.PostSort;
import org.quantumbadger.redreader.reddit.UserCommentSort;
import org.quantumbadger.redreader.reddit.api.RedditAPICommentAction;
import org.quantumbadger.redreader.reddit.api.RedditPostActions;
import org.quantumbadger.redreader.reddit.things.InvalidSubredditNameException;
import org.quantumbadger.redreader.reddit.things.SubredditCanonicalId;

import java.io.File;
import java.util.ArrayList;
import java.util.EnumMap;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.atomic.AtomicReference;

public final class PrefsUtility {

	private static SharedPrefsWrapper sharedPrefs;
	private static Resources mRes;

	private static String getPrefKey(@StringRes final int prefKey) {
		return mRes.getString(prefKey);
	}

	@NonNull private static final AtomicReference<Locale> mDefaultLocale = new AtomicReference<>();

	@Nullable
	public static String getString(
			final int id,
			@Nullable final String defaultValue) {
		return sharedPrefs.getString(getPrefKey(id), defaultValue);
	}

	public static Set<String> getStringSet(
			final int id,
			final int defaultArrayRes) {
		return sharedPrefs.getStringSet(
				getPrefKey(id),
				General.hashsetFromArray(mRes.getStringArray(defaultArrayRes)));
	}

	private static boolean getBoolean(
			final int id,
			final boolean defaultValue) {
		return sharedPrefs.getBoolean(getPrefKey(id), defaultValue);
	}

	private static void setBoolean(
			final int id,
			final boolean newValue) {
		sharedPrefs.edit().putBoolean(getPrefKey(id), newValue).apply();
	}

	@SuppressWarnings("unused")
	private static long getLong(
			final int id,
			final long defaultValue) {
		return sharedPrefs.getLong(getPrefKey(id), defaultValue);
	}

	public static boolean isReLayoutRequired(final Context context, final String key) {
		return context.getString(R.string.pref_appearance_theme_key).equals(key)
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
						R.string.pref_appearance_hide_headertoolbar_postlist_key))
				|| key.equals(context.getString(
					R.string.pref_appearance_hide_comments_from_blocked_users_key))
				|| key.equals(context.getString(R.string.pref_images_thumbnail_size_key))
				|| key.equals(context.getString(R.string.pref_images_inline_image_previews_key))
				|| key.equals(context.getString(
						R.string.pref_images_inline_image_previews_nsfw_key))
				|| key.equals(context.getString(
						R.string.pref_images_inline_image_previews_spoiler_key))
				|| key.equals(context.getString(R.string.pref_images_high_res_thumbnails_key))
				|| key.equals(context.getString(
						R.string.pref_accessibility_separate_body_text_lines_key))
				|| key.equals(context.getString(
						R.string.pref_accessibility_min_comment_height_key))
				|| key.equals(context.getString(
						R.string.pref_behaviour_post_title_opens_comments_key))
				|| key.equals(context.getString(
						R.string.pref_behaviour_post_tap_action_key))
				|| key.equals(context.getString(
						R.string.pref_accessibility_say_comment_indent_level_key))
				|| key.equals(context.getString(
						R.string.pref_behaviour_collapse_sticky_comments_key))
				|| key.equals(context.getString(
						R.string.pref_accessibility_concise_mode_key))
				|| key.equals(context.getString(
						R.string.pref_appearance_post_hide_subreddit_header_key))
				|| key.equals(REDDIT_USER_AGREEMENT_PREF)
				|| key.equals(context.getString(R.string.pref_reddit_client_id_override_key));
	}

	public static boolean isRestartRequired(final Context context, final String key) {
		return context.getString(R.string.pref_appearance_twopane_key).equals(key)
				|| context.getString(R.string.pref_appearance_theme_key).equals(key)
				|| context.getString(R.string.pref_appearance_navbar_color_key).equals(key)
				|| context.getString(R.string.pref_appearance_langforce_key).equals(key)
				|| context.getString(R.string.pref_behaviour_bezel_toolbar_swipezone_key)
						.equals(key)
				|| context.getString(R.string.pref_appearance_hide_username_main_menu_key)
						.equals(key)
				|| context.getString(R.string.pref_appearance_android_status_key).equals(key)
				|| context.getString(R.string.pref_appearance_comments_show_floating_toolbar_key)
						.equals(key)
				|| context.getString(R.string.pref_behaviour_enable_swipe_refresh_key).equals(key)
				|| context.getString(R.string.pref_menus_show_multireddit_main_menu_key).equals(key)
				|| context.getString(R.string.pref_menus_show_subscribed_subreddits_main_menu_key)
						.equals(key)
				|| context.getString(R.string.pref_menus_mainmenu_dev_announcements_key).equals(key)
				|| context.getString(R.string.pref_appearance_bottom_toolbar_key).equals(key)
				|| context.getString(R.string.pref_appearance_hide_toolbar_on_scroll_key)
						.equals(key)
				|| context.getString(R.string.pref_behaviour_block_screenshots_key).equals(key)
				|| context.getString(R.string.pref_behaviour_keep_screen_awake_key).equals(key);
	}

	public static void init(final Context context) {

		ConfigProviders.register(() -> "IJuC7OVo2SgR0QVvEZXr913LYMKU4r7pTqrmPe3MpddGEB+YheeH3jTZ+" +
				"GbEQgpSutsgJugRCPETQGRwkZrw1LJxR93RpgC1iO+G/hN9BaPU1c0Qt33SSMzHCqLzU66dpD/L0yC42" +
				"GhcJF+GUAaRzCnk0BxPjN09aO2H5rQPnUGB1kurxxCExKzWy4gEyWokgYzGGNQwAA==");

		sharedPrefs = General.getSharedPrefs(context);
		mRes = Objects.requireNonNull(context.getResources());
		General.initAppConfig(context);
	}

	///////////////////////////////
	// pref_appearance
	///////////////////////////////

	// pref_appearance_twopane

	public enum AppearanceTwopane {
		NEVER, AUTO, FORCE
	}

	@NonNull
	public static AppearanceTwopane appearance_twopane() {
		return AppearanceTwopane.valueOf(StringUtils.asciiUppercase(getString(
				R.string.pref_appearance_twopane_key,
				"auto")));
	}

	public enum AppearanceTheme {
		RED, GREEN, BLUE, LTBLUE, ORANGE, GRAY, NIGHT, NIGHT_LOWCONTRAST, ULTRABLACK
	}

	public static boolean isNightMode() {

		final AppearanceTheme theme = appearance_theme();

		return theme == AppearanceTheme.NIGHT
				|| theme == AppearanceTheme.NIGHT_LOWCONTRAST
				|| theme == AppearanceTheme.ULTRABLACK;
	}

	public static AppearanceTheme appearance_theme() {
		return AppearanceTheme.valueOf(StringUtils.asciiUppercase(getString(
				R.string.pref_appearance_theme_key,
				"red")));
	}

	public enum AppearanceNavbarColour {
		BLACK, WHITE, PRIMARY, PRIMARYDARK
	}

	public static AppearanceNavbarColour appearance_navbar_colour() {
		return AppearanceNavbarColour.valueOf(StringUtils.asciiUppercase(getString(
				R.string.pref_appearance_navbar_color_key,
				"black")));
	}

	public static void applyTheme(@NonNull final Activity activity) {

		final AppearanceTheme theme = appearance_theme();

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

		applyLanguage(activity);
	}

	public static void applySettingsTheme(@NonNull final Activity activity) {
		activity.setTheme(R.style.RR_Settings);
		applyLanguage(activity);
	}

	private static void applyLanguage(final Activity activity) {

		synchronized(mDefaultLocale) {
			if(mDefaultLocale.get() == null) {
				mDefaultLocale.set(Locale.getDefault());
			}
		}

		final String lang = getString(
				R.string.pref_appearance_langforce_key,
				"auto");

		for(final Resources res : new Resources[] {
				activity.getResources(),
				activity.getApplication().getResources()}) {

			final DisplayMetrics dm = res.getDisplayMetrics();
			final android.content.res.Configuration conf = res.getConfiguration();

			if(!lang.equals("auto")) {

				if(lang.contains("-r")) {
					final String[] split = lang.split("-r");
					setLocaleOnConfiguration(conf, new Locale(split[0], split[1]));

				} else {
					setLocaleOnConfiguration(conf, new Locale(lang));
				}

			} else {
				setLocaleOnConfiguration(conf, mDefaultLocale.get());
			}

			res.updateConfiguration(conf, dm);
		}
	}

	private static void setLocaleOnConfiguration(
			@NonNull final android.content.res.Configuration conf,
			@NonNull final Locale locale) {

		Locale.setDefault(locale);

		if(Build.VERSION.SDK_INT >= 17) {
			conf.setLocale(locale);
		} else {
			//noinspection deprecation
			conf.locale = locale;
		}

	}

	public static NeverAlwaysOrWifiOnly appearance_thumbnails_show() {
		return NeverAlwaysOrWifiOnly.valueOf(StringUtils.asciiUppercase(
				getString(
						R.string.pref_appearance_thumbnails_show_list_key,
						"always")));
	}

	public static NeverAlwaysOrWifiOnly appearance_thumbnails_show_old() {

		if(!getBoolean(
				R.string.pref_appearance_thumbnails_show_key,
				true)) {
			return NeverAlwaysOrWifiOnly.NEVER;
		} else if(getBoolean(
				R.string.pref_appearance_thumbnails_wifionly_key,
				false)) {
			return NeverAlwaysOrWifiOnly.WIFIONLY;
		} else {
			return NeverAlwaysOrWifiOnly.ALWAYS;
		}
	}

	public static boolean appearance_thumbnails_nsfw_show() {
		return getBoolean(
				R.string.pref_appearance_thumbnails_nsfw_show_key,
				false);
	}

	public static boolean appearance_thumbnails_spoiler_show() {
		return getBoolean(
				R.string.pref_appearance_thumbnails_spoiler_show_key,
				false);
	}

	public static float appearance_fontscale_global() {
		return Float.parseFloat(getString(
				R.string.pref_appearance_fontscale_global_key,
				"1"));
	}

	public static float appearance_fontscale_bodytext() {
		if(getString(
				R.string.pref_appearance_fontscale_bodytext_key,
				"-1").equals("-1")) {
			return appearance_fontscale_global();
		}
		return Float.parseFloat(getString(
				R.string.pref_appearance_fontscale_bodytext_key,
				"-1"));
	}

	public static float appearance_fontscale_comment_headers() {
		if(getString(
				R.string.pref_appearance_fontscale_comment_headers_key,
				"-1").equals("-1")) {
			return appearance_fontscale_global();
		}
		return Float.parseFloat(getString(
				R.string.pref_appearance_fontscale_comment_headers_key,
				"-1"));
	}

	public static float appearance_fontscale_linkbuttons() {
		if(getString(
				R.string.pref_appearance_fontscale_linkbuttons_key,
				"-1").equals("-1")) {
			return appearance_fontscale_global();
		}
		return Float.parseFloat(getString(
				R.string.pref_appearance_fontscale_linkbuttons_key,
				"-1"));
	}

	public static float appearance_fontscale_posts() {
		if(getString(
				R.string.pref_appearance_fontscale_posts_key,
				"-1").equals("-1")) {
			return appearance_fontscale_global();
		}
		return Float.parseFloat(getString(
				R.string.pref_appearance_fontscale_posts_key,
				"-1"));
	}

	public static float appearance_fontscale_post_subtitles() {
		if(getString(
				R.string.pref_appearance_fontscale_post_subtitles_key,
				"-1").equals("-1")) {
			return appearance_fontscale_global();
		}
		return Float.parseFloat(getString(
				R.string.pref_appearance_fontscale_post_subtitles_key,
				"-1"));
	}

	public static float appearance_fontscale_post_header_titles() {
		if(getString(
				R.string.pref_appearance_fontscale_post_header_titles_key,
				"-1").equals("-1")) {
			return appearance_fontscale_global();
		}
		return Float.parseFloat(getString(
				R.string.pref_appearance_fontscale_post_header_titles_key,
				"-1"));
	}

	public static float appearance_fontscale_post_header_subtitles() {
		if(getString(
				R.string.pref_appearance_fontscale_post_header_subtitles_key,
				"-1").equals("-1")) {
			return appearance_fontscale_global();
		}
		return Float.parseFloat(getString(
				R.string.pref_appearance_fontscale_post_header_subtitles_key,
				"-1"));
	}

	public static boolean pref_appearance_hide_username_main_menu() {
		return getBoolean(
				R.string.pref_appearance_hide_username_main_menu_key,
				false);
	}

	public static boolean pref_show_popular_main_menu() {
		return getBoolean(
				R.string.pref_menus_show_popular_main_menu_key,
				false);
	}

	public static boolean pref_show_random_main_menu() {
		return getBoolean(
				R.string.pref_menus_show_random_main_menu_key,
				false);
	}

	public static boolean pref_show_multireddit_main_menu() {
		return getBoolean(
				R.string.pref_menus_show_multireddit_main_menu_key,
				true);
	}

	public static boolean pref_show_subscribed_subreddits_main_menu() {
		return getBoolean(
				R.string.pref_menus_show_subscribed_subreddits_main_menu_key,
				true);
	}

	public static boolean pref_menus_mainmenu_dev_announcements() {
		return getBoolean(
				R.string.pref_menus_mainmenu_dev_announcements_key,
				true);
	}

	public static boolean pref_appearance_show_blocked_subreddits_main_menu() {
		return getBoolean(
				R.string.pref_appearance_show_blocked_subreddits_main_menu_key,
				false);
	}

	public static boolean pref_appearance_linkbuttons() {
		return getBoolean(
				R.string.pref_appearance_linkbuttons_key,
				true);
	}

	public enum AppearanceStatusBarMode {
		ALWAYS_HIDE, HIDE_ON_MEDIA, NEVER_HIDE
	}

	public static AppearanceStatusBarMode pref_appearance_android_status() {
		return AppearanceStatusBarMode.valueOf(StringUtils.asciiUppercase(getString(
				R.string.pref_appearance_android_status_key,
				"never_hide")));
	}

	public static boolean pref_appearance_link_text_clickable() {
		return getBoolean(
				R.string.pref_appearance_link_text_clickable_key,
				true);
	}

	public static boolean pref_appearance_image_viewer_show_floating_toolbar() {
		return getBoolean(
				R.string.pref_appearance_image_viewer_show_floating_toolbar_key,
				true);
	}

	public static boolean pref_appearance_show_aspect_ratio_indicator() {
		return getBoolean(
				R.string.pref_appearance_show_aspect_ratio_indicator_key,
				false);
	}

	public static boolean pref_album_skip_to_first() {
		return getBoolean(
				R.string.pref_album_skip_to_first_key,
				false);
	}

	public static boolean pref_appearance_comments_show_floating_toolbar() {
		return getBoolean(
				R.string.pref_appearance_comments_show_floating_toolbar_key,
				true);
	}

	public static boolean pref_appearance_indentlines() {
		return getBoolean(
				R.string.pref_appearance_indentlines_key,
				false);
	}

	public static boolean pref_appearance_left_handed() {
		return getBoolean(
				R.string.pref_appearance_left_handed_key,
				false);
	}

	public static boolean pref_appearance_bottom_toolbar() {
		return getBoolean(
				R.string.pref_appearance_bottom_toolbar_key,
				false);
	}

	public static boolean pref_appearance_hide_toolbar_on_scroll() {
		return getBoolean(
				R.string.pref_appearance_hide_toolbar_on_scroll_key,
				false);
	}

	public static boolean pref_appearance_post_hide_subreddit_header() {
		return getBoolean(
				R.string.pref_appearance_post_hide_subreddit_header_key,
				false);
	}

	public static boolean pref_appearance_hide_headertoolbar_postlist() {
		return getBoolean(
				R.string.pref_appearance_hide_headertoolbar_postlist_key,
				false);
	}

	public static boolean pref_appearance_hide_headertoolbar_commentlist() {
		return getBoolean(
				R.string.pref_appearance_hide_headertoolbar_commentlist_key,
				false);
	}

	public static boolean pref_appearance_hide_comments_from_blocked_users() {
		return getBoolean(
				R.string.pref_appearance_hide_comments_from_blocked_users_key,
				false);
	}

	public enum AppearancePostSubtitleItem {
		AUTHOR,
		FLAIR,
		SCORE,
		AGE,
		GOLD,
		SUBREDDIT,
		DOMAIN,
		STICKY,
		SPOILER,
		NSFW,
		UPVOTE_RATIO,
		COMMENTS
	}

	public static EnumSet<AppearancePostSubtitleItem> appearance_post_subtitle_items() {

		final Set<String> strings = getStringSet(
				R.string.pref_appearance_post_subtitle_items_key,
				R.array.pref_appearance_post_subtitle_items_default);

		final EnumSet<AppearancePostSubtitleItem> result = EnumSet.noneOf(
				AppearancePostSubtitleItem.class);
		for(final String s : strings) {
			result.add(AppearancePostSubtitleItem.valueOf(StringUtils.asciiUppercase(s)));
		}

		return result;
	}

	public static int appearance_post_age_units() {
		try {
			return Integer.parseInt(getString(
					R.string.pref_appearance_post_age_units_key,
					"2"));
		} catch(final Throwable e) {
			return 2;
		}
	}

	public static boolean appearance_post_subtitle_items_use_different_settings() {
		return getBoolean(
				R.string.pref_appearance_post_subtitle_items_use_different_settings_key,
				false);
	}

	public static EnumSet<AppearancePostSubtitleItem> appearance_post_header_subtitle_items() {

		final Set<String> strings = getStringSet(
				R.string.pref_appearance_post_header_subtitle_items_key,
				R.array.pref_appearance_post_subtitle_items_default);

		final EnumSet<AppearancePostSubtitleItem> result = EnumSet.noneOf(
				AppearancePostSubtitleItem.class);
		for(final String s : strings) {
			result.add(AppearancePostSubtitleItem.valueOf(StringUtils.asciiUppercase(s)));
		}

		return result;
	}

	public static int appearance_post_header_age_units() {
		try {
			return Integer.parseInt(getString(
					R.string.pref_appearance_post_header_age_units_key,
					"2"));
		} catch(final Throwable e) {
			return 2;
		}
	}

	public static boolean appearance_post_show_comments_button() {
		return getBoolean(
				R.string.pref_appearance_post_show_comments_button_key,
				true);
	}

	public enum AppearanceCommentHeaderItem {
		AUTHOR, FLAIR, SCORE, CONTROVERSIALITY, AGE, GOLD, SUBREDDIT
	}

	public static EnumSet<AppearanceCommentHeaderItem> appearance_comment_header_items() {

		final Set<String> strings = getStringSet(
				R.string.pref_appearance_comment_header_items_key,
				R.array.pref_appearance_comment_header_items_default);

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

	public static int appearance_comment_age_units() {
		try {
			return Integer.parseInt(getString(
					R.string.pref_appearance_comment_age_units_key,
					"2"));
		} catch(final Throwable e) {
			return 2;
		}
	}

	public enum CommentAgeMode {
		ABSOLUTE, RELATIVE_POST, RELATIVE_PARENT
	}

	public static boolean appearance_user_show_avatars() {
		return getBoolean(
				R.string.pref_appearance_user_show_avatars_key,
				true);
	}

	public static CommentAgeMode appearance_comment_age_mode() {
		return CommentAgeMode.valueOf(StringUtils.asciiUppercase(getString(
				R.string.pref_appearance_comment_age_mode_key,
				"absolute")));
	}

	public static int appearance_inbox_age_units() {
		try {
			return Integer.parseInt(getString(
					R.string.pref_appearance_inbox_age_units_key,
					"2"));
		} catch(final Throwable e) {
			return 2;
		}
	}

	public static int images_thumbnail_size_dp() {
		try {
			return Integer.parseInt(getString(
					R.string.pref_images_thumbnail_size_key,
					"64"));
		} catch(final Throwable e) {
			return 64;
		}
	}

	public static NeverAlwaysOrWifiOnly images_inline_image_previews() {
		return NeverAlwaysOrWifiOnly.valueOf(StringUtils.asciiUppercase(
				getString(
						R.string.pref_images_inline_image_previews_key,
						"always")));
	}

	public static boolean images_inline_image_previews_nsfw() {
		return getBoolean(
				R.string.pref_images_inline_image_previews_nsfw_key,
				false);
	}

	public static boolean images_inline_image_previews_spoiler() {
		return getBoolean(
				R.string.pref_images_inline_image_previews_spoiler_key,
				false);
	}

	public static NeverAlwaysOrWifiOnly images_high_res_thumbnails() {
		return NeverAlwaysOrWifiOnly.valueOf(StringUtils.asciiUppercase(
				getString(
						R.string.pref_images_high_res_thumbnails_key,
						"wifionly")));
	}

	///////////////////////////////
	// pref_behaviour
	///////////////////////////////

	public static boolean pref_behaviour_skiptofrontpage() {
		return getBoolean(
				R.string.pref_behaviour_skiptofrontpage_key,
				false);
	}

	public static boolean pref_behaviour_useinternalbrowser() {
		return getBoolean(
				R.string.pref_behaviour_useinternalbrowser_key,
				true);
	}

	public static boolean pref_behaviour_usecustomtabs() {
		return getBoolean(
				R.string.pref_behaviour_usecustomtabs_key,
				false);
	}

	public static void set_pref_behaviour_notifications(final boolean enabled) {
		setBoolean(
				R.string.pref_behaviour_notifications_key,
				enabled);
	}

	public static boolean pref_behaviour_notifications() {
		return getBoolean(
				R.string.pref_behaviour_notifications_key,
				true);
	}

	public static boolean pref_behaviour_enable_swipe_refresh() {
		return getBoolean(
				R.string.pref_behaviour_enable_swipe_refresh_key,
				true);
	}

	public static boolean pref_behaviour_video_playback_controls() {
		return getBoolean(
				R.string.pref_behaviour_video_playback_controls_key,
				false);
	}

	public static boolean pref_behaviour_video_mute_default() {
		return getBoolean(
				R.string.pref_behaviour_video_mute_default_key,
				true);
	}

	public static boolean pref_behaviour_video_zoom_default() {
		return getBoolean(R.string.pref_behaviour_video_zoom_default_key,
				false);
	}

	public static boolean pref_videos_download_before_playing() {
		return getBoolean(R.string.pref_videos_download_before_playing_key,
				false);
	}

	public static boolean pref_behaviour_imagevideo_tap_close() {
		return getBoolean(R.string.pref_behaviour_imagevideo_tap_close_key,
				true);
	}

	public static int pref_behaviour_bezel_toolbar_swipezone_dp() {
		try {
			return Integer.parseInt(getString(
					R.string.pref_behaviour_bezel_toolbar_swipezone_key,
					"10"));
		} catch(final Throwable e) {
			return 10;
		}
	}

	public static boolean pref_behaviour_back_again() {
		return getBoolean(R.string.pref_behaviour_postlist_back_again_key,
				false);
	}

	public static int pref_behaviour_gallery_swipe_length_dp() {
		try {
			return Integer.parseInt(getString(
					R.string.pref_behaviour_gallery_swipe_length_key,
					"150"));
		} catch(final Throwable e) {
			return 150;
		}
	}

	public static Integer pref_behaviour_comment_min() {
		final Integer defaultValue = -4;

		final String value = getString(
				R.string.pref_behaviour_comment_min_key,
				defaultValue.toString());

		if(value == null || value.trim().isEmpty()) {
			return null;
		}

		try {
			return Integer.parseInt(value);
		} catch(final Throwable e) {
			return defaultValue;
		}
	}

	public enum PostTapAction {
		LINK, COMMENTS, TITLE_COMMENTS
	}

	public static PostTapAction pref_behaviour_post_tap_action() {
		return PostTapAction.valueOf(StringUtils.asciiUppercase(getString(
				R.string.pref_behaviour_post_tap_action_key,
				"link"
		)));
	}

	public static boolean pref_behaviour_post_title_opens_comments() {
		return getBoolean(
				R.string.pref_behaviour_post_title_opens_comments_key,
				false);
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

	public static ImageViewMode pref_behaviour_imageview_mode() {
		return ImageViewMode.valueOf(StringUtils.asciiUppercase(getString(
				R.string.pref_behaviour_imageview_mode_key,
				"internal_opengl")));
	}

	// pref_behaviour_albumview_mode

	public enum AlbumViewMode {
		INTERNAL_LIST,
		INTERNAL_BROWSER,
		EXTERNAL_BROWSER
	}

	public static AlbumViewMode pref_behaviour_albumview_mode() {
		return AlbumViewMode.valueOf(StringUtils.asciiUppercase(getString(
				R.string.pref_behaviour_albumview_mode_key,
				"internal_list")));
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

	public static GifViewMode pref_behaviour_gifview_mode() {
		return GifViewMode.valueOf(StringUtils.asciiUppercase(getString(
				R.string.pref_behaviour_gifview_mode_key,
				"internal_movie")));
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

	public static VideoViewMode pref_behaviour_videoview_mode() {
		return VideoViewMode.valueOf(StringUtils.asciiUppercase(getString(
				R.string.pref_behaviour_videoview_mode_key,
				"internal_videoview")));
	}

	// pref_behaviour_fling_post

	public enum PostFlingAction {
		UPVOTE,
		DOWNVOTE,
		SAVE,
		HIDE,
		COMMENTS,
		LINK,
		ACTION_MENU,
		BROWSER,
		BACK,
		REPORT,
		SAVE_IMAGE,
		GOTO_SUBREDDIT,
		SHARE,
		SHARE_COMMENTS,
		SHARE_IMAGE,
		COPY,
		USER_PROFILE,
		PROPERTIES,
		DISABLED
	}

	public static PostFlingAction pref_behaviour_fling_post_left() {
		return PostFlingAction.valueOf(StringUtils.asciiUppercase(getString(
				R.string.pref_behaviour_fling_post_left_key,
				"downvote")));
	}

	public static PostFlingAction pref_behaviour_fling_post_right() {
		return PostFlingAction.valueOf(StringUtils.asciiUppercase(getString(
				R.string.pref_behaviour_fling_post_right_key,
				"upvote")));
	}

	public enum SelfpostAction {
		COLLAPSE, NOTHING
	}

	public static SelfpostAction pref_behaviour_self_post_tap_actions() {
		return SelfpostAction.valueOf(StringUtils.asciiUppercase(getString(
				R.string.pref_behaviour_self_post_tap_actions_key,
				"collapse")));
	}

	// pref_behaviour_fling_comment

	public enum CommentFlingAction {
		UPVOTE,
		DOWNVOTE,
		SAVE,
		REPORT,
		REPLY,
		CONTEXT,
		GO_TO_COMMENT,
		COMMENT_LINKS,
		SHARE,
		COPY_TEXT,
		COPY_URL,
		USER_PROFILE,
		COLLAPSE,
		ACTION_MENU,
		PROPERTIES,
		BACK,
		DISABLED
	}

	public static CommentFlingAction pref_behaviour_fling_comment_left() {
		return CommentFlingAction.valueOf(StringUtils.asciiUppercase(getString(
				R.string.pref_behaviour_fling_comment_left_key,
				"downvote")));
	}

	public static CommentFlingAction pref_behaviour_fling_comment_right() {
		return CommentFlingAction.valueOf(StringUtils.asciiUppercase(getString(
				R.string.pref_behaviour_fling_comment_right_key,
				"upvote")));
	}

	public enum CommentAction {
		COLLAPSE, ACTION_MENU, NOTHING
	}

	public static CommentAction pref_behaviour_actions_comment_tap() {
		return CommentAction.valueOf(StringUtils.asciiUppercase(getString(
				R.string.pref_behaviour_actions_comment_tap_key,
				"collapse")));
	}

	public static CommentAction pref_behaviour_actions_comment_longclick() {
		return CommentAction.valueOf(StringUtils.asciiUppercase(getString(
				R.string.pref_behaviour_actions_comment_longclick_key,
				"action_menu")));
	}

	public static boolean pref_behaviour_sharing_share_text() {
		return getBoolean(
				R.string.pref_behaviour_sharing_share_text_key,
				true);
	}

	public static boolean pref_behaviour_sharing_include_desc() {
		return getBoolean(
				R.string.pref_behaviour_sharing_include_desc_key,
				true);
	}

	public static boolean pref_behaviour_sharing_dialog() {
		return getBoolean(
				R.string.pref_behaviour_sharing_share_dialog_key,
				false);
	}

	public static String pref_behaviour_sharing_dialog_data_get() {
		return getString(
				R.string.pref_behaviour_sharing_share_dialog_data,
				"");
	}

	public static void pref_behaviour_sharing_dialog_data_set(
			final Context context,
			final String appNames) {
		sharedPrefs.edit()
				.putString(
						context.getString(R.string.pref_behaviour_sharing_share_dialog_data),
						appNames)
				.apply();
	}

	public static PostSort pref_behaviour_postsort() {
		return PostSort.valueOf(StringUtils.asciiUppercase(getString(
				R.string.pref_behaviour_postsort_key,
				"hot")));
	}

	public static PostSort pref_behaviour_user_postsort() {
		return PostSort.valueOf(StringUtils.asciiUppercase(getString(
				R.string.pref_behaviour_user_postsort_key,
				"new")));
	}

	public static PostSort pref_behaviour_multi_postsort() {
		return PostSort.valueOf(StringUtils.asciiUppercase(getString(
				R.string.pref_behaviour_multi_postsort_key,
				"hot")));
	}

	public static PostCommentSort pref_behaviour_commentsort() {
		return PostCommentSort.valueOf(StringUtils.asciiUppercase(getString(
				R.string.pref_behaviour_commentsort_key,
				"best")));
	}

	public static UserCommentSort pref_behaviour_user_commentsort() {
		return UserCommentSort.valueOf(StringUtils.asciiUppercase(getString(
				R.string.pref_behaviour_user_commentsort_key,
				"new")));
	}

	public enum PinnedSubredditSort {
		NAME, DATE
	}

	public static PinnedSubredditSort pref_behaviour_pinned_subredditsort() {
		return PinnedSubredditSort.valueOf(StringUtils.asciiUppercase(getString(
				R.string.pref_behaviour_pinned_subredditsort_key,
				"name")));
	}

	public enum BlockedSubredditSort {
		NAME, DATE
	}

	public static BlockedSubredditSort pref_behaviour_blocked_subredditsort() {
		return BlockedSubredditSort.valueOf(StringUtils.asciiUppercase(getString(
				R.string.pref_behaviour_blocked_subredditsort_key,
				"name")));
	}

	public static boolean pref_behaviour_nsfw() {
		return getBoolean(
				R.string.pref_behaviour_nsfw_key,
				false);
	}

	//Show Visited Posts? True hides them.
	// See strings.xml, prefs_behaviour.xml, PostListingFragment.java
	public static boolean pref_behaviour_hide_read_posts() {
		return getBoolean(
				R.string.pref_behaviour_hide_read_posts_key,
				false);
	}

	public enum SharingDomain {
		STANDARD_REDDIT("reddit.com"),
		SHORT_REDDIT("redd.it"),
		OLD_REDDIT("old.reddit.com"),
		NEW_REDDIT("new.reddit.com"),
		NP_REDDIT("np.reddit.com");

		public final String domain;
		SharingDomain(final String domain) {
			this.domain = domain;
		}
	}

	public static SharingDomain pref_behaviour_sharing_domain() {
		return SharingDomain.valueOf(StringUtils.asciiUppercase(getString(
				R.string.pref_behaviour_sharing_domain_key,
				"standard_reddit")));
	}
	public static boolean pref_behaviour_share_permalink() {
		return getBoolean(
				R.string.pref_behaviour_share_permalink_key,
				false);
	}

	public enum PostCount {
		R25, R50, R100, ALL
	}

	public static PostCount pref_behaviour_post_count() {
		return PostCount.valueOf(getString(
				R.string.pref_behaviour_postcount_key,
				"ALL"));
	}

	public enum ScreenOrientation {
		AUTO, PORTRAIT, LANDSCAPE
	}

	public static ScreenOrientation pref_behaviour_screen_orientation() {
		return ScreenOrientation.valueOf(StringUtils.asciiUppercase(getString(
				R.string.pref_behaviour_screenorientation_key,
				StringUtils.asciiLowercase(ScreenOrientation.AUTO.name()))));
	}

	public enum SaveLocation {
		PROMPT_EVERY_TIME, SYSTEM_DEFAULT
	}

	public static SaveLocation pref_behaviour_save_location() {
		return SaveLocation.valueOf(StringUtils.asciiUppercase(getString(
				R.string.pref_behaviour_save_location_key,
				StringUtils.asciiLowercase(SaveLocation.PROMPT_EVERY_TIME.name()))));
	}

	public static boolean behaviour_block_screenshots() {
		return getBoolean(
				R.string.pref_behaviour_block_screenshots_key,
				false);
	}

	///////////////////////////////
	// pref_cache
	///////////////////////////////

	// pref_cache_location

	public static String pref_cache_location(
			final Context context) {
		File defaultCacheDir = context.getExternalCacheDir();
		if(defaultCacheDir == null) {
			defaultCacheDir = context.getCacheDir();
		}
		return getString(R.string.pref_cache_location_key,
				defaultCacheDir.getAbsolutePath());
	}

	public static void pref_cache_location(
			final Context context,
			final String path) {
		sharedPrefs.edit()
				.putString(context.getString(R.string.pref_cache_location_key), path)
				.apply();
	}

	public static TimeDuration pref_cache_rerequest_postlist_age() {
		try {
			final int hours = Integer.parseInt(
					getString(
							R.string.pref_cache_rerequest_postlist_age_key,
							"1"));

			return TimeDuration.hours(hours);

		} catch(final Throwable e) {
			return TimeDuration.hours(1);
		}
	}

	// pref_cache_maxage

	public static <E> HashMap<Integer, E> createFileTypeMap(
			final E listings,
			final E thumbnails,
			final E images) {
		final HashMap<Integer, E> maxAgeMap = new HashMap<>(10);

		maxAgeMap.put(Constants.FileType.POST_LIST, listings);
		maxAgeMap.put(Constants.FileType.COMMENT_LIST, listings);
		maxAgeMap.put(Constants.FileType.SUBREDDIT_LIST, listings);
		maxAgeMap.put(Constants.FileType.SUBREDDIT_ABOUT, listings);
		maxAgeMap.put(Constants.FileType.USER_ABOUT, listings);
		maxAgeMap.put(Constants.FileType.INBOX_LIST, listings);
		maxAgeMap.put(Constants.FileType.THUMBNAIL, thumbnails);
		maxAgeMap.put(Constants.FileType.IMAGE, images);
		maxAgeMap.put(Constants.FileType.IMAGE_INFO, images);
		maxAgeMap.put(Constants.FileType.CAPTCHA, images);
		maxAgeMap.put(Constants.FileType.INLINE_IMAGE_PREVIEW, images);

		return maxAgeMap;
	}

	public static HashMap<Integer, TimeDuration> pref_cache_maxage() {

		final TimeDuration maxAgeListing
				= TimeDuration.hours(Long.parseLong(getString(
				R.string.pref_cache_maxage_listing_key,
				"168")));

		final TimeDuration maxAgeThumb
				= TimeDuration.hours(Long.parseLong(getString(
				R.string.pref_cache_maxage_thumb_key,
				"168")));

		final TimeDuration maxAgeImage
				= TimeDuration.hours(Long.parseLong(getString(
				R.string.pref_cache_maxage_image_key,
				"72")));

		return createFileTypeMap(maxAgeListing, maxAgeThumb, maxAgeImage);
	}

	public static TimeDuration pref_cache_maxage_entry() {
		return TimeDuration.hours(Long.parseLong(getString(
				R.string.pref_cache_maxage_entry_key,
				"168")));
	}

	// pref_cache_precache_images

	public static NeverAlwaysOrWifiOnly cache_precache_images() {
		return NeverAlwaysOrWifiOnly.valueOf(StringUtils.asciiUppercase(
				getString(
						R.string.pref_cache_precache_images_list_key,
						"wifionly")));
	}

	public static NeverAlwaysOrWifiOnly cache_precache_images_old() {

		if(network_tor()) {
			return NeverAlwaysOrWifiOnly.NEVER;
		}

		if(!getBoolean(
				R.string.pref_cache_precache_images_key,
				true)) {
			return NeverAlwaysOrWifiOnly.NEVER;
		} else if(getBoolean(
				R.string.pref_cache_precache_images_wifionly_key,
				true)) {
			return NeverAlwaysOrWifiOnly.WIFIONLY;
		} else {
			return NeverAlwaysOrWifiOnly.ALWAYS;
		}
	}

	// pref_cache_precache_comments

	public static NeverAlwaysOrWifiOnly cache_precache_comments() {
		return NeverAlwaysOrWifiOnly.valueOf(StringUtils.asciiUppercase(
				getString(
						R.string.pref_cache_precache_comments_list_key,
						"always")));
	}

	public static NeverAlwaysOrWifiOnly cache_precache_comments_old() {

		if(!getBoolean(
				R.string.pref_cache_precache_comments_key,
				true)) {
			return NeverAlwaysOrWifiOnly.NEVER;
		} else if(getBoolean(
				R.string.pref_cache_precache_comments_wifionly_key,
				false)) {
			return NeverAlwaysOrWifiOnly.WIFIONLY;
		} else {
			return NeverAlwaysOrWifiOnly.ALWAYS;
		}
	}

	///////////////////////////////
	// pref_network
	///////////////////////////////

	public static boolean network_tor() {
		return getBoolean(
				R.string.pref_network_tor_key,
				false);
	}

	///////////////////////////////
	// pref_menus
	///////////////////////////////

	public static EnumSet<RedditPostActions.Action> pref_menus_post_context_items() {

		final Set<String> strings = getStringSet(
				R.string.pref_menus_post_context_items_key,
				R.array.pref_menus_post_context_items_return);

		final EnumSet<RedditPostActions.Action> result = EnumSet.noneOf(
				RedditPostActions.Action.class);
		for(final String s : strings) {
			result.add(RedditPostActions.Action.valueOf(StringUtils.asciiUppercase(s)));
		}

		return result;
	}

	public static EnumSet<RedditPostActions.Action> pref_menus_post_toolbar_items() {

		final Set<String> strings = getStringSet(
				R.string.pref_menus_post_toolbar_items_key,
				R.array.pref_menus_post_toolbar_items_return);

		final EnumSet<RedditPostActions.Action> result = EnumSet.noneOf(
				RedditPostActions.Action.class);
		for(final String s : strings) {
			result.add(RedditPostActions.Action.valueOf(StringUtils.asciiUppercase(s)));
		}

		return result;
	}

	public static EnumSet<LinkHandler.LinkAction> pref_menus_link_context_items() {

		final Set<String> strings = getStringSet(
				R.string.pref_menus_link_context_items_key,
				R.array.pref_menus_link_context_items_return);

		final EnumSet<LinkHandler.LinkAction> result
				= EnumSet.noneOf(LinkHandler.LinkAction.class);
		for(final String s : strings) {
			result.add(LinkHandler.LinkAction.valueOf(StringUtils.asciiUppercase(s)));
		}

		return result;
	}

	public static EnumSet<MainMenuListingManager.SubredditAction>
	pref_menus_subreddit_context_items() {

		final Set<String> strings = getStringSet(
				R.string.pref_menus_subreddit_context_items_key,
				R.array.pref_menus_subreddit_context_items_return);

		final EnumSet<MainMenuListingManager.SubredditAction> result = EnumSet.noneOf(
				MainMenuListingManager.SubredditAction.class);
		for(final String s : strings) {
			result.add(MainMenuListingManager.SubredditAction.valueOf(StringUtils.asciiUppercase(
					s)));
		}

		return result;
	}

	public static EnumSet<MainMenuFragment.MainMenuUserItems> pref_menus_mainmenu_useritems() {

		final Set<String> strings = getStringSet(
				R.string.pref_menus_mainmenu_useritems_key,
				R.array.pref_menus_mainmenu_useritems_items_default);

		final EnumSet<MainMenuFragment.MainMenuUserItems> result = EnumSet.noneOf(
				MainMenuFragment.MainMenuUserItems.class);
		for(final String s : strings) {
			result.add(MainMenuFragment.MainMenuUserItems.valueOf(StringUtils.asciiUppercase(s)));
		}

		return result;
	}

	public static EnumSet<MainMenuFragment.MainMenuShortcutItems>
	pref_menus_mainmenu_shortcutitems() {

		final Set<String> strings = getStringSet(
				R.string.pref_menus_mainmenu_shortcutitems_key,
				R.array.pref_menus_mainmenu_shortcutitems_items_default);

		final EnumSet<MainMenuFragment.MainMenuShortcutItems> result = EnumSet.noneOf(
				MainMenuFragment.MainMenuShortcutItems.class);
		for(final String s : strings) {
			result.add(MainMenuFragment.MainMenuShortcutItems.valueOf(
					StringUtils.asciiUppercase(s)));
		}

		return result;
	}

	private static class AppbarItemInfo {
		final OptionsMenuUtility.AppbarItemsPref itemPref;
		final int stringRes;
		final int defaultValue;

		AppbarItemInfo(
				final OptionsMenuUtility.AppbarItemsPref itemPref,
				final int stringRes,
				final int defaultValue) {
			this.itemPref = itemPref;
			this.stringRes = stringRes;
			this.defaultValue = defaultValue;
		}
	}

	public static EnumMap<OptionsMenuUtility.AppbarItemsPref, Integer> pref_menus_appbar_items() {

		final AppbarItemInfo[] appbarItemsInfo = {
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
						Integer.toString(item.defaultValue))));
			} catch(final NumberFormatException | NullPointerException e) {
				appbarItemsPrefs.put(item.itemPref, item.defaultValue);
			}
		}

		return appbarItemsPrefs;
	}

	public static boolean pref_menus_quick_account_switcher() {
		return getBoolean(
				R.string.pref_menus_quick_account_switcher_key,
				true);
	}

	public static EnumSet<RedditAPICommentAction.RedditCommentAction>
				pref_menus_comment_context_items() {

		final Set<String> strings = getStringSet(
				R.string.pref_menus_comment_context_items_key,
				R.array.pref_menus_comment_context_items_return);

		final EnumSet<RedditAPICommentAction.RedditCommentAction> result = EnumSet.noneOf(
				RedditAPICommentAction.RedditCommentAction.class);

		for(final String s : strings) {
			result.add(RedditAPICommentAction.RedditCommentAction.valueOf(
					StringUtils.asciiUppercase(s)));
		}

		return result;
	}

	///////////////////////////////
	// pref_pinned_subreddits
	///////////////////////////////

	public static List<SubredditCanonicalId> pref_pinned_subreddits() {
		return pref_subreddits_list(R.string.pref_pinned_subreddits_key);
	}

	public static void pref_pinned_subreddits_add(
			final Context context,
			final SubredditCanonicalId subreddit) {

		pref_subreddits_add(
				context,
				subreddit,
				R.string.pref_pinned_subreddits_key);

		General.quickToast(context, context.getApplicationContext().getString(
				R.string.pin_successful,
				subreddit.toString()));
	}

	public static void pref_pinned_subreddits_remove(
			final Context context,
			final SubredditCanonicalId subreddit) {

		pref_subreddits_remove(
				context,
				subreddit,
				R.string.pref_pinned_subreddits_key);

		General.quickToast(context, context.getApplicationContext().getString(
				R.string.unpin_successful,
				subreddit.toString()));
	}

	public static boolean pref_pinned_subreddits_check(final SubredditCanonicalId id) {

		return pref_pinned_subreddits().contains(id);
	}

	///////////////////////////////
	// pref_blocked_subreddits
	///////////////////////////////

	public static List<SubredditCanonicalId> pref_blocked_subreddits() {

		return pref_subreddits_list(R.string.pref_blocked_subreddits_key);
	}

	public static void pref_blocked_subreddits_add(
			final Context context,
			final SubredditCanonicalId subreddit) {

		pref_subreddits_add(
				context,
				subreddit,
				R.string.pref_blocked_subreddits_key);

		General.quickToast(context, R.string.block_done);
	}

	public static void pref_blocked_subreddits_remove(
			final Context context,
			final SubredditCanonicalId subreddit) {

		pref_subreddits_remove(
				context,
				subreddit,
				R.string.pref_blocked_subreddits_key);

		General.quickToast(context, R.string.unblock_done);
	}

	public static boolean pref_blocked_subreddits_check(final SubredditCanonicalId subreddit) {

		return pref_blocked_subreddits().contains(subreddit);
	}

	///////////////////////////////
	// Shared pref_subreddits methods
	///////////////////////////////

	private static void pref_subreddits_add(
			final Context context,
			final SubredditCanonicalId subreddit,
			final int prefId) {

		final String value = getString(prefId, "");
		final ArrayList<String> list = WritableHashSet.escapedStringToList(value);

		if(!list.contains(subreddit.toString())) {
			list.add(subreddit.toString());
			final String result = WritableHashSet.listToEscapedString(list);
			sharedPrefs.edit().putString(context.getString(prefId), result).apply();
		}
	}

	private static void pref_subreddits_remove(
			final Context context,
			final SubredditCanonicalId subreddit,
			final int prefId) {

		final String value = getString(prefId, "");
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

		sharedPrefs.edit().putString(context.getString(prefId), resultStr).apply();
	}

	public static List<SubredditCanonicalId> pref_subreddits_list(final int prefId) {

		final String value = getString(prefId, "");
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

	public static boolean pref_accessibility_separate_body_text_lines() {
		return getBoolean(
				R.string.pref_accessibility_separate_body_text_lines_key,
				true);
	}

	public static int pref_accessibility_min_comment_height() {
		try {
			return Integer.parseInt(getString(
					R.string.pref_accessibility_min_comment_height_key,
					"0"));
		} catch(final Throwable e) {
			return 0;
		}
	}

	public static boolean pref_accessibility_say_comment_indent_level() {
		return getBoolean(
				R.string.pref_accessibility_say_comment_indent_level_key,
				true);
	}

	public enum BehaviourCollapseStickyComments {
		ALWAYS, ONLY_BOTS, NEVER
	}

	public static BehaviourCollapseStickyComments behaviour_collapse_sticky_comments() {
		return BehaviourCollapseStickyComments.valueOf(StringUtils.asciiUppercase(getString(
				R.string.pref_behaviour_collapse_sticky_comments_key,
				"ONLY_BOTS")));
	}

	public static boolean pref_accessibility_concise_mode() {
		return getBoolean(
				R.string.pref_accessibility_concise_mode_key,
				false);
	}

	public static boolean pref_behaviour_keep_screen_awake() {
		return getBoolean(
				R.string.pref_behaviour_keep_screen_awake_key,
				false);
	}

	@Nullable
	public static String pref_reddit_client_id_override() {
		final String value = getString(R.string.pref_reddit_client_id_override_key, null);

		if (value == null) {
			return null;
		}

		final String valueTrimmed = value.trim();

		if (valueTrimmed.isEmpty()) {
			return null;
		}

		return valueTrimmed;
	}

	private static final String REDDIT_USER_AGREEMENT_PREF = "accepted_reddit_user_agreement";
	private static final int REDDIT_USER_AGREEMENT_DECLINED = -1;
	private static final int REDDIT_USER_AGREEMENT_APRIL_2023 = 1;
	private static final int REDDIT_USER_AGREEMENT_CURRENT = REDDIT_USER_AGREEMENT_APRIL_2023;

	public static boolean isRedditUserAgreementAccepted() {
		return sharedPrefs.getInt(REDDIT_USER_AGREEMENT_PREF, 0)
				>= REDDIT_USER_AGREEMENT_CURRENT;
	}

	public static boolean isRedditUserAgreementDeclined() {
		return sharedPrefs.getInt(REDDIT_USER_AGREEMENT_PREF, 0)
				== REDDIT_USER_AGREEMENT_DECLINED;
	}

	public static void acceptRedditUserAgreement() {
		sharedPrefs
				.edit()
				.putInt(REDDIT_USER_AGREEMENT_PREF, REDDIT_USER_AGREEMENT_CURRENT)
				.apply();
	}

	public static void declineRedditUserAgreement() {
		sharedPrefs
				.edit()
				.putInt(REDDIT_USER_AGREEMENT_PREF, REDDIT_USER_AGREEMENT_DECLINED)
				.apply();
	}
}
