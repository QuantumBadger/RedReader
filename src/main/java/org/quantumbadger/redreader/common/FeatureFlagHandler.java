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

import android.app.AlertDialog;
import android.content.SharedPreferences;
import android.util.Log;
import androidx.annotation.NonNull;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.activities.BaseActivity;
import org.quantumbadger.redreader.cache.CacheManager;
import org.quantumbadger.redreader.fragments.AccountListDialog;
import org.quantumbadger.redreader.fragments.ChangelogDialog;

import java.util.Set;

public final class FeatureFlagHandler {

	private static final String TAG = "FeatureFlagHandler";

	private FeatureFlagHandler() {}

	private enum FeatureFlagStatus {
		ALREADY_UPGRADED, UPGRADE_NEEDED
	}

	private enum FeatureFlag {

		COMMENT_HEADER_SUBREDDIT_FEATURE("commentHeaderSubredditFeature");

		@NonNull private final String id;

		FeatureFlag(@NonNull final String id) {
			this.id = id;
		}

		@NonNull
		public final String getId() {
			return "rr_feature_flag_" + id;
		}
	}

	public static void handleUpgrade(@NonNull final BaseActivity context) {

		// getAndSetFeatureFlag() will return UPGRADE_NEEDED if the app has been
		// upgraded from a version which did not support the specified feature.
		// It will return ALREADY_UPGRADED if the feature was already present
		// in the last version, or if this is a fresh install of the app.

		final SharedPreferences sharedPrefs = General.getSharedPrefs(context);

		if(getAndSetFeatureFlag(sharedPrefs, FeatureFlag.COMMENT_HEADER_SUBREDDIT_FEATURE)
				== FeatureFlagStatus.UPGRADE_NEEDED) {

			Log.i(TAG, "Upgrading, show comment subreddit in header by default");

			final Set<String> existingCommentHeaderItems
					= PrefsUtility.getStringSet(
					R.string.pref_appearance_comment_header_items_key,
					R.array.pref_appearance_comment_header_items_default,
					context,
					sharedPrefs
			);

			existingCommentHeaderItems.add("subreddit");

			sharedPrefs.edit().putStringSet(
					context.getString(R.string.pref_appearance_comment_header_items_key),
					existingCommentHeaderItems
			).apply();
		}
	}

	private static void setFeatureFlag(
			@NonNull final SharedPreferences sharedPreferences,
			@NonNull final FeatureFlag featureFlag) {

		sharedPreferences.edit().putBoolean(featureFlag.getId(), true).apply();
	}

	@NonNull
	private static FeatureFlagStatus getAndSetFeatureFlag(
			@NonNull final SharedPreferences sharedPreferences,
			@NonNull final FeatureFlag featureFlag) {

		final String name = "rr_feature_flag_" + featureFlag.id;

		final boolean current = sharedPreferences.getBoolean(name, false);

		if(!current) {
			sharedPreferences.edit().putBoolean(name, true).apply();
		}

		return current ? FeatureFlagStatus.ALREADY_UPGRADED : FeatureFlagStatus.UPGRADE_NEEDED;
	}

	public static void handleFirstInstall(@NonNull final SharedPreferences sharedPrefs) {

		// Set all feature flags when first installing

		for(final FeatureFlag flag : FeatureFlag.values()) {
			setFeatureFlag(sharedPrefs, flag);
		}
	}


	public static void handleLegacyUpgrade(
			@NonNull final BaseActivity activity,
			final int appVersion,
			@NonNull final String versionName) {

		final SharedPreferences sharedPreferences = General.getSharedPrefs(activity);

		final int lastVersion = sharedPreferences.getInt("lastVersion", 0);

		Log.i(TAG, "[Migration] Last version: " + lastVersion);

		if(lastVersion < 63) {
			// Upgrading across the 1.9.0 boundary (when oAuth was introduced)

			new AlertDialog.Builder(activity)
					.setTitle(R.string.firstrun_login_title)
					.setMessage(R.string.upgrade_v190_login_message)
					.setPositiveButton(
							R.string.firstrun_login_button_now,
							(dialog, which) -> new AccountListDialog().show(
									activity.getSupportFragmentManager(),
									null))
					.setNegativeButton(R.string.firstrun_login_button_later, null)
					.show();
		}

		if(lastVersion != appVersion) {

			General.quickToast(
					activity,
					String.format(
							activity.getString(R.string.upgrade_message),
							versionName));

			sharedPreferences.edit().putInt("lastVersion", appVersion).apply();
			ChangelogDialog.newInstance().show(activity.getSupportFragmentManager(), null);

			if(lastVersion <= 51) {
				// Upgrading from v1.8.6.3 or lower

				final Set<String> existingCommentHeaderItems
						= PrefsUtility.getStringSet(
								R.string.pref_appearance_comment_header_items_key,
								R.array.pref_appearance_comment_header_items_default,
								activity,
								sharedPreferences);

				existingCommentHeaderItems.add("gold");

				sharedPreferences.edit().putStringSet(
						activity.getString(R.string.pref_appearance_comment_header_items_key),
						existingCommentHeaderItems).apply();

				General.startNewThread(
						"EmptyCache",
						() -> CacheManager.getInstance(activity).emptyTheWholeCache());
			}

			if(lastVersion <= 76) {
				// Upgrading from v1.9.6.1 or lower, enable image sharing from post context menu

				final Set<String> existingPostContextItems
						= PrefsUtility.getStringSet(
						R.string.pref_menus_post_context_items_key,
						R.array.pref_menus_post_context_items_return,
						activity,
						sharedPreferences
				);

				existingPostContextItems.add("share_image");

				sharedPreferences.edit().putStringSet(
						activity.getString(R.string.pref_menus_post_context_items_key),
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
						activity,
						sharedPreferences
				);

				existingPostContextItems.add("edit");
				existingPostContextItems.add("pin");
				existingPostContextItems.add("subscribe");
				existingPostContextItems.add("block");

				sharedPreferences.edit().putStringSet(
						activity.getString(R.string.pref_menus_post_context_items_key),
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
						activity,
						sharedPreferences
				);

				if(PrefsUtility.pref_show_popular_main_menu(
						activity,
						sharedPreferences
				)) {
					existingShortcutPreferences.add("popular");
				}


				if(PrefsUtility.pref_show_random_main_menu(
						activity,
						sharedPreferences
				)) {
					existingShortcutPreferences.add("random");
				}

				sharedPreferences.edit().putStringSet(
						activity.getString(R.string.pref_menus_mainmenu_shortcutitems_key),
						existingShortcutPreferences).apply();
			}

			if(lastVersion <= 87) {
				// + Context menu of post header will now appear also on
				// post self-text long click
				// + "Copy Self-Text" context menu item added

				final Set<String> existingPostContextItems
						= PrefsUtility.getStringSet(
						R.string.pref_menus_post_context_items_key,
						R.array.pref_menus_post_context_items_return,
						activity,
						sharedPreferences);

				existingPostContextItems.add("copy_selftext");

				sharedPreferences.edit().putStringSet(
						activity.getString(R.string.pref_menus_post_context_items_key),
						existingPostContextItems).apply();
			}

			if(lastVersion <= 89) {
				//Upgrading from 89/1.9.11 or lower, enable finer control over font scales
				//and set them to match the existing settings
				//The old Inbox Font Scale setting is ignored

				Log.i(TAG, "[Migration] Upgrading from v89");

				final String existingPostFontscalePreference = PrefsUtility.getString(
						R.string.pref_appearance_fontscale_posts_key,
						"-1",
						activity,
						sharedPreferences);

				final String existingCommentSelfTextFontscalePreference
						= PrefsUtility.getString(
								R.string.pref_appearance_fontscale_bodytext_key,
								"-1",
								activity,
								sharedPreferences);

				if(existingPostFontscalePreference.equals(
						existingCommentSelfTextFontscalePreference)) {

					Log.i(TAG, "[Migration] Old font preferences were both "
							+ existingPostFontscalePreference);

					// Avoid setting the global font scale to -1
					if(!existingPostFontscalePreference.equals("-1")) {

						Log.i(TAG, "[Migration] Migrating font preferences");

						sharedPreferences.edit().putString(
								activity.getString(R.string.pref_appearance_fontscale_global_key),
								existingPostFontscalePreference).apply();

						sharedPreferences.edit().putString(
								activity.getString(R.string.pref_appearance_fontscale_posts_key),
								"-1").apply();

						sharedPreferences.edit().putString(
								activity.getString(R.string.pref_appearance_fontscale_bodytext_key),
								"-1").apply();
					}

				} else {

					Log.i(TAG, "[Migration] Old font prefs: comments="
							+ existingCommentSelfTextFontscalePreference
							+ ", posts="
							+ existingPostFontscalePreference
							+ ". Migrating.");

					sharedPreferences.edit().putString(
							activity.getString(
									R.string.pref_appearance_fontscale_post_subtitles_key),
							existingPostFontscalePreference).apply();

					sharedPreferences.edit().putString(
							activity.getString(
									R.string.pref_appearance_fontscale_post_header_titles_key),
							existingPostFontscalePreference).apply();

					sharedPreferences.edit().putString(
							activity.getString(
									R.string.pref_appearance_fontscale_post_header_subtitles_key),
							existingPostFontscalePreference).apply();

					sharedPreferences.edit().putString(
							activity.getString(
									R.string.pref_appearance_fontscale_comment_headers_key),
							existingCommentSelfTextFontscalePreference).apply();

					sharedPreferences.edit().putString(
							activity.getString(R.string.pref_appearance_fontscale_linkbuttons_key),
							existingCommentSelfTextFontscalePreference).apply();
				}

				//Upgrading from 89/1.9.11 or lower, switch to ListPreference for
				//appearance_thumbnails_show, cache_precache_images, cache_precache_comments

				final String existingThumbnailsShowPreference = StringUtils.asciiLowercase(
						PrefsUtility.appearance_thumbnails_show_old(
								activity,
								sharedPreferences).toString());

				final String existingPrecacheImagesPreference = StringUtils.asciiLowercase(
						PrefsUtility.cache_precache_images_old(
								activity,
								sharedPreferences).toString());

				final String existingPrecacheCommentsPreference = StringUtils.asciiLowercase(
						PrefsUtility.cache_precache_comments_old(
								activity,
								sharedPreferences).toString());

				sharedPreferences.edit().putString(
						activity.getString(R.string.pref_appearance_thumbnails_show_list_key),
						existingThumbnailsShowPreference).apply();

				sharedPreferences.edit().putString(
						activity.getString(R.string.pref_cache_precache_images_list_key),
						existingPrecacheImagesPreference).apply();

				sharedPreferences.edit().putString(
						activity.getString(R.string.pref_cache_precache_comments_list_key),
						existingPrecacheCommentsPreference).apply();
			}

			if(lastVersion <= 92) {
				// Upgrading from 92/1.12 or lower

				// Switch to individual ListPreference's for
				// pref_menus_appbar (formerly pref_menus_optionsmenu_items)

				final Set<String> existingOptionsMenuItems
						= PrefsUtility.getStringSet(
						R.string.pref_menus_optionsmenu_items_key,
						R.array.pref_menus_optionsmenu_items_items_return,
						activity,
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
							activity.getString(item.stringRes),
							showAsAction).apply();
				}

			}
		}
	}
}
