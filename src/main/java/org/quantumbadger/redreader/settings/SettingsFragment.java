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

package org.quantumbadger.redreader.settings;

import android.app.AlertDialog;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.SharedPreferences;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.os.Build;
import android.os.Bundle;
import android.preference.EditTextPreference;
import android.preference.ListPreference;
import android.preference.Preference;
import android.preference.PreferenceFragment;
import android.preference.PreferenceManager;
import android.text.Html;
import org.quantumbadger.redreader.BuildConfig;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.activities.ChangelogActivity;
import org.quantumbadger.redreader.activities.HtmlViewActivity;
import org.quantumbadger.redreader.cache.CacheManager;
import org.quantumbadger.redreader.common.AndroidCommon;
import org.quantumbadger.redreader.common.FileUtils;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.common.TorCommon;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

public final class SettingsFragment extends PreferenceFragment {

	@Override
	public void onCreate(final Bundle savedInstanceState) {

		super.onCreate(savedInstanceState);

		final Context context = getActivity();

		final String panel = getArguments().getString("panel");
		final int resource;

		try {
			resource = R.xml.class.getDeclaredField("prefs_" + panel).getInt(null);
		} catch(final IllegalAccessException e) {
			throw new RuntimeException(e);
		} catch(final NoSuchFieldException e) {
			throw new RuntimeException(e);
		}

		addPreferencesFromResource(resource);

		final int[] listPrefsToUpdate = {
				R.string.pref_appearance_twopane_key,
				R.string.pref_behaviour_self_post_tap_actions_key,
				R.string.pref_behaviour_fling_post_left_key,
				R.string.pref_behaviour_fling_post_right_key,
				R.string.pref_behaviour_fling_comment_left_key,
				R.string.pref_behaviour_fling_comment_right_key,
				R.string.pref_appearance_theme_key,
				R.string.pref_appearance_navbar_color_key,
				R.string.pref_cache_maxage_listing_key,
				R.string.pref_cache_maxage_thumb_key,
				R.string.pref_cache_maxage_image_key,
				R.string.pref_cache_maxage_entry_key,
				R.string.pref_appearance_fontscale_global_key,
				R.string.pref_appearance_fontscale_posts_key,
				R.string.pref_appearance_fontscale_post_subtitles_key,
				R.string.pref_appearance_fontscale_post_header_titles_key,
				R.string.pref_appearance_fontscale_post_header_subtitles_key,
				R.string.pref_appearance_fontscale_comment_headers_key,
				R.string.pref_appearance_fontscale_bodytext_key,
				R.string.pref_appearance_fontscale_linkbuttons_key,
				R.string.pref_behaviour_actions_comment_tap_key,
				R.string.pref_behaviour_actions_comment_longclick_key,
				R.string.pref_behaviour_commentsort_key,
				R.string.pref_behaviour_user_commentsort_key,
				R.string.pref_behaviour_postsort_key,
				R.string.pref_behaviour_user_postsort_key,
				R.string.pref_behaviour_multi_postsort_key,
				R.string.pref_appearance_langforce_key,
				R.string.pref_behaviour_postcount_key,
				R.string.pref_behaviour_bezel_toolbar_swipezone_key,
				R.string.pref_behaviour_imageview_mode_key,
				R.string.pref_behaviour_albumview_mode_key,
				R.string.pref_behaviour_gifview_mode_key,
				R.string.pref_behaviour_videoview_mode_key,
				R.string.pref_behaviour_screenorientation_key,
				R.string.pref_behaviour_gallery_swipe_length_key,
				R.string.pref_behaviour_pinned_subredditsort_key,
				R.string.pref_behaviour_blocked_subredditsort_key,
				R.string.pref_cache_rerequest_postlist_age_key,
				R.string.pref_appearance_thumbnails_show_list_key,
				R.string.pref_cache_precache_images_list_key,
				R.string.pref_cache_precache_comments_list_key,
				R.string.pref_menus_appbar_sort_key,
				R.string.pref_menus_appbar_refresh_key,
				R.string.pref_menus_appbar_past_key,
				R.string.pref_menus_appbar_submit_post_key,
				R.string.pref_menus_appbar_pin_key,
				R.string.pref_menus_appbar_block_key,
				R.string.pref_menus_appbar_subscribe_key,
				R.string.pref_menus_appbar_sidebar_key,
				R.string.pref_menus_appbar_accounts_key,
				R.string.pref_menus_appbar_theme_key,
				R.string.pref_menus_appbar_settings_key,
				R.string.pref_menus_appbar_close_all_key,
				R.string.pref_menus_appbar_reply_key,
				R.string.pref_menus_appbar_search_key
		};

		final int[] editTextPrefsToUpdate = {
				R.string.pref_behaviour_comment_min_key
		};

		for(final int pref : listPrefsToUpdate) {

			final ListPreference listPreference =
					(ListPreference)findPreference(getString(pref));

			if(listPreference == null) {
				continue;
			}

			final int index = listPreference.findIndexOfValue(listPreference.getValue());
			if(index < 0) {
				continue;
			}

			listPreference.setSummary(listPreference.getEntries()[index]);

			listPreference.setOnPreferenceChangeListener((preference, newValue) -> {
				final int index1 = listPreference.findIndexOfValue((String)newValue);
				listPreference.setSummary(listPreference.getEntries()[index1]);
				return true;
			});
		}

		for(final int pref : editTextPrefsToUpdate) {

			final EditTextPreference editTextPreference =
					(EditTextPreference)findPreference(getString(pref));

			if(editTextPreference == null) {
				continue;
			}

			editTextPreference.setSummary(editTextPreference.getText());

			editTextPreference.setOnPreferenceChangeListener((preference, newValue) -> {
				if(newValue != null) {
					editTextPreference.setSummary(newValue.toString());
				} else {
					editTextPreference.setSummary("(null)");
				}
				return true;
			});
		}


		final Preference versionPref =
				findPreference(getString(R.string.pref_about_version_key));
		final Preference changelogPref =
				findPreference(getString(R.string.pref_about_changelog_key));
		final Preference torPref =
				findPreference(getString(R.string.pref_network_tor_key));
		final Preference licensePref =
				findPreference(getString(R.string.pref_about_license_key));

		final PackageInfo pInfo;

		try {
			pInfo = context.getPackageManager()
					.getPackageInfo(context.getPackageName(), 0);
		} catch(final PackageManager.NameNotFoundException e) {
			throw new RuntimeException(e);
		}

		if(versionPref != null) {
			versionPref.setSummary(pInfo.versionName);
		}

		if(changelogPref != null) {
			changelogPref.setOnPreferenceClickListener(preference -> {
				final Intent intent = new Intent(context, ChangelogActivity.class);
				context.startActivity(intent);
				return true;
			});
		}

		if(licensePref != null) {
			licensePref.setOnPreferenceClickListener(preference -> {
				HtmlViewActivity.showAsset(context, "license.html");
				return true;
			});
		}

		if(torPref != null) {
			torPref.setOnPreferenceChangeListener((preference, newValue) -> {

				// Run this after the preference has actually changed
				AndroidCommon.UI_THREAD_HANDLER.post(new Runnable() {
					@Override
					public void run() {
						TorCommon.updateTorStatus(context);
						if(TorCommon.isTorEnabled()
								!= Boolean.TRUE.equals(newValue)) {
							throw new RuntimeException(
									"Tor not correctly enabled after preference change");
						}
					}
				});

				return true;
			});
		}

		if(Build.VERSION.SDK_INT < Build.VERSION_CODES.LOLLIPOP) {
			final Preference pref =
					findPreference(getString(R.string.pref_appearance_navbar_color_key));

			if(pref != null) {
				pref.setEnabled(false);
				pref.setSummary(R.string.pref_not_supported_before_lollipop);
			}
		}

		final Preference cacheLocationPref =
				findPreference(getString(R.string.pref_cache_location_key));
		if(cacheLocationPref != null) {
			cacheLocationPref.setOnPreferenceClickListener(preference -> {
				showChooseStorageLocationDialog();
				return true;
			});
			updateStorageLocationText(PrefsUtility.pref_cache_location(
					context,
					PreferenceManager.getDefaultSharedPreferences(context)));
		}

		//This disables the "Show NSFW thumbnails" setting when Show thumbnails is set to Never
		//Based off https://stackoverflow.com/a/4137963
		final ListPreference thumbnailPref = (ListPreference)findPreference(
				getString(R.string.pref_appearance_thumbnails_show_list_key));
		final Preference thumbnailNsfwPref =
				findPreference(getString(R.string.pref_appearance_thumbnails_nsfw_show_key));

		if(thumbnailPref != null) {
			thumbnailPref.setOnPreferenceChangeListener((preference, newValue) -> {
				final int index = thumbnailPref.findIndexOfValue((String)newValue);
				thumbnailPref.setSummary(thumbnailPref.getEntries()[index]);

				if(newValue.equals("never")) {
					thumbnailNsfwPref.setEnabled(false);
				} else {
					thumbnailNsfwPref.setEnabled(true);
				}

				return true;
			});
		}
	}

	private void showChooseStorageLocationDialog() {
		final Context context = getActivity();
		final SharedPreferences prefs =
				PreferenceManager.getDefaultSharedPreferences(context);
		final String currentStorage = PrefsUtility.pref_cache_location(context, prefs);

		final List<File> checkPaths = CacheManager.getCacheDirs(context);

		final List<File> folders = new ArrayList<>(checkPaths.size());

		final List<CharSequence> choices = new ArrayList<>(checkPaths.size());
		int selectedIndex = 0;

		for(int i = 0; i < checkPaths.size(); i++) {
			final File dir = checkPaths.get(i);
			if(dir == null || !dir.exists() || !dir.canRead() || !dir.canWrite()) {
				continue;
			}
			folders.add(dir);
			if(currentStorage.equals(dir.getAbsolutePath())) {
				selectedIndex = i;
			}

			String path = dir.getAbsolutePath();
			final long bytes = FileUtils.getFreeSpaceAvailable(path);
			final String freeSpace = General.addUnits(bytes);
			if(!path.endsWith("/")) {
				path += "/";
			}
			final String appCachePostfix = BuildConfig.APPLICATION_ID + "/cache/";
			if(path.endsWith("Android/data/" + appCachePostfix)) {
				path = path.substring(0, path.length() - appCachePostfix.length() - 14);
			} else if(path.endsWith(appCachePostfix)) {
				path = path.substring(0, path.length() - appCachePostfix.length() - 1);
			}
			choices.add(Html.fromHtml("<small>" + path +
					" [" + freeSpace + "]</small>"));
		}
		new AlertDialog.Builder(context)
				.setTitle(R.string.pref_cache_location_title)
				.setSingleChoiceItems(choices.toArray(new CharSequence[choices.size()]),
						selectedIndex, new DialogInterface.OnClickListener() {
							@Override
							public void onClick(final DialogInterface dialog, final int i) {
								dialog.dismiss();
								final String path = folders.get(i).getAbsolutePath();
								PrefsUtility.pref_cache_location(context, prefs, path);
								updateStorageLocationText(path);
							}
						})
				.setNegativeButton(
						R.string.dialog_close,
						new DialogInterface.OnClickListener() {
							@Override
							public void onClick(final DialogInterface dialog, final int i) {
								dialog.dismiss();
							}
						})
				.create()
				.show();
	}

	private void updateStorageLocationText(final String path) {
		findPreference(getString(R.string.pref_cache_location_key)).setSummary(path);
	}
}
