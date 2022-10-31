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

package org.saiditnet.redreader.settings;

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
import org.saiditnet.redreader.BuildConfig;
import org.saiditnet.redreader.R;
import org.saiditnet.redreader.activities.ChangelogActivity;
import org.saiditnet.redreader.activities.HtmlViewActivity;
import org.saiditnet.redreader.cache.CacheManager;
import org.saiditnet.redreader.common.AndroidCommon;
import org.saiditnet.redreader.common.General;
import org.saiditnet.redreader.common.PrefsUtility;
import org.saiditnet.redreader.common.TorCommon;

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
		} catch (IllegalAccessException e) {
			throw new RuntimeException(e);
		} catch (NoSuchFieldException e) {
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
				R.string.pref_appearance_fontscale_posts_key,
				R.string.pref_appearance_fontscale_comments_key,
				R.string.pref_appearance_fontscale_inbox_key,
				R.string.pref_behaviour_actions_comment_tap_key,
				R.string.pref_behaviour_actions_comment_longclick_key,
				R.string.pref_behaviour_commentsort_key,
				R.string.pref_behaviour_postsort_key,
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
				R.string.pref_cache_rerequest_postlist_age_key
		};

		final int[] editTextPrefsToUpdate = {
				R.string.pref_behaviour_comment_min_key
		};

		for(int pref : listPrefsToUpdate) {

			final ListPreference listPreference = (ListPreference)findPreference(getString(pref));

			if(listPreference == null) continue;

			final int index = listPreference.findIndexOfValue(listPreference.getValue());
			if(index < 0) continue;

			listPreference.setSummary(listPreference.getEntries()[index]);

			listPreference.setOnPreferenceChangeListener(new Preference.OnPreferenceChangeListener() {
				public boolean onPreferenceChange(Preference preference, Object newValue) {
					final int index = listPreference.findIndexOfValue((String)newValue);
					listPreference.setSummary(listPreference.getEntries()[index]);
					return true;
				}
			});
		}

		for(final int pref : editTextPrefsToUpdate) {

			final EditTextPreference editTextPreference = (EditTextPreference)findPreference(getString(pref));

			if(editTextPreference == null) continue;

			editTextPreference.setSummary(editTextPreference.getText());

			editTextPreference.setOnPreferenceChangeListener(new Preference.OnPreferenceChangeListener() {

				@Override
				public boolean onPreferenceChange(Preference preference, Object newValue) {
					if(newValue != null) {
						editTextPreference.setSummary(newValue.toString());
					} else {
						editTextPreference.setSummary("(null)");
					}
					return true;
				}
			});
		}


		final Preference versionPref = findPreference(getString(R.string.pref_about_version_key));
		final Preference changelogPref = findPreference(getString(R.string.pref_about_changelog_key));
		final Preference torPref = findPreference(getString(R.string.pref_network_tor_key));
		final Preference licensePref = findPreference(getString(R.string.pref_about_license_key));

		final PackageInfo pInfo;

		try {
			pInfo = context.getPackageManager().getPackageInfo(context.getPackageName(), 0);
		} catch(PackageManager.NameNotFoundException e) {
			throw new RuntimeException(e);
		}

		if(versionPref != null) {
			versionPref.setSummary(pInfo.versionName);
		}

		if(changelogPref != null) {
			changelogPref.setOnPreferenceClickListener(new Preference.OnPreferenceClickListener() {
				public boolean onPreferenceClick(Preference preference) {
					final Intent intent = new Intent(context, ChangelogActivity.class);
					context.startActivity(intent);
					return true;
				}
			});
		}

		if(licensePref != null) {
			licensePref.setOnPreferenceClickListener(new Preference.OnPreferenceClickListener() {
				public boolean onPreferenceClick(Preference preference) {
					HtmlViewActivity.showAsset(context, "license.html");
					return true;
				}
			});
		}

		if(torPref != null) {
			torPref.setOnPreferenceChangeListener(new Preference.OnPreferenceChangeListener() {
				@Override
				public boolean onPreferenceChange(Preference preference, final Object newValue) {

					// Run this after the preference has actually changed
					AndroidCommon.UI_THREAD_HANDLER.post(new Runnable() {
						@Override
						public void run() {
							TorCommon.updateTorStatus(context);
							if(TorCommon.isTorEnabled() != Boolean.TRUE.equals(newValue)) {
								throw new RuntimeException("Tor not correctly enabled after preference change");
							}
						}
					});

					return true;
				}
			});
		}

		if(Build.VERSION.SDK_INT < Build.VERSION_CODES.LOLLIPOP) {
			final Preference pref = findPreference(getString(R.string.pref_appearance_navbar_color_key));

			if(pref != null) {
				pref.setEnabled(false);
				pref.setSummary(R.string.pref_not_supported_before_lollipop);
			}
		}

		Preference cacheLocationPref = findPreference(getString(R.string.pref_cache_location_key));
		if (cacheLocationPref != null) {
			cacheLocationPref.setOnPreferenceClickListener(new Preference.OnPreferenceClickListener() {
				@Override
				public boolean onPreferenceClick(Preference preference) {
					showChooseStorageLocationDialog();
					return true;
				}
			});
			updateStorageLocationText(PrefsUtility.pref_cache_location(context,
					PreferenceManager.getDefaultSharedPreferences(context)));
		}
	}

	private void showChooseStorageLocationDialog() {
		final Context context = getActivity();
		final SharedPreferences prefs = PreferenceManager.getDefaultSharedPreferences(context);
		String currentStorage = PrefsUtility.pref_cache_location(context, prefs);

		List<File> checkPaths = CacheManager.getCacheDirs(context);

		final List<File> folders = new ArrayList<>(checkPaths.size());

		List<CharSequence> choices = new ArrayList<>(checkPaths.size());
		int selectedIndex = 0;

		for (int i = 0; i < checkPaths.size(); i++) {
			File dir = checkPaths.get(i);
			if (dir == null || !dir.exists() || !dir.canRead() || !dir.canWrite()) {
				continue;
			}
			folders.add(dir);
			if (currentStorage.equals(dir.getAbsolutePath())) {
				selectedIndex = i;
			}

			String path = dir.getAbsolutePath();
			long bytes = General.getFreeSpaceAvailable(path);
			String freeSpace = General.addUnits(bytes);
			if (!path.endsWith("/")) {
				path += "/";
			}
			String appCachePostfix = BuildConfig.APPLICATION_ID + "/cache/";
			if (path.endsWith("Android/data/" + appCachePostfix)) {
				path = path.substring(0, path.length() - appCachePostfix.length() - 14);
			} else if (path.endsWith(appCachePostfix)) {
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
							public void onClick(DialogInterface dialog, int i) {
								dialog.dismiss();
								String path = folders.get(i).getAbsolutePath();
								PrefsUtility.pref_cache_location(context, prefs, path);
								updateStorageLocationText(path);
							}
						})
				.setNegativeButton(R.string.dialog_close, new DialogInterface.OnClickListener() {
					@Override
					public void onClick(DialogInterface dialog, int i) {
						dialog.dismiss();
					}
				})
				.create()
				.show();
	}

	private void updateStorageLocationText(String path) {
		findPreference(getString(R.string.pref_cache_location_key)).setSummary(path);
	}
}
