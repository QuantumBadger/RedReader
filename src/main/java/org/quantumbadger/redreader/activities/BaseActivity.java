/*******************************************************************************
 * This file is part of Scroll.
 *
 * Scroll is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Scroll is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Scroll.  If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/

package org.quantumbadger.redreader.activities;

import android.app.Activity;
import android.content.SharedPreferences;
import android.content.pm.ActivityInfo;
import android.os.Bundle;
import android.preference.PreferenceManager;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.common.PrefsUtility;

public class BaseActivity extends Activity implements SharedPreferences.OnSharedPreferenceChangeListener {

	private SharedPreferences sharedPreferences;

	private static boolean closingAll = false;

	public void closeAllExceptMain() {
		closingAll = true;
		closeIfNecessary();
	}

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		sharedPreferences = PreferenceManager.getDefaultSharedPreferences(this);
		sharedPreferences.registerOnSharedPreferenceChangeListener(this);
		setOrientationFromPrefs();
		closeIfNecessary();
	}

	@Override
	protected void onResume() {
		super.onResume();
		setOrientationFromPrefs();
		closeIfNecessary();
	}


	@Override
	protected void onDestroy() {
		super.onDestroy();
		sharedPreferences.unregisterOnSharedPreferenceChangeListener(this);
	}

	private void closeIfNecessary() {
		if(closingAll) {
			if(this instanceof MainActivity) {
				closingAll = false;
			} else {
				finish();
			}
		}
	}

	private void setOrientationFromPrefs() {
		PrefsUtility.ScreenOrientation orientation = PrefsUtility.pref_behaviour_screen_orientation(this, sharedPreferences);
		if (orientation == PrefsUtility.ScreenOrientation.AUTO)
			setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_UNSPECIFIED);
		else if (orientation == PrefsUtility.ScreenOrientation.PORTRAIT)
			setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_PORTRAIT);
		else if (orientation == PrefsUtility.ScreenOrientation.LANDSCAPE)
			setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_LANDSCAPE);
	}

	protected void onSharedPreferenceChangedInner(final SharedPreferences prefs, final String key) {
		// Do nothing
	}

	@Override
	public final void onSharedPreferenceChanged(final SharedPreferences prefs, final String key) {

		onSharedPreferenceChangedInner(prefs, key);

		if(key.equals(getString(R.string.pref_network_https_key))) {
			PrefsUtility.network_https(this, prefs);

		} else if(key.equals(getString(R.string.pref_menus_optionsmenu_items_key))) {
			invalidateOptionsMenu();
		}
	}
}
