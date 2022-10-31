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

import android.content.SharedPreferences;
import android.content.pm.ActivityInfo;
import android.os.Bundle;
import android.preference.PreferenceManager;
import android.view.MenuItem;
import android.view.Window;
import org.saiditnet.redreader.R;
import org.saiditnet.redreader.common.PrefsUtility;

import java.util.List;

public final class SettingsActivity
		extends AppCompatPreferenceActivity
		implements SharedPreferences.OnSharedPreferenceChangeListener {

	private SharedPreferences sharedPreferences;

	@Override
	protected void onCreate(final Bundle savedInstanceState) {

		getWindow().requestFeature(Window.FEATURE_ACTION_BAR);
		PrefsUtility.applySettingsTheme(this);
		super.onCreate(savedInstanceState);
		sharedPreferences = PreferenceManager.getDefaultSharedPreferences(this);
		sharedPreferences.registerOnSharedPreferenceChangeListener(this);
		setOrientationFromPrefs();

		getSupportActionBar().setHomeButtonEnabled(true);
		getSupportActionBar().setDisplayHomeAsUpEnabled(true);
	}

	@Override
	protected void onDestroy() {
		super.onDestroy();
		sharedPreferences.unregisterOnSharedPreferenceChangeListener(this);
	}

	@Override
	public void onBuildHeaders(final List<Header> target) {
		loadHeadersFromResource(R.xml.prefheaders, target);
	}

	@Override
	public boolean onOptionsItemSelected(final MenuItem item) {
		switch(item.getItemId()) {
			case android.R.id.home:
				finish();
				return true;
			default:
				return false;
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

	@Override
	public void onSharedPreferenceChanged(final SharedPreferences prefs, final String key) {

		if(key.equals(getString(R.string.pref_behaviour_screenorientation_key))) {
			setOrientationFromPrefs();
		}
	}

	@Override
	protected boolean isValidFragment(final String fragmentName) {
		return fragmentName.equals(SettingsFragment.class.getCanonicalName());
	}
}
