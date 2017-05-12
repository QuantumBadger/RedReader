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

import android.content.SharedPreferences;
import android.content.pm.ActivityInfo;
import android.content.res.TypedArray;
import android.graphics.drawable.ColorDrawable;
import android.os.Build;
import android.os.Bundle;
import android.preference.PreferenceManager;
import android.support.v7.widget.Toolbar;
import android.view.LayoutInflater;
import android.view.MenuItem;
import android.widget.LinearLayout;

import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.common.PrefsUtility;

import java.util.List;

public final class SettingsActivity
		extends AppCompatPreferenceActivity
		implements SharedPreferences.OnSharedPreferenceChangeListener {

	private SharedPreferences sharedPreferences;
	private Toolbar toolbar;

	@Override
	protected void onCreate(final Bundle savedInstanceState) {
		PrefsUtility.applyTheme(this);
		super.onCreate(savedInstanceState);
		sharedPreferences = PreferenceManager.getDefaultSharedPreferences(this);
		sharedPreferences.registerOnSharedPreferenceChangeListener(this);
		setOrientationFromPrefs();
		LinearLayout root =
				(LinearLayout) findViewById(android.R.id.list).getParent().getParent().getParent();
		toolbar = (Toolbar) LayoutInflater.from(this).inflate(R.layout.layout_setting, root, false);
		root.addView(toolbar, 0);
		setSupportActionBar(toolbar);
		getSupportActionBar().setHomeButtonEnabled(true);
		getSupportActionBar().setDisplayHomeAsUpEnabled(true);
		if(Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP) {
			final TypedArray appearance = obtainStyledAttributes(new int[]{
					R.attr.colorPrimary,
					R.attr.colorPrimaryDark});
			getSupportActionBar().setBackgroundDrawable(new ColorDrawable(appearance.getColor(0,0)));
			appearance.recycle();
		}

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
