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

package org.quantumbadger.redreader.activities;

import android.content.Intent;
import org.holoeverywhere.preference.PreferenceManager;
import org.holoeverywhere.preference.SharedPreferences;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.common.PrefsUtility;

import java.util.EnumSet;

public abstract class RefreshableActivity extends BaseActivity {

	private boolean paused = false;
	private final EnumSet<RefreshableFragment> refreshOnResume = EnumSet.noneOf(RefreshableFragment.class);

	private final SharedPreferences.OnSharedPreferenceChangeListener changeListener
			= new SharedPreferences.OnSharedPreferenceChangeListener() {
		public void onSharedPreferenceChanged(SharedPreferences prefs, String key) {
			if(key.equals(getString(R.string.pref_network_https_key))) {
				PrefsUtility.network_https(RefreshableActivity.this, prefs);
			}
		}
	};

	public enum RefreshableFragment {
		MAIN, MAIN_RELAYOUT, POSTS, COMMENTS, RESTART, ALL
	}

	@Override
	protected void onPause() {
		super.onPause();
		paused = true;
		PreferenceManager.getDefaultSharedPreferences(this).unregisterOnSharedPreferenceChangeListener(changeListener);
	}

	@Override
	protected void onResume() {

		super.onResume();

		final SharedPreferences prefs = PreferenceManager.getDefaultSharedPreferences(this);
		PrefsUtility.network_https(this, prefs);
		prefs.registerOnSharedPreferenceChangeListener(changeListener);

		paused = false;

		for(final RefreshableFragment f : refreshOnResume) {
			doRefreshNow(f, false);
		}

		refreshOnResume.clear();
	}

	protected void doRefreshNow(RefreshableFragment which, boolean force) {

		if(which == RefreshableFragment.RESTART) {

			// http://stackoverflow.com/a/3419987/1526861
			final Intent intent = getIntent();
			overridePendingTransition(0, 0);
			intent.addFlags(Intent.FLAG_ACTIVITY_NO_ANIMATION);
			finish();
			overridePendingTransition(0, 0);
			startActivity(intent);

		} else {
			doRefresh(which, force);
		}
	}

	protected abstract void doRefresh(RefreshableFragment which, boolean force);

	public final void requestRefresh(final RefreshableFragment which, final boolean force) {
		runOnUiThread(new Runnable() {
			public void run() {
				if(!paused) {
					doRefreshNow(which, force);
				} else {
					refreshOnResume.add(which); // TODO this doesn't remember "force" (but it doesn't really matter...)
				}
			}}
		);
	}
}
