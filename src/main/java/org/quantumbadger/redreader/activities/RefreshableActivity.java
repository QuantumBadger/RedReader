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

package com.konneh.scroll.activities;

import android.content.Intent;
import android.content.SharedPreferences;
import com.konneh.scroll.common.PrefsUtility;

import java.util.EnumSet;

public abstract class RefreshableActivity extends BaseActivity {

	private boolean paused = false;
	private final EnumSet<RefreshableFragment> refreshOnResume = EnumSet.noneOf(RefreshableFragment.class);

	public enum RefreshableFragment {
		MAIN, MAIN_RELAYOUT, POSTS, COMMENTS, RESTART, ALL
	}

	@Override
	protected void onPause() {
		super.onPause();
		paused = true;
	}

	@Override
	protected void onSharedPreferenceChangedInner(final SharedPreferences prefs, final String key) {

		if(PrefsUtility.isRestartRequired(this, key)) {
			requestRefresh(RefreshableFragment.RESTART, false);
		}

		if(PrefsUtility.isRefreshRequired(this, key)) {
			requestRefresh(RefreshableFragment.ALL, false);
		}

		if(this instanceof MainActivity && PrefsUtility.isReLayoutRequired(this, key)) {
			requestRefresh(RefreshableFragment.MAIN_RELAYOUT, false);
		}
	}

	@Override
	protected void onResume() {

		super.onResume();

		paused = false;

		if(!refreshOnResume.isEmpty()) {
			for(final RefreshableFragment f : refreshOnResume) {
				doRefreshNow(f, false);
			}

			refreshOnResume.clear();
		}
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
