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

package org.saiditnet.redreader.activities;

import android.content.SharedPreferences;
import android.os.Bundle;
import org.saiditnet.redreader.R;
import org.saiditnet.redreader.common.General;
import org.saiditnet.redreader.common.PrefsUtility;

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
			return;
		}

		if(this instanceof MainActivity && PrefsUtility.isReLayoutRequired(this, key)) {
			requestRefresh(RefreshableFragment.MAIN_RELAYOUT, false);
			return;
		}

		if(PrefsUtility.isRefreshRequired(this, key)) {
			requestRefresh(RefreshableFragment.ALL, false);
			return;
		}

		if(this instanceof MainActivity) {
			if(key.equals(getString(R.string.pref_pinned_subreddits_key)) ||
					key.equals(getString(R.string.pref_blocked_subreddits_key))) {
				requestRefresh(RefreshableFragment.MAIN, false);
			}
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
			General.recreateActivityNoAnimation(this);

		} else {
			doRefresh(which, force, null);
		}
	}

	protected abstract void doRefresh(RefreshableFragment which, boolean force, final Bundle savedInstanceState);

	public final void requestRefresh(final RefreshableFragment which, final boolean force) {
		runOnUiThread(new Runnable() {
			@Override
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
