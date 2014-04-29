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

import android.content.Context;
import org.apache.commons.lang3.StringUtils;
import org.holoeverywhere.preference.PreferenceManager;
import org.holoeverywhere.app.Activity;
import org.holoeverywhere.preference.SharedPreferences;
import org.quantumbadger.redreader.R;

import java.util.HashSet;

public class HiddenSubredditManager {
	private HashSet<String> hiddenSubreddits;

	public static HiddenSubredditManager getInstance(final Context context) {
		return new HiddenSubredditManager(context);
	}

	public HiddenSubredditManager(final Context context) {
		loadHiddenSubreddits(context);
	}

	private void loadHiddenSubreddits(final Context context) {
		final SharedPreferences prefs = PreferenceManager.getDefaultSharedPreferences(context);
		hiddenSubreddits = PrefsUtility.pref_all_filter(context, prefs);
	}

	public boolean isSubredditHidden(String Subreddit) {
		return !hiddenSubreddits.contains(Subreddit.toLowerCase());
	}

	public void hideSubreddit(final Activity activity, String Subreddit) {
		hiddenSubreddits.add(Subreddit.toLowerCase());
		final SharedPreferences prefs = PreferenceManager.getDefaultSharedPreferences(activity);
		final SharedPreferences.Editor editor = prefs.edit();
		editor.putString(activity.getString(R.string.pref_all_filter_key), StringUtils.join(hiddenSubreddits, ','));
		editor.commit();
	}
}
