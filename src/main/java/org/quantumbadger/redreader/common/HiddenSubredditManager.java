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
	private static HashSet<String> HiddenSubreddits;
	private static boolean IsEmpty;

	public static void loadHiddens(final Context context) {
		final SharedPreferences prefs = PreferenceManager.getDefaultSharedPreferences(context);
		HiddenSubreddits = PrefsUtility.pref_all_filter(context, prefs);
		IsEmpty = (HiddenSubreddits.size() == 0);
	}

	public static boolean showSubreddit(String Subreddit) {
		if (IsEmpty)
			return true;
		return !HiddenSubreddits.contains(Subreddit.toLowerCase());
	}

	public static void hideSubreddit(final Activity activity, String Subreddit) {
		HiddenSubreddits.add(Subreddit.toLowerCase());
		final SharedPreferences prefs = PreferenceManager.getDefaultSharedPreferences(activity);
		final SharedPreferences.Editor editor = prefs.edit();
		editor.putString(activity.getString(R.string.pref_all_filter_key), StringUtils.join(HiddenSubreddits, ','));
		editor.commit();
		IsEmpty = false;
	}
}
