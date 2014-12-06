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

package org.quantumbadger.redreader.listingcontrollers;

import android.content.Context;
import android.net.Uri;
import org.holoeverywhere.preference.PreferenceManager;
import org.quantumbadger.redreader.cache.CacheRequest;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.fragments.CommentListingFragment;
import org.quantumbadger.redreader.reddit.url.CommentListingURL;

import java.util.UUID;

// TODO add notification/header for abnormal sort order
// TODO parse URLs, support parents/context -- use post permalink
public class CommentListingController {

	private CommentListingURL mUrl;
	private UUID mSession = null;

	public UUID getSession() {
		return mSession;
	}

	public void setSession(UUID session) {
		mSession = session;
	}

	public static enum Sort {

		BEST("confidence"),
		HOT("hot"),
		NEW("new"),
		OLD("old"),
		TOP("top"),
		CONTROVERSIAL("controversial");

		public final String key;

		private Sort(final String key) {
			this.key = key;
		}

		public static Sort lookup(String name) {

			name = name.toUpperCase();

			if(name.equals("CONFIDENCE")) {
				return BEST; // oh, reddit...
			}

			try {
				return Sort.valueOf(name);
			} catch(IllegalArgumentException e) {
				return null;
			}
		}
	}

	public CommentListingController(CommentListingURL url, final Context context) {

		if(url.order == null) {
			url = url.order(defaultOrder(context));
		}

		this.mUrl = url;
	}

	private Sort defaultOrder(final Context context) {
		return PrefsUtility.pref_behaviour_commentsort(context, PreferenceManager.getDefaultSharedPreferences(context));
	}

	public void setSort(final Sort s) {
		mUrl = mUrl.order(s);
	}

	public Uri getUri() {
		return mUrl.generateJsonUri();
	}

	public CommentListingFragment get(final boolean force) {
		if(force) mSession = null;
		return CommentListingFragment.newInstance(mUrl, mSession, force ? CacheRequest.DownloadType.FORCE : CacheRequest.DownloadType.IF_NECESSARY);
	}
}
