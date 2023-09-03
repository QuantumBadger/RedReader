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

package org.quantumbadger.redreader.reddit.url;

import android.net.Uri;

import androidx.annotation.Nullable;

import java.util.List;

public class OpaqueSharedURL extends RedditURLParser.RedditURL {

	@Nullable
	public final String subreddit;
	@Nullable public final String shareKey;

	public OpaqueSharedURL(@Nullable final String subreddit, @Nullable final String shareKey) {
		this.subreddit = subreddit;
		this.shareKey = shareKey;
	}

	@Override
	public Uri generateJsonUri() {
		return null;
	}

	@Override
	public int pathType() {
		return RedditURLParser.OPAQUE_SHARED_URL;
	}

	public static OpaqueSharedURL parse(final Uri uri) {
		// URLs look like https://reddit.com/r/RedReader/s/<alphanumeric>
		// first pull out the path segments and ensure they match the example (should be 4)
		final List<String> pathSegments = uri.getPathSegments();
		if (pathSegments.size() != 4) {
			return null;
		}

		// ensure the first segment is "r" and the third is "s"
		if (!pathSegments.get(0).equals("r") || !pathSegments.get(2).equals("s")) {
			return null;
		}

		return new OpaqueSharedURL(pathSegments.get(1), pathSegments.get(3));
	}

	public static Uri getUrlToFetch(final OpaqueSharedURL url) {
		return Uri.parse(String.format("https://www.reddit.com/r/%s/s/%s", url.subreddit, url.shareKey));
	}

	@Nullable
	public String getSubreddit() {
		return subreddit;
	}

	@Nullable
	public String getShareKey() {
		return shareKey;
	}
}
