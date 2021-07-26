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

import android.content.Context;
import android.net.Uri;
import org.quantumbadger.redreader.common.Constants;
import org.quantumbadger.redreader.common.StringUtils;

import java.util.ArrayList;
import java.util.List;

public class UserProfileURL extends RedditURLParser.RedditURL {

	public final String username;

	public UserProfileURL(final String username) {
		this.username = username;
	}

	public static UserProfileURL parse(final Uri uri) {

		final String[] pathSegments;
		{
			final List<String> pathSegmentsList = uri.getPathSegments();

			final ArrayList<String> pathSegmentsFiltered = new ArrayList<>(
					pathSegmentsList.size());
			for(String segment : pathSegmentsList) {

				while(StringUtils.asciiLowercase(segment).endsWith(".json")
						|| StringUtils.asciiLowercase(segment).endsWith(".xml")) {
					segment = segment.substring(0, segment.lastIndexOf('.'));
				}

				if(!segment.isEmpty()) {
					pathSegmentsFiltered.add(segment);
				}
			}

			pathSegments
					= pathSegmentsFiltered.toArray(new String[0]);
		}

		if(pathSegments.length != 2) {
			return null;
		}

		if(!pathSegments[0].equalsIgnoreCase("user") && !pathSegments[0].equalsIgnoreCase(
				"u")) {
			return null;
		}

		// TODO validate username with regex
		final String username = pathSegments[1];

		return new UserProfileURL(username);
	}

	@Override
	public Uri generateJsonUri() {

		final Uri.Builder builder = new Uri.Builder();
		builder.scheme(Constants.Reddit.getScheme())
				.authority(Constants.Reddit.getDomain());

		builder.appendEncodedPath("user");
		builder.appendPath(username);

		builder.appendEncodedPath(".json");

		return builder.build();
	}

	@Override
	public @RedditURLParser.PathType
	int pathType() {
		return RedditURLParser.USER_PROFILE_URL;
	}

	@Override
	public String humanReadableName(final Context context, final boolean shorter) {
		return username;
	}
}
