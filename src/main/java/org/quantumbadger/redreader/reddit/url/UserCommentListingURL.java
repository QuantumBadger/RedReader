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
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.common.Constants;
import org.quantumbadger.redreader.common.General;

import java.util.ArrayList;
import java.util.List;

public class UserCommentListingURL extends CommentListingURL {

	public final String user;
	public final Integer limit;
	public final String after;

	UserCommentListingURL(String user, Integer limit, String after) {
		this.user = user;
		this.limit = limit;
		this.after = after;
	}

	@Override
	public UserCommentListingURL after(String newAfter) {
		return new UserCommentListingURL(user, limit, newAfter);
	}

	@Override
	public UserCommentListingURL limit(Integer newLimit) {
		return new UserCommentListingURL(user, newLimit, after);
	}

	public static UserCommentListingURL parse(Uri uri) {

		final String[] pathSegments;
		{
			final List<String> pathSegmentsList = uri.getPathSegments();

			final ArrayList<String> pathSegmentsFiltered = new ArrayList<>(pathSegmentsList.size());
			for(String segment : pathSegmentsList) {

				while(segment.toLowerCase().endsWith(".json") || segment.toLowerCase().endsWith(".xml")) {
					segment = segment.substring(0, segment.lastIndexOf('.'));
				}

				if(segment.length() > 0) {
					pathSegmentsFiltered.add(segment);
				}
			}

			pathSegments = pathSegmentsFiltered.toArray(new String[pathSegmentsFiltered.size()]);
		}

		if(pathSegments.length < 3) {
			return null;
		}

		if(!pathSegments[0].equalsIgnoreCase("user") && !pathSegments[0].equalsIgnoreCase("u")) {
			return null;
		}

		// TODO validate username with regex
		final String username = pathSegments[1];
		final String typeName = pathSegments[2];

		if(!typeName.equalsIgnoreCase("comments")) {
			return null;
		}

		Integer limit = null;
		String after = null;

		for(final String parameterKey : General.getUriQueryParameterNames(uri)) {

			if(parameterKey.equalsIgnoreCase("after")) {
				after = uri.getQueryParameter(parameterKey);

			} else if(parameterKey.equalsIgnoreCase("limit")) {
				try {
					limit = Integer.parseInt(uri.getQueryParameter(parameterKey));
				} catch(Throwable ignored) {}
			}
		}

		return new UserCommentListingURL(username, limit, after);
	}

	@Override
	public Uri generateJsonUri() {

		Uri.Builder builder = new Uri.Builder();
		builder.scheme(Constants.Reddit.getScheme()).authority(Constants.Reddit.getDomain());

		builder.appendEncodedPath("user");
		builder.appendPath(user);
		builder.appendEncodedPath("comments");

		if(after != null) {
			builder.appendQueryParameter("after", after);
		}

		if(limit != null) {
			builder.appendQueryParameter("limit", String.valueOf(limit));
		}

		builder.appendEncodedPath(".json");

		return builder.build();
	}

	@Override
	public @RedditURLParser.PathType int pathType() {
		return RedditURLParser.USER_COMMENT_LISTING_URL;
	}

	@Override
	public String humanReadableName(Context context, boolean shorter) {

		final String name = context.getString(R.string.user_comments);

		if(shorter) {
			return name;
		} else {
			return String.format("%s (%s)", name, user);
		}
	}
}
