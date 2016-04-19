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

public class UserPostListingURL extends PostListingURL {

	public static UserPostListingURL getSaved(String username) {
		return new UserPostListingURL(Type.SAVED, username, null, null, null);
	}

	public static UserPostListingURL getHidden(String username) {
		return new UserPostListingURL(Type.HIDDEN, username, null, null, null);
	}

	public static UserPostListingURL getLiked(String username) {
		return new UserPostListingURL(Type.UPVOTED, username, null, null, null);
	}

	public static UserPostListingURL getDisliked(String username) {
		return new UserPostListingURL(Type.DOWNVOTED, username, null, null, null);
	}

	public static UserPostListingURL getSubmitted(String username) {
		return new UserPostListingURL(Type.SUBMITTED, username, null, null, null);
	}

	public final Type type;
	public final String user;
	public final Integer limit;
	public final String before, after;

	UserPostListingURL(Type type, String user, Integer limit, String before, String after) {
		this.type = type;
		this.user = user;
		this.limit = limit;
		this.before = before;
		this.after = after;
	}

	public enum Type {
		SAVED, HIDDEN, UPVOTED, DOWNVOTED, SUBMITTED
	}

	@Override
	public UserPostListingURL after(String newAfter) {
		return new UserPostListingURL(type, user, limit, before, newAfter);
	}

	@Override
	public UserPostListingURL limit(Integer newLimit) {
		return new UserPostListingURL(type, user, newLimit, before, after);
	}

	public static UserPostListingURL parse(Uri uri) {

		Integer limit = null;
		String before = null, after = null;

		for(final String parameterKey : General.getUriQueryParameterNames(uri)) {

			if(parameterKey.equalsIgnoreCase("after")) {
				after = uri.getQueryParameter(parameterKey);

			} else if(parameterKey.equalsIgnoreCase("before")) {
				before = uri.getQueryParameter(parameterKey);

			} else if(parameterKey.equalsIgnoreCase("limit")) {
				try {
					limit = Integer.parseInt(uri.getQueryParameter(parameterKey));
				} catch(Throwable ignored) {}
			}
		}

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
		final String typeName = pathSegments[2].toUpperCase();
		final Type type;

		try {
			type = Type.valueOf(typeName);
		} catch(Throwable t) {
			return null;
		}

		return new UserPostListingURL(type, username, limit, before, after);
	}

	@Override
	public Uri generateJsonUri() {

		Uri.Builder builder = new Uri.Builder();
		builder.scheme(Constants.Reddit.getScheme()).authority(Constants.Reddit.getDomain());

		builder.appendEncodedPath("user");
		builder.appendPath(user);
		builder.appendEncodedPath(type.name().toLowerCase());

		if(before != null) {
			builder.appendQueryParameter("before", before);
		}

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
		return RedditURLParser.USER_POST_LISTING_URL;
	}

	@Override
	public String humanReadableName(Context context, boolean shorter) {

		final String name;

		switch(type) {

			case SAVED:
				name = context.getString(R.string.mainmenu_saved);
				break;

			case HIDDEN:
				name = context.getString(R.string.mainmenu_hidden);
				break;

			case UPVOTED:
				name = context.getString(R.string.mainmenu_upvoted);
				break;

			case DOWNVOTED:
				name = context.getString(R.string.mainmenu_downvoted);
				break;

			case SUBMITTED:
				name = context.getString(R.string.mainmenu_submitted);
				break;

			default:
				return super.humanReadableName(context, shorter);
		}

		if(shorter) {
			return name;
		} else {
			return String.format("%s (%s)", name, user);
		}
	}
}
