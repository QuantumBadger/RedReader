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
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import org.quantumbadger.redreader.common.Constants;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.reddit.PostSort;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

public class MultiredditPostListURL extends PostListingURL {

	public static RedditURLParser.RedditURL getMultireddit(
			@NonNull final String name) {

		Uri.Builder builder = new Uri.Builder();
		builder.scheme(Constants.Reddit.getScheme()).authority(Constants.Reddit.getDomain());

		builder.encodedPath("/me/m/");
		builder.appendPath(name);

		return RedditURLParser.parse(builder.build());
	}

	public static RedditURLParser.RedditURL getMultireddit(
			@NonNull final String username,
			@NonNull final String name) {

		Uri.Builder builder = new Uri.Builder();
		builder.scheme(Constants.Reddit.getScheme()).authority(Constants.Reddit.getDomain());

		builder.encodedPath("/user/");
		builder.appendPath(username);
		builder.appendPath("/m/");
		builder.appendPath(name);

		return RedditURLParser.parse(builder.build());
	}

	@Nullable public final String username;
	@NonNull public final String name;

	@Nullable public final PostSort order;
	@Nullable public final Integer limit;
	@Nullable public final String before, after;

	private MultiredditPostListURL(
			@Nullable final String username,
			@NonNull final String name,
			@Nullable final PostSort order,
			@Nullable final Integer limit,
			@Nullable final String before,
			@Nullable final String after) {

		this.username = username;
		this.name = name;
		this.order = order;
		this.limit = limit;
		this.before = before;
		this.after = after;
	}

	public MultiredditPostListURL after(String newAfter) {
		return new MultiredditPostListURL(username, name, order, limit, before, newAfter);
	}

	public MultiredditPostListURL limit(Integer newLimit) {
		return new MultiredditPostListURL(username, name, order, newLimit, before, after);
	}

	public MultiredditPostListURL sort(PostSort newOrder) {
		return new MultiredditPostListURL(username, name, newOrder, limit, before, after);
	}

	public PostSort getOrder() {
		return order;
	}

	@Override
	public Uri generateJsonUri() {

		Uri.Builder builder = new Uri.Builder();
		builder.scheme(Constants.Reddit.getScheme()).authority(Constants.Reddit.getDomain());

		if(username != null) {
			builder.encodedPath("/user/");
			builder.appendPath(username);
		} else {
			builder.encodedPath("/me/");
		}

		builder.appendPath("m");
		builder.appendPath(name);

		if(order != null) {
			order.addToSubredditListingUri(builder);
		}

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
		return RedditURLParser.MULTIREDDIT_POST_LISTING_URL;
	}

	public static MultiredditPostListURL parse(final Uri uri) {

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

				while(General.asciiLowercase(segment).endsWith(".json") || General.asciiLowercase(segment).endsWith(".xml")) {
					segment = segment.substring(0, segment.lastIndexOf('.'));
				}

				if(segment.length() > 0) {
					pathSegmentsFiltered.add(segment);
				}
			}

			pathSegments = pathSegmentsFiltered.toArray(new String[pathSegmentsFiltered.size()]);
		}

		final PostSort order;
		if(pathSegments.length > 0) {
			order = PostSort.parse(pathSegments[pathSegments.length - 1], uri.getQueryParameter("t"));
		} else {
			order = null;
		}

		if(pathSegments.length < 3) {
			return null;
		}

		if(pathSegments[0].equalsIgnoreCase("me")) {

			if(!pathSegments[1].equalsIgnoreCase("m")) {
				return null;
			}

			return new MultiredditPostListURL(
					null,
					pathSegments[2],
					order,
					limit,
					before,
					after);

		} else {

			if(!pathSegments[0].equalsIgnoreCase("user")
					|| !pathSegments[2].equalsIgnoreCase("m")
					|| pathSegments.length < 4) {

				return null;
			}

			return new MultiredditPostListURL(
					pathSegments[1],
					pathSegments[3],
					order,
					limit,
					before,
					after);
		}
	}

	@Override
	public String humanReadablePath() {

		String path = super.humanReadablePath();

		if(order == null) {
			return path;
		}

		switch(order) {
			case TOP_HOUR:
			case TOP_DAY:
			case TOP_WEEK:
			case TOP_MONTH:
			case TOP_YEAR:
			case TOP_ALL:
				return path + "?t=" + General.asciiLowercase(order.name().split("_")[1]);

			default:
				return path;
		}
	}

	@Override
	public String humanReadableName(Context context, boolean shorter) {

		if(username == null) {
			return name;

		} else {
			return String.format(Locale.US, "%s (%s)", name, username);
		}
	}
}
