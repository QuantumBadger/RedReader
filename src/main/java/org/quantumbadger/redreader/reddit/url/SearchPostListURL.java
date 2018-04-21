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
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.reddit.PostSort;

import java.util.ArrayList;
import java.util.List;

public class SearchPostListURL extends PostListingURL {

	public final String subreddit, query;

	public final PostSort order;
	public final Integer limit;
	public final String before, after;

	SearchPostListURL(String subreddit, String query, PostSort order, Integer limit, String before, String after) {
		this.subreddit = subreddit;
		this.query = query;
		this.order = order;
		this.limit = limit;
		this.before = before;
		this.after = after;
	}

	SearchPostListURL(String subreddit, String query, Integer limit, String before, String after) {
		this(subreddit, query, PostSort.RELEVANCE, limit, before, after);
	}

	public static SearchPostListURL build(String subreddit, String query) {
		if(subreddit != null) {
			while(subreddit.startsWith("/")) subreddit = subreddit.substring(1);
			while(subreddit.startsWith("r/")) subreddit = subreddit.substring(2);
		}
		return new SearchPostListURL(subreddit, query, null, null, null);
	}

	@Override
	public PostListingURL after(String after) {
		return new SearchPostListURL(subreddit, query, order, limit, before, after);
	}

	@Override
	public PostListingURL limit(Integer limit) {
		return new SearchPostListURL(subreddit, query, order, limit, before, after);
	}

	public SearchPostListURL sort(PostSort newOrder) {
		return new SearchPostListURL(subreddit, query, newOrder, limit, before, after);
	}

	@Override
	public Uri generateJsonUri() {

		Uri.Builder builder = new Uri.Builder();
		builder.scheme(Constants.Reddit.getScheme()).authority(Constants.Reddit.getHumanReadableDomain());

		if(subreddit != null) {
			builder.encodedPath("/r/");
			builder.appendPath(subreddit);
			builder.appendQueryParameter("restrict_sr", "on");
		} else {
			builder.encodedPath("/");
		}

		builder.appendEncodedPath("search");

		if(query != null) {
			builder.appendQueryParameter("q", query);
		}

		if(order != null) {
			switch(order) {
				case RELEVANCE:
				case NEW:
				case HOT:
				case TOP:
				case COMMENTS:
					builder.appendQueryParameter("sort", General.asciiLowercase(order.name()));
					break;
			}
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

		// if the user doesn't have NSFW content disabled, it won't show up anyway
		// leaving this on by default doesn't hurt
		builder.appendQueryParameter("include_over_18", "on");

		return builder.build();
	}

	@Override
	public @RedditURLParser.PathType int pathType() {
		return RedditURLParser.SEARCH_POST_LISTING_URL;
	}

	public static SearchPostListURL parse(final Uri uri) {

		boolean restrict_sr = false;
		String query = "";
		PostSort order = null;
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

			} else if(parameterKey.equalsIgnoreCase("sort")) {
				order = PostSort.valueOfOrNull(uri.getQueryParameter(parameterKey));

			} else if(parameterKey.equalsIgnoreCase("q")) {
				query = uri.getQueryParameter(parameterKey);

			} else if(parameterKey.equalsIgnoreCase("restrict_sr")) {
				restrict_sr = "on".equalsIgnoreCase(uri.getQueryParameter(parameterKey));
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

		if(pathSegments.length != 1 && pathSegments.length != 3) return null;
		if(!pathSegments[pathSegments.length - 1].equalsIgnoreCase("search")) return null;

		switch(pathSegments.length) {

			case 1: {
				return new SearchPostListURL(null, query, order, limit, before, after);
			}

			case 3: {

				if(!pathSegments[0].equals("r")) return null;

				final String subreddit = pathSegments[1];
				return new SearchPostListURL(restrict_sr ? subreddit : null, query, order, limit, before, after);
			}

			default:
				return null;
		}
	}

	@Override
	public String humanReadableName(Context context, boolean shorter) {

		if(shorter) return "Search Results";

		// TODO strings
		final StringBuilder builder = new StringBuilder("Search");

		if(query != null) {
			builder.append(" for \"").append(query).append("\"");
		}

		if(subreddit != null) {
			builder.append(" on /r/").append(subreddit);
		}

		return builder.toString();
	}

	@Override
	public String humanReadablePath() {
		final StringBuilder builder = new StringBuilder(super.humanReadablePath());

		if(query != null) {
			builder.append("?q=").append(query);
		}

		return builder.toString();
	}
}
