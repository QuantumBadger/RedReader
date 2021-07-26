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
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.common.Constants;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.StringUtils;
import org.quantumbadger.redreader.reddit.PostSort;
import org.quantumbadger.redreader.reddit.things.InvalidSubredditNameException;
import org.quantumbadger.redreader.reddit.things.SubredditCanonicalId;

import java.util.ArrayList;
import java.util.List;

public class SubredditPostListURL extends PostListingURL {

	public static SubredditPostListURL getFrontPage() {
		return new SubredditPostListURL(Type.FRONTPAGE, null, null, null, null, null);
	}

	public static SubredditPostListURL getPopular() {
		return new SubredditPostListURL(Type.POPULAR, null, null, null, null, null);
	}

	public static SubredditPostListURL getRandom() {
		return new SubredditPostListURL(Type.RANDOM, "random", null, null, null, null);
	}

	public static SubredditPostListURL getRandomNsfw() {
		return new SubredditPostListURL(Type.RANDOM, "randnsfw", null, null, null, null);
	}

	public static SubredditPostListURL getAll() {
		return new SubredditPostListURL(Type.ALL, null, null, null, null, null);
	}

	public static RedditURLParser.RedditURL getSubreddit(final String subreddit) throws
			InvalidSubredditNameException {
		return getSubreddit(new SubredditCanonicalId(subreddit));
	}

	public static RedditURLParser.RedditURL getSubreddit(final SubredditCanonicalId subreddit) {

		return RedditURLParser.parse(new Uri.Builder()
				.scheme(Constants.Reddit.getScheme())
				.authority(Constants.Reddit.getDomain())
				.encodedPath(subreddit.toString()).build());
	}

	public enum Type {
		FRONTPAGE, ALL, SUBREDDIT, SUBREDDIT_COMBINATION, ALL_SUBTRACTION, POPULAR, RANDOM
	}

	@NonNull public final Type type;
	@Nullable public final String subreddit;

	@Nullable public final PostSort order;
	@Nullable public final Integer limit;
	@Nullable public final String before;
	@Nullable public final String after;

	private SubredditPostListURL(
			@NonNull final Type type,
			@Nullable final String subreddit,
			@Nullable final PostSort order,
			@Nullable final Integer limit,
			@Nullable final String before,
			@Nullable final String after) {

		this.type = type;
		this.subreddit = subreddit;
		this.order = order;
		this.limit = limit;
		this.before = before;
		this.after = after;
	}

	@Override
	public SubredditPostListURL after(final String newAfter) {
		return new SubredditPostListURL(type, subreddit, order, limit, before, newAfter);
	}

	@Override
	public SubredditPostListURL limit(final Integer newLimit) {
		return new SubredditPostListURL(type, subreddit, order, newLimit, before, after);
	}

	public SubredditPostListURL sort(final PostSort newOrder) {
		return new SubredditPostListURL(type, subreddit, newOrder, limit, before, after);
	}

	@Nullable
	@Override
	public PostSort getOrder() {
		return order;
	}

	@Override
	public Uri generateJsonUri() {

		final Uri.Builder builder = new Uri.Builder();
		builder.scheme(Constants.Reddit.getScheme())
				.authority(Constants.Reddit.getDomain());

		switch(type) {

			case FRONTPAGE:
				builder.encodedPath("/");
				break;

			case ALL:
				builder.encodedPath("/r/all");
				break;

			case SUBREDDIT:
			case SUBREDDIT_COMBINATION:
			case ALL_SUBTRACTION:
			case RANDOM:
				builder.encodedPath("/r/");
				builder.appendPath(subreddit);
				break;

			case POPULAR:
				builder.encodedPath("/r/popular");
				break;
		}

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
	public @RedditURLParser.PathType
	int pathType() {
		return RedditURLParser.SUBREDDIT_POST_LISTING_URL;
	}

	public static SubredditPostListURL parse(final Uri uri) {

		Integer limit = null;
		String before = null;
		String after = null;

		for(final String parameterKey : General.getUriQueryParameterNames(uri)) {

			if(parameterKey.equalsIgnoreCase("after")) {
				after = uri.getQueryParameter(parameterKey);

			} else if(parameterKey.equalsIgnoreCase("before")) {
				before = uri.getQueryParameter(parameterKey);

			} else if(parameterKey.equalsIgnoreCase("limit")) {
				try {
					limit = Integer.parseInt(uri.getQueryParameter(parameterKey));
				} catch(final Throwable ignored) {
				}

			}
		}

		final String[] pathSegments;
		{
			final List<String> pathSegmentsList = uri.getPathSegments();

			final ArrayList<String> pathSegmentsFiltered =
					new ArrayList<>(pathSegmentsList.size());
			for(String segment : pathSegmentsList) {

				while(StringUtils.asciiLowercase(segment).endsWith(".json")
						|| StringUtils.asciiLowercase(segment).endsWith(".xml")) {
					segment = segment.substring(0, segment.lastIndexOf('.'));
				}

				if(!segment.isEmpty()) {
					pathSegmentsFiltered.add(segment);
				}
			}

			pathSegments = pathSegmentsFiltered.toArray(new String[0]);
		}

		final PostSort order;
		if(pathSegments.length > 0) {
			order = PostSort.parse(
					pathSegments[pathSegments.length - 1],
					uri.getQueryParameter("t"));
		} else {
			order = null;
		}

		switch(pathSegments.length) {
			case 0:
				return new SubredditPostListURL(
						Type.FRONTPAGE,
						null,
						null,
						limit,
						before,
						after);

			case 1: {
				if(order != null) {
					return new SubredditPostListURL(
							Type.FRONTPAGE,
							null,
							order,
							limit,
							before,
							after);
				} else {
					return null;
				}
			}

			case 2:
			case 3: {

				if(!pathSegments[0].equals("r")) {
					return null;
				}

				final String subreddit = StringUtils.asciiLowercase(pathSegments[1]);

				if(subreddit.equals("all")) {

					if(pathSegments.length == 2) {
						return new SubredditPostListURL(
								Type.ALL,
								null,
								null,
								limit,
								before,
								after);

					} else if(order != null) {
						return new SubredditPostListURL(
								Type.ALL,
								null,
								order,
								limit,
								before,
								after);

					} else {
						return null;
					}

				} else if(subreddit.equals("popular")) {

					return new SubredditPostListURL(
							Type.POPULAR,
							null,
							order,
							limit,
							before,
							after);

				} else if(subreddit.equals("random") || subreddit.equals("randnsfw")) {

					return new SubredditPostListURL(
							Type.RANDOM,
							subreddit,
							order,
							limit,
							before,
							after);

				} else if(subreddit.matches("all(\\-[\\w\\.]+)+")) {

					if(pathSegments.length == 2) {
						return new SubredditPostListURL(
								Type.ALL_SUBTRACTION,
								subreddit,
								null,
								limit,
								before,
								after);

					} else if(order != null) {
						return new SubredditPostListURL(
								Type.ALL_SUBTRACTION,
								subreddit,
								order,
								limit,
								before,
								after);

					} else {
						return null;
					}

				} else if(subreddit.matches("\\w+(\\+[\\w\\.]+)+")) {

					if(pathSegments.length == 2) {
						return new SubredditPostListURL(
								Type.SUBREDDIT_COMBINATION,
								subreddit,
								null,
								limit,
								before,
								after);

					} else if(order != null) {
						return new SubredditPostListURL(
								Type.SUBREDDIT_COMBINATION,
								subreddit,
								order,
								limit,
								before,
								after);

					} else {
						return null;
					}

				} else if(subreddit.matches("[\\w\\.]+")) {

					if(pathSegments.length == 2) {
						return new SubredditPostListURL(
								Type.SUBREDDIT,
								subreddit,
								null,
								limit,
								before,
								after);

					} else if(order != null) {
						return new SubredditPostListURL(
								Type.SUBREDDIT,
								subreddit,
								order,
								limit,
								before,
								after);

					} else {
						return null;
					}

				} else {
					return null;
				}
			}

			default:
				return null;
		}
	}

	@Override
	public String humanReadablePath() {

		final String path = super.humanReadablePath();

		if(order == null) {
			return path;
		}

		switch(order) {
			case CONTROVERSIAL_HOUR:
			case CONTROVERSIAL_DAY:
			case CONTROVERSIAL_WEEK:
			case CONTROVERSIAL_MONTH:
			case CONTROVERSIAL_YEAR:
			case CONTROVERSIAL_ALL:
			case TOP_HOUR:
			case TOP_DAY:
			case TOP_WEEK:
			case TOP_MONTH:
			case TOP_YEAR:
			case TOP_ALL:
				return path + "?t=" + StringUtils.asciiLowercase(order.name().split("_")[1]);

			default:
				return path;
		}
	}

	@Override
	public String humanReadableName(final Context context, final boolean shorter) {

		switch(type) {

			case FRONTPAGE:
				return context.getString(R.string.mainmenu_frontpage);

			case ALL:
				return context.getString(R.string.mainmenu_all);

			case POPULAR:
				return context.getString(R.string.mainmenu_popular);

			case RANDOM:
				return context.getString("randnsfw".equals(subreddit)
						? R.string.mainmenu_random_nsfw
						: R.string.mainmenu_random);

			case SUBREDDIT:
				try {
					return new SubredditCanonicalId(subreddit).toString();
				} catch(final InvalidSubredditNameException e) {
					return subreddit;
				}

			case SUBREDDIT_COMBINATION:
			case ALL_SUBTRACTION:
				return subreddit;

			default:
				return super.humanReadableName(context, shorter);
		}
	}

	public SubredditPostListURL changeSubreddit(final String newSubreddit) {
		return new SubredditPostListURL(type, newSubreddit, order, limit, before, after);
	}
}
