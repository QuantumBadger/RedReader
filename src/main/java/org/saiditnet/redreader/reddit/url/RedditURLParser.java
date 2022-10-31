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

package org.saiditnet.redreader.reddit.url;

import android.content.Context;
import android.net.Uri;
import android.support.annotation.IntDef;
import org.saiditnet.redreader.common.General;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

public class RedditURLParser {

	public static final int SUBREDDIT_POST_LISTING_URL = 0;
	public static final int USER_POST_LISTING_URL = 1;
	public static final int SEARCH_POST_LISTING_URL = 2;
	public static final int UNKNOWN_POST_LISTING_URL = 3;
	public static final int USER_PROFILE_URL = 4;
	public static final int USER_COMMENT_LISTING_URL = 5;
	public static final int UNKNOWN_COMMENT_LISTING_URL = 6;
	public static final int POST_COMMENT_LISTING_URL = 7;
	public static final int MULTIREDDIT_POST_LISTING_URL = 8;

	@IntDef({SUBREDDIT_POST_LISTING_URL, USER_POST_LISTING_URL, SEARCH_POST_LISTING_URL,
		UNKNOWN_POST_LISTING_URL, USER_PROFILE_URL, USER_COMMENT_LISTING_URL,
		UNKNOWN_COMMENT_LISTING_URL, POST_COMMENT_LISTING_URL, MULTIREDDIT_POST_LISTING_URL})
	@Retention(RetentionPolicy.SOURCE)
	public @interface PathType {}

	private static boolean isRedditUri(Uri uri) {

		if (uri == null || uri.getHost() == null) return false;
		final String[] hostSegments = General.asciiLowercase(uri.getHost()).split("\\.");
		if (hostSegments.length < 2) return false;
		if (hostSegments[hostSegments.length - 1].equals("net") && hostSegments[hostSegments.length - 2].equals("saidit"))
			return true;
		// if (hostSegments[hostSegments.length - 1].equals("it") && hostSegments[hostSegments.length - 2].equals("redd"))
		// 	return true;

		return false;
	}

	public static RedditURL parse(Uri uri) {

		if (uri == null) return null;
		if (!isRedditUri(uri)) return null;

		{
			final SubredditPostListURL subredditPostListURL = SubredditPostListURL.parse(uri);
			if (subredditPostListURL != null) {
				return subredditPostListURL;
			}
		}

		{
			final MultiredditPostListURL multiredditPostListURL = MultiredditPostListURL.parse(uri);
			if (multiredditPostListURL != null) {
				return multiredditPostListURL;
			}
		}

		{
			final SearchPostListURL searchPostListURL = SearchPostListURL.parse(uri);
			if (searchPostListURL != null) {
				return searchPostListURL;
			}
		}

		{
			final UserPostListingURL userPostListURL = UserPostListingURL.parse(uri);
			if (userPostListURL != null) {
				return userPostListURL;
			}
		}

		{
			final UserCommentListingURL userCommentListURL = UserCommentListingURL.parse(uri);
			if (userCommentListURL != null) {
				return userCommentListURL;
			}
		}

		{
			final PostCommentListingURL commentListingURL = PostCommentListingURL.parse(uri);
			if (commentListingURL != null) {
				return commentListingURL;
			}
		}

		{
			final UserProfileURL userProfileURL = UserProfileURL.parse(uri);
			if (userProfileURL != null) {
				return userProfileURL;
			}
		}

		return null;
	}

	public static RedditURL parseProbableCommentListing(Uri uri) {

		RedditURL matchURL = parse(uri);
		if (matchURL != null) return matchURL;

		return new UnknownCommentListURL(uri);
	}

	public static RedditURL parseProbablePostListing(Uri uri) {

		RedditURL matchURL = parse(uri);
		if (matchURL != null) return matchURL;

		return new UnknownPostListURL(uri);
	}

	public static abstract class RedditURL {
		public abstract Uri generateJsonUri();

		public abstract @PathType int pathType();

		public final SubredditPostListURL asSubredditPostListURL() {
			return (SubredditPostListURL) this;
		}

		public final MultiredditPostListURL asMultiredditPostListURL() {
			return (MultiredditPostListURL) this;
		}

		public final SearchPostListURL asSearchPostListURL() {
			return (SearchPostListURL) this;
		}

		public final UserPostListingURL asUserPostListURL() {
			return (UserPostListingURL) this;
		}

		public UserProfileURL asUserProfileURL() {
			return (UserProfileURL) this;
		}

		public PostCommentListingURL asPostCommentListURL() {
			return (PostCommentListingURL) this;
		}

		public UserCommentListingURL asUserCommentListURL() {
			return (UserCommentListingURL) this;
		}

		public String humanReadableName(Context context, boolean shorter) {
			return humanReadablePath();
		}

		public String humanReadableUrl() {
			return "saidit.net" + humanReadablePath();
		}

		public String humanReadablePath() {

			final Uri src = generateJsonUri();

			final StringBuilder builder = new StringBuilder();

			for (String pathElement : src.getPathSegments()) {
				if (!pathElement.equals(".json")) {
					builder.append("/");
					builder.append(pathElement);
				}
			}

			return builder.toString();
		}

		@Override
		public String toString() {
			return generateJsonUri().toString();
		}
	}
}
