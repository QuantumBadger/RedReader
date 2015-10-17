/*******************************************************************************
 * This file is part of Scroll.
 *
 * Scroll is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Scroll is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Scroll.  If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/

package com.konneh.scroll.reddit.url;

import android.content.Context;
import android.net.Uri;

public class RedditURLParser {

	public enum PathType {
		SubredditPostListingURL,
		UserPostListingURL,
		SearchPostListingURL,
		UnknownPostListingURL,
		UserProfileURL,
		UserCommentListingURL,
		UnknownCommentListingURL,
		PostCommentListingURL
	}

	private static boolean isRedditUri(Uri uri) {

		if(uri == null || uri.getHost() == null) return false;
		final String[] hostSegments = uri.getHost().toLowerCase().split("\\.");
		if(hostSegments.length < 2) return false;
		if(hostSegments[hostSegments.length - 1].equals("com") && hostSegments[hostSegments.length - 2].equals("reddit")) return true;
		if(hostSegments[hostSegments.length - 1].equals("it") && hostSegments[hostSegments.length - 2].equals("redd")) return true;

		return false;
	}

	public static RedditURL parse(Uri uri) {

		if(uri == null) return null;
		if(!isRedditUri(uri)) return null;

		{
			final SubredditPostListURL subredditPostListURL = SubredditPostListURL.parse(uri);
			if(subredditPostListURL != null) {
				return subredditPostListURL;
			}
		}

		{
			final SearchPostListURL searchPostListURL = SearchPostListURL.parse(uri);
			if(searchPostListURL != null) {
				return searchPostListURL;
			}
		}

		{
			final UserPostListingURL userPostListURL = UserPostListingURL.parse(uri);
			if(userPostListURL != null) {
				return userPostListURL;
			}
		}

		{
			final UserCommentListingURL userCommentListURL = UserCommentListingURL.parse(uri);
			if(userCommentListURL != null) {
				return userCommentListURL;
			}
		}

		{
			final PostCommentListingURL commentListingURL = PostCommentListingURL.parse(uri);
			if(commentListingURL != null) {
				return commentListingURL;
			}
		}

		{
			final UserProfileURL userProfileURL = UserProfileURL.parse(uri);
			if(userProfileURL != null) {
				return userProfileURL;
			}
		}

		return null;
	}

	public static RedditURL parseProbableCommentListing(Uri uri) {

		RedditURL matchURL = parse(uri);
		if(matchURL != null) return matchURL;

		return new UnknownCommentListURL(uri);
	}

	public static RedditURL parseProbablePostListing(Uri uri) {

		RedditURL matchURL = parse(uri);
		if(matchURL != null) return matchURL;

		return new UnknownPostListURL(uri);
	}

	public static abstract class RedditURL {
		public abstract Uri generateJsonUri();
		public abstract PathType pathType();

		public final SubredditPostListURL asSubredditPostListURL() {
			return (SubredditPostListURL)this;
		}

		public final SearchPostListURL asSearchPostListURL() {
			return (SearchPostListURL)this;
		}

		public final UserPostListingURL asUserPostListURL() {
			return (UserPostListingURL)this;
		}

		public UserProfileURL asUserProfileURL() {
			return (UserProfileURL)this;
		}

		public PostCommentListingURL asPostCommentListURL() {
			return (PostCommentListingURL)this;
		}

		public UserCommentListingURL asUserCommentListURL() {
			return (UserCommentListingURL)this;
		}

		public String humanReadableName(Context context, boolean shorter) {
			return humanReadablePath();
		}

		public String humanReadableUrl() {
			return "reddit.com" + humanReadablePath();
		}

		public String humanReadablePath() {

			final Uri src = generateJsonUri();

			final StringBuilder builder = new StringBuilder();

			for(String pathElement : src.getPathSegments()) {
				if(!pathElement.equals(".json")) {
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
