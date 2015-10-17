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

package org.quantumbadger.redreader.listingcontrollers;

import android.app.Activity;
import android.net.Uri;
import org.quantumbadger.redreader.cache.CacheRequest;
import org.quantumbadger.redreader.fragments.PostListingFragment;
import org.quantumbadger.redreader.reddit.things.RedditSubreddit;
import org.quantumbadger.redreader.reddit.url.PostListingURL;
import org.quantumbadger.redreader.reddit.url.RedditURLParser;
import org.quantumbadger.redreader.reddit.url.SubredditPostListURL;

import java.util.UUID;

// TODO add notification/header for abnormal sort order
public class PostListingController {

	private UUID session = null;
	private PostListingURL url;

	public void setSession(UUID session) {
		this.session = session;
	}

	public UUID getSession() {
		return session;
	}

	public PostListingController(PostListingURL url) {
		this.url = url;
	}

	public boolean isSortable() {
		return (url.pathType() == RedditURLParser.PathType.SubredditPostListingURL)
				|| (url.pathType() == RedditURLParser.PathType.SearchPostListingURL);
	}

	public static enum Sort {
		HOT, NEW, RISING, TOP_HOUR, TOP_DAY, TOP_WEEK, TOP_MONTH, TOP_YEAR, TOP_ALL, CONTROVERSIAL,
		// Sorts related to Search Listings
		RELEVANCE, COMMENTS, TOP
	}

	public static Sort parseSort(String string) {
		Sort[] sorts = Sort.values();
		for(Sort sort: sorts)
			if(sort.name().toLowerCase().contentEquals(string))
				return sort;
		return null;
	}

	public final void setSort(final Sort order) {
		if(url.pathType() == RedditURLParser.PathType.SubredditPostListingURL) {
			url = url.asSubredditPostListURL().sort(order);
		} else if(url.pathType() == RedditURLParser.PathType.SearchPostListingURL) {
			url = url.asSearchPostListURL().sort(order);
		} else {
			throw new RuntimeException("Cannot set sort for this URL");
		}
	}

	public final Sort getSort() {

		if(url.pathType() == RedditURLParser.PathType.SubredditPostListingURL) {
			return url.asSubredditPostListURL().order;
		}

		return null;
	}

	public Uri getUri() {
		return url.generateJsonUri();
	}

	public final PostListingFragment get(final Activity parent, final boolean force) {
		if(force) session = null;
		return new PostListingFragment(parent, getUri(), session, force ? CacheRequest.DownloadType.FORCE : CacheRequest.DownloadType.IF_NECESSARY);
	}

	public final boolean isSubreddit() {
		return url.pathType() == RedditURLParser.PathType.SubredditPostListingURL
				&& url.asSubredditPostListURL().type == SubredditPostListURL.Type.SUBREDDIT;
	}

	public final boolean isSearchResults() {
		return url.pathType() == RedditURLParser.PathType.SearchPostListingURL;
	}

	public final boolean isSubredditSearchResults() {
		return isSearchResults() && url.asSearchPostListURL().subreddit != null;
	}

	public final String subredditCanonicalName() {

		if(url.pathType() == RedditURLParser.PathType.SubredditPostListingURL
				&& url.asSubredditPostListURL().type == SubredditPostListURL.Type.SUBREDDIT) {
			try {
				return RedditSubreddit.getCanonicalName(url.asSubredditPostListURL().subreddit);
			} catch(RedditSubreddit.InvalidSubredditNameException e) {
				throw new RuntimeException(e);
			}
		} else if(url.pathType() == RedditURLParser.PathType.SearchPostListingURL
				&& url.asSearchPostListURL().subreddit != null) {
			try {
				return RedditSubreddit.getCanonicalName(url.asSearchPostListURL().subreddit);
			} catch(RedditSubreddit.InvalidSubredditNameException e) {
				throw new RuntimeException(e);
			}
		}

		return null;
	}
}
