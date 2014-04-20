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

package org.quantumbadger.redreader.listingcontrollers;

import android.net.Uri;
import org.quantumbadger.redreader.cache.CacheRequest;
import org.quantumbadger.redreader.fragments.PostListingFragment;
import org.quantumbadger.redreader.reddit.RedditURLParser;
import org.quantumbadger.redreader.reddit.things.RedditSubreddit;

import java.util.UUID;

// TODO add notification/header for abnormal sort order
public class PostListingController {

	private UUID session = null;
	private RedditURLParser.PostListingURL url;

	public void setSession(UUID session) {
		this.session = session;
	}

	public UUID getSession() {
		return session;
	}

	public PostListingController(RedditURLParser.PostListingURL url) {
		this.url = url;
	}

	public boolean isSortable() {
		return url.pathType() == RedditURLParser.PathType.SubredditPostListingURL;
	}

	public static enum Sort {
		HOT, NEW, RISING, TOP_HOUR, TOP_DAY, TOP_WEEK, TOP_MONTH, TOP_YEAR, TOP_ALL, CONTROVERSIAL
	}

	public final void setSort(final Sort order) {
		if(url.pathType() == RedditURLParser.PathType.SubredditPostListingURL) {
			url = url.asSubredditPostListURL().sort(order);
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
		return url.generateUri();
	}

	public final PostListingFragment get(final boolean force) {
		if(force) session = null;
		return PostListingFragment.newInstance(getUri(), session, force ? CacheRequest.DownloadType.FORCE : CacheRequest.DownloadType.IF_NECESSARY);
	}

	public final boolean isSubreddit() {
		return url.pathType() == RedditURLParser.PathType.SubredditPostListingURL
				&& url.asSubredditPostListURL().type == RedditURLParser.SubredditPostListURL.Type.SUBREDDIT;
	}

	public final String subredditCanonicalName() {

		if(url.pathType() == RedditURLParser.PathType.SubredditPostListingURL
				&& url.asSubredditPostListURL().type == RedditURLParser.SubredditPostListURL.Type.SUBREDDIT) {
			try {
				return RedditSubreddit.getCanonicalName(url.asSubredditPostListURL().subreddit);
			} catch(RedditSubreddit.InvalidSubredditNameException e) {
				throw new RuntimeException(e);
			}
		}

		return null;
	}
}
