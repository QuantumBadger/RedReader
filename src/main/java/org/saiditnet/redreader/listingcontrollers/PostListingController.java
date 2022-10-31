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

package org.saiditnet.redreader.listingcontrollers;

import android.content.Context;
import android.net.Uri;
import android.os.Bundle;
import android.preference.PreferenceManager;
import android.support.v7.app.AppCompatActivity;
import org.saiditnet.redreader.common.PrefsUtility;
import org.saiditnet.redreader.fragments.PostListingFragment;
import org.saiditnet.redreader.reddit.PostSort;
import org.saiditnet.redreader.reddit.things.RedditSubreddit;
import org.saiditnet.redreader.reddit.url.PostListingURL;
import org.saiditnet.redreader.reddit.url.RedditURLParser;
import org.saiditnet.redreader.reddit.url.SubredditPostListURL;
import org.saiditnet.redreader.reddit.url.UserPostListingURL;

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

	public PostListingController(PostListingURL url, final Context context) {

		if(url.pathType() == RedditURLParser.SUBREDDIT_POST_LISTING_URL) {
			if(url.asSubredditPostListURL().order == null) {

				PostSort order = defaultOrder(context);

				if(order == PostSort.BEST
						&& url.asSubredditPostListURL().type != SubredditPostListURL.Type.FRONTPAGE) {

					order = PostSort.HOT;
				}

				url = url.asSubredditPostListURL().sort(order);
			}
		} else if(url.pathType() == RedditURLParser.USER_POST_LISTING_URL) {
			if(url.asUserPostListURL().order == null) {
				url = url.asUserPostListURL().sort(PostSort.NEW);
			}
		}

		this.url = url;
	}

	public boolean isSortable() {
		if(url.pathType() == RedditURLParser.USER_POST_LISTING_URL) {
			return (url.asUserPostListURL().type == UserPostListingURL.Type.SUBMITTED);
		}
		return (url.pathType() == RedditURLParser.SUBREDDIT_POST_LISTING_URL)
				|| (url.pathType() == RedditURLParser.MULTIREDDIT_POST_LISTING_URL)
				|| (url.pathType() == RedditURLParser.SEARCH_POST_LISTING_URL);
	}

	public boolean isFrontPage() {
		return url.pathType() == RedditURLParser.SUBREDDIT_POST_LISTING_URL
				&& url.asSubredditPostListURL().type == SubredditPostListURL.Type.FRONTPAGE;
	}

	public final void setSort(final PostSort order) {
		if(url.pathType() == RedditURLParser.SUBREDDIT_POST_LISTING_URL) {
			url = url.asSubredditPostListURL().sort(order);

		} else if(url.pathType() == RedditURLParser.MULTIREDDIT_POST_LISTING_URL) {
			url = url.asMultiredditPostListURL().sort(order);

		} else if(url.pathType() == RedditURLParser.SEARCH_POST_LISTING_URL) {
			url = url.asSearchPostListURL().sort(order);

		} else if(url.pathType() == RedditURLParser.USER_POST_LISTING_URL) {
			url = url.asUserPostListURL().sort(order);

		} else {
			throw new RuntimeException("Cannot set sort for this URL");
		}
	}

	private PostSort defaultOrder(final Context context) {
		return PrefsUtility.pref_behaviour_postsort(context, PreferenceManager.getDefaultSharedPreferences(context));
	}

	public final PostSort getSort() {

		if(url.pathType() == RedditURLParser.SUBREDDIT_POST_LISTING_URL) {
			return url.asSubredditPostListURL().order;
		}

		if(url.pathType() == RedditURLParser.MULTIREDDIT_POST_LISTING_URL) {
			return url.asMultiredditPostListURL().order;
		}

		if(url.pathType() == RedditURLParser.SEARCH_POST_LISTING_URL) {
			return url.asSearchPostListURL().order;
		}

		if(url.pathType() == RedditURLParser.USER_POST_LISTING_URL) {
			return url.asUserPostListURL().order;
		}

		return null;
	}

	public Uri getUri() {
		return url.generateJsonUri();
	}

	public final PostListingFragment get(final AppCompatActivity parent, final boolean force, final Bundle savedInstanceState) {
		if(force) session = null;
		return new PostListingFragment(parent, savedInstanceState, getUri(), session, force);
	}

	public final boolean isSubreddit() {
		return url.pathType() == RedditURLParser.SUBREDDIT_POST_LISTING_URL
				&& url.asSubredditPostListURL().type == SubredditPostListURL.Type.SUBREDDIT;
	}

	public final boolean isRandomSubreddit() {
		return url.pathType() == RedditURLParser.SUBREDDIT_POST_LISTING_URL
				&& url.asSubredditPostListURL().type == SubredditPostListURL.Type.RANDOM;
	}

	public final boolean isSearchResults() {
		return url.pathType() == RedditURLParser.SEARCH_POST_LISTING_URL;
	}

	public final boolean isSubredditSearchResults() {
		return isSearchResults() && url.asSearchPostListURL().subreddit != null;
	}

	public final boolean isUserPostListing() {
		return url.pathType() == RedditURLParser.USER_POST_LISTING_URL;
	}

	public final String subredditCanonicalName() {

		if(url.pathType() == RedditURLParser.SUBREDDIT_POST_LISTING_URL
				&& url.asSubredditPostListURL().type == SubredditPostListURL.Type.SUBREDDIT) {
			try {
				return RedditSubreddit.getCanonicalName(url.asSubredditPostListURL().subreddit);
			} catch(RedditSubreddit.InvalidSubredditNameException e) {
				throw new RuntimeException(e);
			}
		} else if(url.pathType() == RedditURLParser.SEARCH_POST_LISTING_URL
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
