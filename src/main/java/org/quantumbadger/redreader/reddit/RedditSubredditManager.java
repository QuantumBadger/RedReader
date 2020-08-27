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

package org.quantumbadger.redreader.reddit;

import android.content.Context;
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.TimestampBound;
import org.quantumbadger.redreader.io.RawObjectDB;
import org.quantumbadger.redreader.io.RequestResponseHandler;
import org.quantumbadger.redreader.io.ThreadedRawObjectDB;
import org.quantumbadger.redreader.io.UpdatedVersionListener;
import org.quantumbadger.redreader.io.WeakCache;
import org.quantumbadger.redreader.reddit.api.RedditAPIIndividualSubredditDataRequester;
import org.quantumbadger.redreader.reddit.api.SubredditRequestFailure;
import org.quantumbadger.redreader.reddit.things.RedditSubreddit;
import org.quantumbadger.redreader.reddit.things.SubredditCanonicalId;

import java.util.Collection;
import java.util.HashMap;

public class RedditSubredditManager {

	public void offerRawSubredditData(
			final Collection<RedditSubreddit> toWrite,
			final long timestamp) {
		subredditCache.performWrite(toWrite);
	}

	// TODO need way to cancel web update and start again?
	// TODO anonymous user

	// TODO Ability to temporarily flag subreddits as subscribed/unsubscribed
	// TODO Ability to temporarily add/remove subreddits from multireddits

	// TODO store favourites in preference

	public enum SubredditListType {
		SUBSCRIBED,
		MODERATED,
		MULTIREDDITS,
		MOST_POPULAR,
		DEFAULTS
	}

	private static RedditSubredditManager singleton;
	private static RedditAccount singletonUser;

	private final WeakCache<SubredditCanonicalId, RedditSubreddit, SubredditRequestFailure>
			subredditCache;

	public static synchronized RedditSubredditManager getInstance(
			final Context context,
			final RedditAccount user) {

		if(singleton == null || !user.equals(singletonUser)) {
			singletonUser = user;
			singleton = new RedditSubredditManager(context, user);
		}

		return singleton;
	}

	private RedditSubredditManager(final Context context, final RedditAccount user) {

		// Subreddit cache

		final RawObjectDB<SubredditCanonicalId, RedditSubreddit> subredditDb
				= new RawObjectDB<>(
				context,
				getDbFilename("subreddits", user),
				RedditSubreddit.class);

		final ThreadedRawObjectDB<SubredditCanonicalId, RedditSubreddit, SubredditRequestFailure>
				subredditDbWrapper
				= new ThreadedRawObjectDB<>(
				subredditDb,
				new RedditAPIIndividualSubredditDataRequester(context, user));

		subredditCache = new WeakCache<>(subredditDbWrapper);
	}

	private static String getDbFilename(final String type, final RedditAccount user) {
		return General.sha1(user.username.getBytes()) + "_" + type + "_subreddits.db";
	}

	public void getSubreddit(
			final SubredditCanonicalId subredditCanonicalId,
			final TimestampBound timestampBound,
			final RequestResponseHandler<RedditSubreddit, SubredditRequestFailure> handler,
			final UpdatedVersionListener<
					SubredditCanonicalId, RedditSubreddit> updatedVersionListener) {

		subredditCache.performRequest(
				subredditCanonicalId,
				timestampBound,
				handler,
				updatedVersionListener);
	}

	public void getSubreddits(
			final Collection<SubredditCanonicalId> ids,
			final TimestampBound timestampBound,
			final RequestResponseHandler<
					HashMap<SubredditCanonicalId, RedditSubreddit>,
					SubredditRequestFailure> handler) {

		subredditCache.performRequest(ids, timestampBound, handler);
	}
}
