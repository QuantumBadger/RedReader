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

package org.saiditnet.redreader.reddit;

import android.content.Context;
import org.saiditnet.redreader.account.RedditAccount;
import org.saiditnet.redreader.common.General;
import org.saiditnet.redreader.common.TimestampBound;
import org.saiditnet.redreader.io.*;
import org.saiditnet.redreader.reddit.api.RedditAPIIndividualSubredditDataRequester;
import org.saiditnet.redreader.reddit.api.SubredditRequestFailure;
import org.saiditnet.redreader.reddit.things.RedditSubreddit;

import java.util.Collection;
import java.util.HashMap;

public class RedditSubredditManager {

	public void offerRawSubredditData(Collection<RedditSubreddit> toWrite, long timestamp) {
		subredditCache.performWrite(toWrite);
	}

	// TODO need way to cancel web update and start again?
	// TODO anonymous user

	// TODO Ability to temporarily flag subreddits as subscribed/unsubscribed
	// TODO Ability to temporarily add/remove subreddits from multireddits

	// TODO store favourites in preference

	public enum SubredditListType { SUBSCRIBED, MODERATED, MULTIREDDITS, MOST_POPULAR, DEFAULTS }

	private static RedditSubredditManager singleton;
	private static RedditAccount singletonUser;

	private final WeakCache<String, RedditSubreddit, SubredditRequestFailure> subredditCache;

	public static synchronized RedditSubredditManager getInstance(Context context, RedditAccount user) {

		if(singleton == null || !user.equals(singletonUser)) {
			singletonUser = user;
			singleton = new RedditSubredditManager(context, user);
		}

		return singleton;
	}

	private RedditSubredditManager(Context context, RedditAccount user) {

		// Subreddit cache

		final RawObjectDB<String, RedditSubreddit> subredditDb
				= new RawObjectDB<>(context, getDbFilename("subreddits", user), RedditSubreddit.class);

		final ThreadedRawObjectDB<String, RedditSubreddit, SubredditRequestFailure> subredditDbWrapper
				= new ThreadedRawObjectDB<>(subredditDb, new RedditAPIIndividualSubredditDataRequester(context, user));

		subredditCache = new WeakCache<>(subredditDbWrapper);
	}

	private static String getDbFilename(String type, RedditAccount user) {
		return General.sha1(user.username.getBytes()) + "_" + type + "_subreddits.db";
	}

	public void getSubreddit(String subredditCanonicalId,
							 TimestampBound timestampBound,
							 RequestResponseHandler<RedditSubreddit, SubredditRequestFailure> handler,
							 UpdatedVersionListener<String, RedditSubreddit> updatedVersionListener) {

		final String subredditDisplayName = RedditSubreddit.getDisplayNameFromCanonicalName(subredditCanonicalId);
		subredditCache.performRequest(subredditDisplayName, timestampBound, handler, updatedVersionListener);
	}

	public void getSubreddits(Collection<String> ids,
							 TimestampBound timestampBound,
							 RequestResponseHandler<HashMap<String, RedditSubreddit>, SubredditRequestFailure> handler) {

		subredditCache.performRequest(ids, timestampBound, handler);
	}
}
