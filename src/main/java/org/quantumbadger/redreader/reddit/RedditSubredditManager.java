package org.quantumbadger.redreader.reddit;

import android.content.Context;
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.cache.CacheRequest;
import org.quantumbadger.redreader.common.General;

public class RedditSubredditManager {

	private final RedditAccount user;

	private CacheRequest webUpdateRequest = null;

	// TODO need way to cancel web update and start again
	// TODO anonymous user

	private static RedditAccount currentUser;
	private static RedditSubredditManager singleton;

	public static synchronized RedditSubredditManager getInstance(Context context, RedditAccount user) {

		if(singleton == null || !user.equals(currentUser)) {
			currentUser = user;
			singleton = new RedditSubredditManager(context, user);
		}

		return singleton;
	}

	private RedditSubredditManager(Context context, RedditAccount user) {
		this.user = user;
		// TODO load subscriptions
	}

	private static String getDbFilename(RedditAccount user) {
		return General.sha1(user.username.getBytes()) + "_subreddits.db";
	}
}
