package org.quantumbadger.redreader.reddit;

import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.common.Constants;
import org.quantumbadger.redreader.reddit.things.SubredditCanonicalId;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;


// Keeps an in-memory list of all known subreddits per account
public class RedditSubredditHistory {

	private static final HashMap<RedditAccount, HashSet<SubredditCanonicalId>> SUBREDDITS = new HashMap<>();

	private static HashSet<SubredditCanonicalId> getForAccount(final RedditAccount account) {

		HashSet<SubredditCanonicalId> result = SUBREDDITS.get(account);

		if(result == null) {
			result = new HashSet<>(Constants.Reddit.DEFAULT_SUBREDDITS);
			SUBREDDITS.put(account, result);
		}

		return result;
	}

	public static synchronized void addSubreddit(
			final RedditAccount account,
			final SubredditCanonicalId id) {

		getForAccount(account).add(id);
	}

	public static synchronized void addSubreddits(
			final RedditAccount account,
			final Collection<SubredditCanonicalId> ids) {

		getForAccount(account).addAll(ids);
	}

	public static synchronized ArrayList<SubredditCanonicalId> getSubredditsSorted(
			final RedditAccount account) {

		final ArrayList<SubredditCanonicalId> result = new ArrayList<>(getForAccount(account));
		Collections.sort(result);
		return result;
	}
}
