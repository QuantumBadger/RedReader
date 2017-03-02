package org.quantumbadger.redreader.reddit;

import org.quantumbadger.redreader.common.Constants;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.reddit.things.RedditSubreddit;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;


// Keeps an in-memory list of all known subreddits per account
public class RedditSubredditHistory
{
	private static final HashMap<String, HashSet<String>> SUBREDDITS = new HashMap<>();

	public static synchronized void addSubreddit(final String username, final String name) throws RedditSubreddit.InvalidSubredditNameException
	{
		if (!SUBREDDITS.containsKey(username) || SUBREDDITS.get(username) == null
				|| !SUBREDDITS.get(username).isEmpty()){
			putDefaultSubreddits(username);
		}
		SUBREDDITS.get(username).add(General.asciiLowercase(RedditSubreddit.stripRPrefix(name)));
	}

	public static synchronized ArrayList<String> getSubredditsSorted(final String username)
	{
		if (!SUBREDDITS.containsKey(username) || SUBREDDITS.get(username) == null
				|| !SUBREDDITS.get(username).isEmpty()){
			putDefaultSubreddits(username);
		}
		final ArrayList<String> result = new ArrayList<>(SUBREDDITS.get(username));
		Collections.sort(result);
		return result;
	}

	private static void putDefaultSubreddits(final String username){
		if (!SUBREDDITS.containsKey(username) || SUBREDDITS.get(username) == null) {
			SUBREDDITS.put(username, new HashSet<String>());
		}
		if (SUBREDDITS.get(username).isEmpty()) {
			for (final String subreddit : Constants.Reddit.DEFAULT_SUBREDDITS) {
				try {
					addSubreddit(username, subreddit);
				} catch (final RedditSubreddit.InvalidSubredditNameException e) {
					throw new RuntimeException(e);
				}
			}
		}
	}
}
