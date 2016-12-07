package org.quantumbadger.redreader.reddit;

import org.quantumbadger.redreader.common.Constants;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.reddit.things.RedditSubreddit;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;


// Keeps an in-memory list of all known subreddits
public class RedditSubredditHistory
{
	private static final HashSet<String> SUBREDDITS = new HashSet<>();

	static
	{
		for(final String subreddit : Constants.Reddit.DEFAULT_SUBREDDITS)
		{
			try
			{
				addSubreddit(subreddit);
			}
			catch(final RedditSubreddit.InvalidSubredditNameException e)
			{
				throw new RuntimeException(e);
			}
		}
	}

	public static synchronized void addSubreddit(final String name) throws RedditSubreddit.InvalidSubredditNameException
	{
		SUBREDDITS.add(General.asciiLowercase(RedditSubreddit.stripRPrefix(name)));
	}

	public static synchronized ArrayList<String> getSubredditsSorted()
	{
		final ArrayList<String> result = new ArrayList<>(SUBREDDITS);
		Collections.sort(result);
		return result;
	}
}
