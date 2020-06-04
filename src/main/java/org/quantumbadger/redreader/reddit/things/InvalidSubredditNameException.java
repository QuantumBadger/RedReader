package org.quantumbadger.redreader.reddit.things;

import java.util.Locale;

public final class InvalidSubredditNameException extends Exception {
    public InvalidSubredditNameException(final String subredditName) {
        super(String.format(
        		Locale.US,
				"Invalid subreddit name '%s'", subredditName == null ? "NULL" : subredditName));
    }
}
