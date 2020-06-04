package org.quantumbadger.redreader.reddit.things;

import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import org.quantumbadger.redreader.common.General;

public class SubredditCanonicalId implements Comparable<SubredditCanonicalId> {

	@NonNull private final String mId;

	public SubredditCanonicalId(@NonNull final String name) throws InvalidSubredditNameException {

		final String userSr = RedditSubreddit.stripUserPrefix(name);

		if(userSr != null) {
			mId = "/user/" + General.asciiLowercase(userSr);
		} else {
			mId = "/r/" + General.asciiLowercase(RedditSubreddit.stripRPrefix(name));
		}
	}

	public String getDisplayNameLowercase() {

		if(mId.startsWith("/user/")) {
			return mId;
		}

		return mId.substring(3);
	}

	@NonNull
	@Override
	public String toString() {
		return mId;
	}

	@Override
	public int hashCode() {
		return mId.hashCode();
	}

	@Override
	public boolean equals(@Nullable final Object obj) {

		if(this == obj) {
			return true;
		}

		if(!(obj instanceof SubredditCanonicalId)) {
			return false;
		}

		return ((SubredditCanonicalId)obj).mId.equals(mId);
	}

	@Override
	public int compareTo(final SubredditCanonicalId o) {
		return mId.compareTo(o.mId);
	}
}
