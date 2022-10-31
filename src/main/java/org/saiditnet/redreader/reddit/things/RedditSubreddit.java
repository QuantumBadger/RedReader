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

package org.saiditnet.redreader.reddit.things;

import android.os.Parcel;
import android.os.Parcelable;
import org.apache.commons.lang3.StringEscapeUtils;
import org.saiditnet.redreader.common.General;
import org.saiditnet.redreader.common.UnexpectedInternalStateException;
import org.saiditnet.redreader.io.WritableObject;

import java.util.Locale;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class RedditSubreddit implements Parcelable, Comparable<RedditSubreddit>, WritableObject<String> {

	public String getKey() {
		try {
			return getCanonicalName();
		} catch(InvalidSubredditNameException e) {
			throw new UnexpectedInternalStateException(String.format(Locale.US, "Cannot save sub '%s'", url));
		}
	}

	public long getTimestamp() {
		return downloadTime;
	}

	@WritableObjectVersion public static int DB_VERSION = 1;

	public static final class InvalidSubredditNameException extends Exception {
		public InvalidSubredditNameException(String subredditName) {
			super(String.format(Locale.US, "Invalid sub name '%s'.", subredditName == null ? "NULL" : subredditName));
		}
	}

	@WritableField public String header_img, header_title;
	@WritableField public String description, description_html, public_description;
	@WritableField public String id, name, title, display_name, url;
	@WritableField public long created, created_utc;
	@WritableField public Integer accounts_active, subscribers;
	@WritableField public boolean over18;

	@WritableObjectTimestamp public long downloadTime;

	private static final Pattern NAME_PATTERN = Pattern.compile("(/)?(s/)?([\\w\\+\\-\\.:]+)/?");
	private static final Pattern USER_PATTERN = Pattern.compile("(/)?(u/|user/)([\\w\\+\\-\\.:]+)/?");

	public RedditSubreddit(CreationData creationData) {
		this();
		downloadTime = creationData.timestamp;
	}

	public static String stripRPrefix(String name) throws InvalidSubredditNameException {
		final Matcher matcher = NAME_PATTERN.matcher(name);
		if(matcher.matches()) {
			return matcher.group(3);
		} else {
			throw new InvalidSubredditNameException(name);
		}
	}

	public static String stripUserPrefix(String name) {
		final Matcher matcher = USER_PATTERN.matcher(name);
		if(matcher.matches()) {
			return matcher.group(3);
		} else {
			return null;
		}
	}

	/**
	 * @param name a subreddit name in the form "subreddit", "r/subreddit" or "/r/subreddit" (case-insensitive)
	 * @return a subreddit name in the form "/r/subreddit" (lower-cased)
	 * @throws InvalidSubredditNameException if {@code name} is null or not in the expected format
	 */
	public static String getCanonicalName(String name) throws InvalidSubredditNameException {

		final String userSr = stripUserPrefix(name);

		if(userSr != null) {
			return "/user/" + General.asciiLowercase(userSr);
		}

		return "/s/" + General.asciiLowercase(stripRPrefix(name));
	}

	public String getCanonicalName() throws InvalidSubredditNameException {
		return getCanonicalName(url);
	}

	public static String getDisplayNameFromCanonicalName(String canonicalName) {

		if(canonicalName.startsWith("/user/")) {
			return canonicalName;
		}

		return canonicalName.substring(3);
	}

	public int describeContents() {
		return 0;
	}

	public void writeToParcel(final Parcel out, final int flags) {
		out.writeString(header_img);
		out.writeString(header_title);
		out.writeString(description);
		out.writeString(description_html);
		out.writeString(public_description);
		out.writeString(id);
		out.writeString(name);
		out.writeString(title);
		out.writeString(display_name);
		out.writeString(url);
		out.writeLong(created);
		out.writeLong(created_utc);
		out.writeInt(accounts_active == null ? -1 : accounts_active);
		out.writeInt(subscribers == null ? -1 : subscribers);
		out.writeInt(over18 ? 1 : 0);
	}

	public RedditSubreddit() {}

	public RedditSubreddit(String url, String title, final boolean isSortable) {
		this.url = url;
		this.title = title;
	}

	public RedditSubreddit(final Parcel parcel) {
		header_img = parcel.readString();
		header_title = parcel.readString();
		description = parcel.readString();
		description_html = parcel.readString();
		public_description = parcel.readString();
		id = parcel.readString();
		name = parcel.readString();
		title = parcel.readString();
		display_name = parcel.readString();
		url = parcel.readString();
		created = parcel.readLong();
		created_utc = parcel.readLong();

		accounts_active = parcel.readInt();
		subscribers = parcel.readInt();

		if(accounts_active < 0) accounts_active = null;
		if(subscribers < 0) subscribers = null;

		over18 = parcel.readInt() == 1;
	}

	public static final Parcelable.Creator<RedditSubreddit> CREATOR = new Parcelable.Creator<RedditSubreddit>() {
		public RedditSubreddit createFromParcel(final Parcel in) {
			return new RedditSubreddit(in);
		}

		public RedditSubreddit[] newArray(final int size) {
			return new RedditSubreddit[size];
		}
	};

	public int compareTo(final RedditSubreddit another) {
		return General.asciiLowercase(display_name).compareTo(General.asciiLowercase(another.display_name));
	}

	public String getSidebarHtml(boolean nightMode) {
		final String unescaped = StringEscapeUtils.unescapeHtml4(description_html);

		final StringBuilder result = new StringBuilder(unescaped.length() + 512);

		result.append("<html>");

		result.append("<head>");
		result.append("<meta name=\"viewport\" content=\"width=device-width, user-scalable=yes\">");

		if(nightMode) {
			result.append("<style>");
			result.append("body {color: white; background-color: black;}");
			result.append("a {color: #3399FF; background-color: 000033;}");
			result.append("</style>");
		}

		result.append("</head>");

		result.append("<body>");
		result.append(unescaped);
		result.append("</body>");

		result.append("</html>");

		return result.toString();
	}
}
