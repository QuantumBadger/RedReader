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

package org.quantumbadger.redreader.reddit.things;

import android.content.Intent;
import android.os.Parcel;
import android.os.Parcelable;
import androidx.annotation.NonNull;
import androidx.appcompat.app.AppCompatActivity;
import org.apache.commons.text.StringEscapeUtils;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.activities.HtmlViewActivity;
import org.quantumbadger.redreader.common.HasUniqueId;
import org.quantumbadger.redreader.common.ParcelHelper;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.io.WritableObject;
import org.quantumbadger.redreader.jsonwrap.JsonObject;

import java.util.Locale;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class RedditSubreddit implements
		Parcelable,
		Comparable<RedditSubreddit>,
		WritableObject<SubredditCanonicalId>,
		JsonObject.JsonDeserializable,
		HasUniqueId {

	@Override
	public SubredditCanonicalId getKey() {
		try {
			return getCanonicalId();
		} catch(final InvalidSubredditNameException e) {
			throw new RuntimeException(String.format(
					Locale.US,
					"Cannot save subreddit '%s'",
					url), e);
		}
	}

	@Override
	public long getTimestamp() {
		return downloadTime;
	}

	@WritableObjectVersion public static int DB_VERSION = 1;

	@WritableField public String header_img;
	@WritableField public String header_title;
	@WritableField public String description;
	@WritableField public String description_html;
	public String public_description_html;
	@WritableField public String id;
	@WritableField public String name;
	@WritableField public String title;
	@WritableField public String display_name;
	@WritableField public String url;
	@WritableField public long created;
	@WritableField public long created_utc;
	@WritableField public Integer accounts_active;
	@WritableField public Integer subscribers;
	@WritableField public Boolean over18;

	@WritableObjectTimestamp public long downloadTime;

	private static final Pattern NAME_PATTERN = Pattern.compile(
			"((/)?r/)?([\\w\\+\\-\\.:]+)/?");
	private static final Pattern USER_PATTERN = Pattern.compile(
			"/?(u/|user/)([\\w\\+\\-\\.:]+)/?");

	public RedditSubreddit(final CreationData creationData) {
		this();
		downloadTime = creationData.timestamp;
	}

	public static String stripRPrefix(final String name) throws InvalidSubredditNameException {
		final Matcher matcher = NAME_PATTERN.matcher(name);
		if(matcher.matches()) {
			return matcher.group(3);
		} else {
			throw new InvalidSubredditNameException(name);
		}
	}

	public static String stripUserPrefix(final String name) {
		final Matcher matcher = USER_PATTERN.matcher(name);
		if(matcher.matches()) {
			return matcher.group(2);
		} else {
			return null;
		}
	}

	public SubredditCanonicalId getCanonicalId() throws InvalidSubredditNameException {
		return new SubredditCanonicalId(url);
	}

	public String getUrl() {

		if(url != null) {
			return url;
		}

		return "https://reddit.com/r/" + display_name;
	}

	@Override
	public int describeContents() {
		return 0;
	}

	@Override
	public void writeToParcel(final Parcel out, final int flags) {
		out.writeString(header_img);
		out.writeString(header_title);
		out.writeString(description);
		out.writeString(description_html);
		out.writeString(public_description_html);
		out.writeString(id);
		out.writeString(name);
		out.writeString(title);
		out.writeString(display_name);
		out.writeString(url);
		out.writeLong(created);
		out.writeLong(created_utc);
		out.writeInt(accounts_active == null ? -1 : accounts_active);
		out.writeInt(subscribers == null ? -1 : subscribers);
		ParcelHelper.writeNullableBoolean(out, over18);
	}

	public RedditSubreddit() {
	}

	public RedditSubreddit(final String url, final String title, final boolean isSortable) {
		this.url = url;
		this.title = title;
	}

	public RedditSubreddit(final Parcel parcel) {
		header_img = parcel.readString();
		header_title = parcel.readString();
		description = parcel.readString();
		description_html = parcel.readString();
		public_description_html = parcel.readString();
		id = parcel.readString();
		name = parcel.readString();
		title = parcel.readString();
		display_name = parcel.readString();
		url = parcel.readString();
		created = parcel.readLong();
		created_utc = parcel.readLong();

		accounts_active = parcel.readInt();
		subscribers = parcel.readInt();

		if(accounts_active < 0) {
			accounts_active = null;
		}
		if(subscribers < 0) {
			subscribers = null;
		}

		over18 = ParcelHelper.readNullableBoolean(parcel);
	}

	public static final Parcelable.Creator<RedditSubreddit> CREATOR
			= new Parcelable.Creator<RedditSubreddit>() {
		@Override
		public RedditSubreddit createFromParcel(final Parcel in) {
			return new RedditSubreddit(in);
		}

		@Override
		public RedditSubreddit[] newArray(final int size) {
			return new RedditSubreddit[size];
		}
	};

	@Override
	public int compareTo(final RedditSubreddit another) {
		return display_name.compareToIgnoreCase(another.display_name);
	}

	public String getSidebarHtml(final boolean nightMode) {
		final String unescaped = StringEscapeUtils.unescapeHtml4(description_html);

		final StringBuilder result = new StringBuilder(unescaped.length() + 512);

		result.append("<html>");

		result.append("<head>");
		result.append(
				"<meta name=\"viewport\" content=\"width=device-width, user-scalable=yes\">");

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

	public boolean hasSidebar() {
		return description_html != null && !description_html.isEmpty();
	}

	public void showSidebarActivity(final AppCompatActivity context) {

		final Intent intent = new Intent(context, HtmlViewActivity.class);

		intent.putExtra("html", getSidebarHtml(PrefsUtility.isNightMode()));

		intent.putExtra("title", String.format(
				Locale.US, "%s: %s",
				context.getString(R.string.sidebar_activity_title),
				url));

		context.startActivityForResult(intent, 1);
	}

	@NonNull
	@Override
	public String getUniqueId() {
		return id;
	}
}
