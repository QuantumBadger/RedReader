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

import android.net.Uri;
import android.os.Parcel;
import android.os.Parcelable;
import androidx.annotation.Nullable;
import org.apache.commons.lang3.StringEscapeUtils;
import org.quantumbadger.redreader.common.LinkHandler;
import org.quantumbadger.redreader.jsonwrap.JsonValue;
import org.quantumbadger.redreader.reddit.url.PostCommentListingURL;

import java.util.HashSet;


public final class RedditComment implements Parcelable, RedditThingWithIdAndType {

	public String body, body_html, author, subreddit;
	public String author_flair_text;
	public Boolean archived, likes, score_hidden;

	public JsonValue replies;

	public String id, subreddit_id, link_id, parent_id, name, context;

	public int ups, downs;
	public int gilded;

	public Object edited;

	public long created, created_utc;

	public Boolean saved;

	public String distinguished;

	public RedditComment() {}

	// one of the many reasons why the Android API is awful
	private RedditComment(final Parcel in) {

		body = in.readString();
		body_html = in.readString();
		author = in.readString();
		subreddit = in.readString();
		author_flair_text = in.readString();


		archived = in.readInt() == 1;
		switch(in.readInt()) {
			case -1: likes = false; break;
			case 0: likes = null; break;
			case 1: likes = true; break;
		}

		replies = null;

		id = in.readString();
		subreddit_id = in.readString();
		link_id = in.readString();
		parent_id = in.readString();
		name = in.readString();
		context = in.readString();

		ups = in.readInt();
		downs = in.readInt();

		final long in_edited = in.readLong();
		if(in_edited == -1) {
			edited = false;
		} else {
			edited = in_edited;
		}

		created = in.readLong();
		created_utc = in.readLong();

		saved = in.readInt() != 0;
		gilded = in.readInt();

		distinguished = in.readString();
	}

	public void writeToParcel(final Parcel parcel, final int flags) {

		parcel.writeString(body);
		parcel.writeString(body_html);
		parcel.writeString(author);
		parcel.writeString(subreddit);
		parcel.writeString(author_flair_text);
		parcel.writeInt(archived ? 1 : 0);

		if(likes == null) {
			parcel.writeInt(0);
		} else {
			parcel.writeInt(likes ? 1 : -1);
		}

		parcel.writeString(id);
		parcel.writeString(subreddit_id);
		parcel.writeString(link_id);
		parcel.writeString(parent_id);
		parcel.writeString(name);
		parcel.writeString(context);

		parcel.writeInt(ups);
		parcel.writeInt(downs);

		if(edited instanceof Long) {
			parcel.writeLong((Long)edited);
		} else {
			parcel.writeLong(-1);
		}

		parcel.writeLong(created);
		parcel.writeLong(created_utc);

		parcel.writeInt(saved ? 1 : 0);
		parcel.writeInt(gilded);

		parcel.writeString(distinguished);
	}

	@Override
	public String getIdAlone() {
		return id;
	}

	@Override
	public String getIdAndType() {
		return name;
	}

	public boolean isArchived() {
		return Boolean.TRUE.equals(archived);
	}

	@Nullable
	public PostCommentListingURL getContextUrl() {

		if(context != null) {

			String rawContextUrl = context;

			if(rawContextUrl.startsWith("r/")) {
				rawContextUrl = "/" + rawContextUrl;
			}

			if(rawContextUrl.startsWith("/")) {
				rawContextUrl = "https://reddit.com" + rawContextUrl;
			}

			return PostCommentListingURL.parse(Uri.parse(rawContextUrl));

		} else {
			return new PostCommentListingURL(
				null,
				link_id,
				getIdAlone(),
				3,
				null,
				null);
		}
	}

	public int describeContents() {
		return 0;
	}

	public static final Parcelable.Creator<RedditComment> CREATOR = new Parcelable.Creator<RedditComment>() {
		public RedditComment createFromParcel(final Parcel in) {
			return new RedditComment(in);
		}

		public RedditComment[] newArray(final int size) {
			return new RedditComment[size];
		}
	};

	public HashSet<String> computeAllLinks() {
		return LinkHandler.computeAllLinks(StringEscapeUtils.unescapeHtml4(body_html));
	}
}
