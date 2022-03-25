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
import org.apache.commons.text.StringEscapeUtils;
import org.quantumbadger.redreader.common.LinkHandler;
import org.quantumbadger.redreader.common.ParcelHelper;
import org.quantumbadger.redreader.jsonwrap.JsonBoolean;
import org.quantumbadger.redreader.jsonwrap.JsonLong;
import org.quantumbadger.redreader.jsonwrap.JsonObject;
import org.quantumbadger.redreader.jsonwrap.JsonValue;
import org.quantumbadger.redreader.reddit.url.PostCommentListingURL;

import java.util.HashSet;


public final class RedditComment implements
		Parcelable,
		RedditThingWithIdAndType,
		JsonObject.JsonDeserializable {

	public String body;
	public String body_html;
	public String author;
	public String subreddit;
	public String author_flair_text;
	public Boolean archived;
	public Boolean likes;
	public Boolean score_hidden;
	public Boolean locked;
	public Boolean can_mod_post;

	public JsonValue replies;

	public String id;
	public String subreddit_id;
	public String link_id;
	public String parent_id;
	public String name;
	public String context;

	public int ups;
	public int downs;
	public int gilded;
	public int controversiality;

	@Nullable public JsonValue edited;

	public long created;
	public long created_utc;

	@Nullable public Boolean saved;

	@Nullable public String distinguished;

	@Nullable public Boolean stickied;

	public RedditComment() {
	}

	private RedditComment(final Parcel in) {

		body = in.readString();
		body_html = in.readString();
		author = in.readString();
		subreddit = in.readString();
		author_flair_text = in.readString();

		archived = ParcelHelper.readNullableBoolean(in);
		likes = ParcelHelper.readNullableBoolean(in);
		score_hidden = ParcelHelper.readNullableBoolean(in);
		locked = ParcelHelper.readNullableBoolean(in);
		can_mod_post = ParcelHelper.readNullableBoolean(in);

		replies = null;

		id = in.readString();
		subreddit_id = in.readString();
		link_id = in.readString();
		parent_id = in.readString();
		name = in.readString();
		context = in.readString();

		ups = in.readInt();
		downs = in.readInt();

		final long inEdited = in.readLong();
		if(inEdited == -1) {
			edited = JsonBoolean.FALSE;
		} else {
			edited = new JsonLong(inEdited);
		}

		created = in.readLong();
		created_utc = in.readLong();

		saved = ParcelHelper.readNullableBoolean(in);
		gilded = in.readInt();
		controversiality = in.readInt();

		distinguished = in.readString();

		stickied = ParcelHelper.readNullableBoolean(in);
	}

	@Override
	public void writeToParcel(final Parcel parcel, final int flags) {

		parcel.writeString(body);
		parcel.writeString(body_html);
		parcel.writeString(author);
		parcel.writeString(subreddit);
		parcel.writeString(author_flair_text);

		ParcelHelper.writeNullableBoolean(parcel, archived);
		ParcelHelper.writeNullableBoolean(parcel, likes);
		ParcelHelper.writeNullableBoolean(parcel, score_hidden);
		ParcelHelper.writeNullableBoolean(parcel, locked);
		ParcelHelper.writeNullableBoolean(parcel, can_mod_post);

		parcel.writeString(id);
		parcel.writeString(subreddit_id);
		parcel.writeString(link_id);
		parcel.writeString(parent_id);
		parcel.writeString(name);
		parcel.writeString(context);

		parcel.writeInt(ups);
		parcel.writeInt(downs);

		if(edited instanceof JsonLong) {
			parcel.writeLong(edited.asLong());
		} else {
			parcel.writeLong(-1);
		}

		parcel.writeLong(created);
		parcel.writeLong(created_utc);

		ParcelHelper.writeNullableBoolean(parcel, saved);
		parcel.writeInt(gilded);
		parcel.writeInt(controversiality);

		parcel.writeString(distinguished);

		ParcelHelper.writeNullableBoolean(parcel, stickied);
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

	public boolean isLocked() {
		return Boolean.TRUE.equals(locked);
	}

	public boolean canModerate() {
		return Boolean.TRUE.equals(can_mod_post);
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

	@Override
	public int describeContents() {
		return 0;
	}

	public static final Parcelable.Creator<RedditComment> CREATOR
			= new Parcelable.Creator<RedditComment>() {
		@Override
		public RedditComment createFromParcel(final Parcel in) {
			return new RedditComment(in);
		}

		@Override
		public RedditComment[] newArray(final int size) {
			return new RedditComment[size];
		}
	};

	public HashSet<String> computeAllLinks() {
		return LinkHandler.computeAllLinks(StringEscapeUtils.unescapeHtml4(body_html));
	}

	public boolean wasEdited() {
		return edited != null && !Boolean.FALSE.equals(edited.asBoolean());
	}

	public boolean isControversial() {
		return controversiality == 1;
	}
}
