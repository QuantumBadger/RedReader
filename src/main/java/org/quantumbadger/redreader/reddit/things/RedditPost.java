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


import android.os.Parcel;
import android.os.Parcelable;
import androidx.annotation.Nullable;
import org.quantumbadger.redreader.common.Optional;
import org.quantumbadger.redreader.common.ParcelUtils;
import org.quantumbadger.redreader.jsonwrap.JsonBoolean;
import org.quantumbadger.redreader.jsonwrap.JsonLong;
import org.quantumbadger.redreader.jsonwrap.JsonObject;
import org.quantumbadger.redreader.jsonwrap.JsonValue;

public final class RedditPost implements
		Parcelable,
		RedditThingWithIdAndType,
		JsonObject.JsonDeserializable {

	public String id;
	public String name;
	public String url;
	public String title;
	public String author;
	public String domain;
	public String subreddit;
	public String subreddit_id;
	public int num_comments;
	public int score;
	public int ups;
	public int downs;
	public int gilded;
	public double upvote_ratio;
	public boolean archived;
	public boolean over_18;
	public boolean hidden;
	public boolean saved;
	public boolean is_self;
	public boolean clicked;
	public boolean stickied;
	public boolean can_mod_post;
	@Nullable public JsonValue edited;
	@Nullable public Boolean likes;
	@Nullable public Boolean spoiler;
	@Nullable public Boolean locked;

	public long created;
	public long created_utc;

	public String selftext;
	public String selftext_html;
	public String permalink;
	public String link_flair_text;
	public String author_flair_text;
	public String thumbnail; // an image URL

	public JsonObject media;
	@Nullable public String rr_internal_dash_url;

	@Nullable public JsonObject preview;
	@Nullable public Boolean is_video;

	@Nullable public String distinguished;
	@Nullable public String suggested_sort;

	public RedditPost() {
	}

	@Nullable
	public String getDashUrl() {

		if(rr_internal_dash_url != null) {
			return rr_internal_dash_url;

		} else if(media != null) {
			try {
				rr_internal_dash_url = media.getObject("reddit_video")
						.getString("fallback_url");

			} catch(final Exception e) {
				rr_internal_dash_url = null;
			}

		}

		return rr_internal_dash_url;
	}

	public String getUrl() {

		if(getDashUrl() != null) {
			return rr_internal_dash_url;
		}

		if(preview != null && url != null && url.contains(".gif")) {

			final Optional<String> mp4Url
					= preview.getStringAtPath("images", 0, "variants", "mp4", "source", "url");

			if(mp4Url.isPresent()) {
				url = mp4Url.get();
			}
		}

		return url;
	}

	// one of the many reasons why the Android API is awful
	private RedditPost(final Parcel in) {
		id = in.readString();
		name = in.readString();
		title = in.readString();
		url = in.readString();
		author = in.readString();
		domain = in.readString();
		subreddit = in.readString();
		subreddit_id = in.readString();
		num_comments = in.readInt();
		score = in.readInt();
		ups = in.readInt();
		downs = in.readInt();
		gilded = in.readInt();
		upvote_ratio = in.readDouble();
		archived = in.readInt() == 1;
		over_18 = in.readInt() == 1;
		hidden = in.readInt() == 1;
		saved = in.readInt() == 1;
		is_self = in.readInt() == 1;
		clicked = in.readInt() == 1;
		stickied = in.readInt() == 1;
		can_mod_post = in.readInt() == 1;

		final long inEdited = in.readLong();
		if(inEdited == -1) {
			edited = JsonBoolean.FALSE;
		} else {
			edited = new JsonLong(inEdited);
		}

		likes = ParcelUtils.readNullableBoolean(in);
		created = in.readLong();
		created_utc = in.readLong();
		selftext = in.readString();
		selftext_html = in.readString();
		permalink = in.readString();
		link_flair_text = in.readString();
		author_flair_text = in.readString();
		thumbnail = in.readString();
		spoiler = ParcelUtils.readNullableBoolean(in);
		locked = ParcelUtils.readNullableBoolean(in);
		rr_internal_dash_url = in.readString();
		distinguished = in.readString();
		suggested_sort = in.readString();
	}

	@Override
	public int describeContents() {
		return 0;
	}

	@Override
	public void writeToParcel(final Parcel parcel, final int flags) {

		parcel.writeString(id);
		parcel.writeString(name);
		parcel.writeString(title);
		parcel.writeString(getUrl());
		parcel.writeString(author);
		parcel.writeString(domain);
		parcel.writeString(subreddit);
		parcel.writeString(subreddit_id);
		parcel.writeInt(num_comments);
		parcel.writeInt(score);
		parcel.writeInt(ups);
		parcel.writeInt(downs);
		parcel.writeInt(gilded);
		parcel.writeDouble(upvote_ratio);
		parcel.writeInt(archived ? 1 : 0);
		parcel.writeInt(over_18 ? 1 : 0);
		parcel.writeInt(hidden ? 1 : 0);
		parcel.writeInt(saved ? 1 : 0);
		parcel.writeInt(is_self ? 1 : 0);
		parcel.writeInt(clicked ? 1 : 0);
		parcel.writeInt(stickied ? 1 : 0);
		parcel.writeInt(can_mod_post ? 1 : 0);

		if(edited instanceof JsonLong) {
			parcel.writeLong(edited.asLong());
		} else {
			parcel.writeLong(-1);
		}

		ParcelUtils.writeNullableBoolean(parcel, likes);
		parcel.writeLong(created);
		parcel.writeLong(created_utc);
		parcel.writeString(selftext);
		parcel.writeString(selftext_html);
		parcel.writeString(permalink);
		parcel.writeString(link_flair_text);
		parcel.writeString(author_flair_text);
		parcel.writeString(thumbnail);
		ParcelUtils.writeNullableBoolean(parcel, spoiler);
		ParcelUtils.writeNullableBoolean(parcel, locked);

		getDashUrl();
		parcel.writeString(rr_internal_dash_url);
		parcel.writeString(distinguished);
		parcel.writeString(suggested_sort);
	}

	public static final Parcelable.Creator<RedditPost> CREATOR
			= new Parcelable.Creator<RedditPost>() {
		@Override
		public RedditPost createFromParcel(final Parcel in) {
			return new RedditPost(in);
		}

		@Override
		public RedditPost[] newArray(final int size) {
			return new RedditPost[size];
		}
	};

	@Override
	public String getIdAlone() {
		return id;
	}

	@Override
	public String getIdAndType() {
		return name;
	}

	public boolean wasEdited() {
		return edited != null && !Boolean.FALSE.equals(edited.asBoolean());
	}
}
