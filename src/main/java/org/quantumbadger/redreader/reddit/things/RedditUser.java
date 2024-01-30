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
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import org.apache.commons.text.StringEscapeUtils;
import org.quantumbadger.redreader.common.ParcelUtils;
import org.quantumbadger.redreader.jsonwrap.JsonObject;

public class RedditUser implements Parcelable, JsonObject.JsonDeserializable {

	@Nullable public Integer comment_karma;
	@Nullable public Integer link_karma;

	@Nullable public Long created;
	@Nullable public Long created_utc;

	@Nullable public Boolean has_mail;
	@Nullable public Boolean has_mod_mail;
	@Nullable public Boolean is_friend;
	@Nullable public Boolean is_gold;
	@Nullable public Boolean is_mod;
	@Nullable public Boolean is_suspended;
	@Nullable public Boolean over_18;
	@Nullable public Boolean is_followed;

	@Nullable public String id;
	@NonNull public String name;
	@Nullable public String icon_img;

	@Nullable public Boolean is_employee;

	@Override
	public int describeContents() {
		return 0;
	}

	public RedditUser() {
	}

	// one of the many reasons why the Android API is awful
	private RedditUser(final Parcel in) {

		comment_karma = in.readInt();
		link_karma = in.readInt();

		created = in.readLong();
		created_utc = in.readLong();

		final int inHasMail = in.readInt();
		if(inHasMail == 0) {
			has_mail = null;
		} else {
			has_mail = inHasMail == 1;
		}

		final int inHasModMail = in.readInt();
		if(inHasModMail == 0) {
			has_mod_mail = null;
		} else {
			has_mod_mail = inHasModMail == 1;
		}

		is_friend = in.readInt() == 1;
		is_gold = in.readInt() == 1;
		is_mod = in.readInt() == 1;
		over_18 = in.readInt() == 1;

		id = in.readString();
		name = in.readString();
		icon_img = in.readString();

		is_employee = ParcelUtils.readNullableBoolean(in);
	}

	@Override
	public void writeToParcel(final Parcel parcel, final int flags) {

		parcel.writeInt(comment_karma);
		parcel.writeInt(link_karma);

		parcel.writeLong(created);
		parcel.writeLong(created_utc);

		if(has_mail == null) {
			parcel.writeInt(0);
		} else {
			parcel.writeInt(has_mail ? 1 : -1);
		}

		if(has_mod_mail == null) {
			parcel.writeInt(0);
		} else {
			parcel.writeInt(has_mod_mail ? 1 : -1);
		}

		parcel.writeInt(is_friend ? 1 : 0);
		parcel.writeInt(is_gold ? 1 : 0);
		parcel.writeInt(is_mod ? 1 : 0);
		parcel.writeInt(over_18 ? 1 : 0);

		parcel.writeString(id);
		parcel.writeString(name);
		parcel.writeString(icon_img);

		ParcelUtils.writeNullableBoolean(parcel, is_employee);
	}

	@Nullable
	public String getIconUrl() {
		if(icon_img == null) {
			return null;
		} else {
			return StringEscapeUtils.unescapeHtml4(icon_img);
		}
	}

	public static final Parcelable.Creator<RedditUser> CREATOR
			= new Parcelable.Creator<RedditUser>() {
		@Override
		public RedditUser createFromParcel(final Parcel in) {
			return new RedditUser(in);
		}

		@Override
		public RedditUser[] newArray(final int size) {
			return new RedditUser[size];
		}
	};
}
