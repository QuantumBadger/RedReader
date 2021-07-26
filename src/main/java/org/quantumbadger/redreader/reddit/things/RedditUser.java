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
import org.quantumbadger.redreader.jsonwrap.JsonObject;

public class RedditUser implements Parcelable, JsonObject.JsonDeserializable {

	public int comment_karma;
	public int link_karma;

	public long created;
	public long created_utc;

	public Boolean has_mail;
	public Boolean has_mod_mail;
	public boolean is_friend;
	public boolean is_gold;
	public boolean is_mod;
	public boolean over_18;

	public String id;
	public String modhash;
	public String name;

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
		modhash = in.readString();
		name = in.readString();
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
		parcel.writeString(modhash);
		parcel.writeString(name);
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
