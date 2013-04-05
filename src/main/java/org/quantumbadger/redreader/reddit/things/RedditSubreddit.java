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

public class RedditSubreddit implements Parcelable, Comparable<RedditSubreddit> {

	public String header_img, header_title;
	public String description, description_html, public_description;
	public String id, name, title, display_name, url;
	public long created, created_utc;
	public Integer accounts_active, subscribers;
	public boolean over18;
	private final boolean isReal, isSortable;

	public int describeContents() {
		return 0;
	}

	public void writeToParcel(final Parcel out, final int flags) {
		out.writeString(header_img);
		out.writeString(header_title);
		//out.writeString(description); // TODO See if this still gives a speed increase
		//out.writeString(description_html);
		//out.writeString(public_description);
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
		out.writeInt(isReal ? 1 : 0);
		out.writeInt(isSortable ? 1 : 0);
	}

	public RedditSubreddit() {
		isReal = true;
		isSortable = true;
	}

	public boolean isReal() {
		return isReal;
	}

	public RedditSubreddit(String url, String title, final boolean isSortable) {
		this.url = url;
		this.title = title;
		isReal = false;
		this.isSortable = isSortable;
	}

	public RedditSubreddit(final Parcel parcel) {
		header_img = parcel.readString();
		header_title = parcel.readString();
		//description = parcel.readString();
		//description_html = parcel.readString();
		//public_description = parcel.readString();
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

		isReal = parcel.readInt() == 1;
		isSortable = parcel.readInt() == 1;
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
		return display_name.toLowerCase().compareTo(another.display_name.toLowerCase());
	}

	public boolean isSortable() {
		return isSortable;
	}
}
