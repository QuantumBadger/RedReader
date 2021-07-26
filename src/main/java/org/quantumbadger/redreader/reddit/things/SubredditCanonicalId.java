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
import org.quantumbadger.redreader.common.StringUtils;
import org.quantumbadger.redreader.jsonwrap.JsonObject;

public class SubredditCanonicalId implements
		Comparable<SubredditCanonicalId>,
		Parcelable,
		JsonObject.JsonDeserializable {

	@NonNull private final String mId;

	public SubredditCanonicalId(@NonNull String name) throws InvalidSubredditNameException {

		name = StringUtils.asciiLowercase(name.trim());
		final String userSr = RedditSubreddit.stripUserPrefix(name);

		if(userSr != null) {
			mId = "/user/" + userSr;
		} else {
			mId = "/r/" + RedditSubreddit.stripRPrefix(name);
		}
	}

	public static final Creator<SubredditCanonicalId> CREATOR
			= new Creator<SubredditCanonicalId>() {

		@Override
		public SubredditCanonicalId createFromParcel(final Parcel in) {
			try {
				return new SubredditCanonicalId(in.readString());
			} catch(final InvalidSubredditNameException e) {
				throw new RuntimeException(e);
			}
		}

		@Override
		public SubredditCanonicalId[] newArray(final int size) {
			return new SubredditCanonicalId[size];
		}
	};

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

	@Override
	public int describeContents() {
		return 0;
	}

	@Override
	public void writeToParcel(final Parcel dest, final int flags) {
		dest.writeString(mId);
	}
}
