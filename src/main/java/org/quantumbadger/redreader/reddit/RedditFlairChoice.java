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

package org.quantumbadger.redreader.reddit;

import android.os.Parcel;
import android.os.Parcelable;
import androidx.annotation.NonNull;
import org.quantumbadger.redreader.common.Optional;
import org.quantumbadger.redreader.jsonwrap.JsonArray;
import org.quantumbadger.redreader.jsonwrap.JsonObject;
import org.quantumbadger.redreader.jsonwrap.JsonValue;

import java.util.ArrayList;
import java.util.List;

public class RedditFlairChoice implements Parcelable {

	@NonNull public final String text;
	@NonNull public final String templateId;

	private RedditFlairChoice(
			@NonNull final String text,
			@NonNull final String templateId) {

		this.text = text;
		this.templateId = templateId;
	}

	@NonNull
	public static Optional<List<RedditFlairChoice>> fromJsonList(@NonNull final JsonArray json) {

		final ArrayList<RedditFlairChoice> result = new ArrayList<>(json.size());

		for(final JsonValue value : json) {

			final JsonObject object = value.asObject();

			if(object == null) {
				return Optional.empty();
			}

			final Optional<RedditFlairChoice> choice = fromJson(object);

			if(choice.isEmpty()) {
				return Optional.empty();
			}

			result.add(choice.get());
		}

		return Optional.of(result);
	}

	@NonNull
	public static Optional<RedditFlairChoice> fromJson(
			@NonNull final JsonObject json) {

		final String flairText = json.getString("flair_text");
		final String flairTemplateId = json.getString("flair_template_id");

		if(flairText == null || flairTemplateId == null) {
			return Optional.empty();
		}

		return Optional.of(new RedditFlairChoice(flairText, flairTemplateId));
	}

	@Override
	public int describeContents() {
		return 0;
	}

	@Override
	public void writeToParcel(final Parcel dest, final int flags) {
		dest.writeString(text);
		dest.writeString(templateId);
	}

	public static final Parcelable.Creator<RedditFlairChoice> CREATOR
			= new Parcelable.Creator<RedditFlairChoice>() {

		@Override
		public RedditFlairChoice createFromParcel(final Parcel in) {

			final String text = in.readString();
			final String templateId = in.readString();

			return new RedditFlairChoice(text, templateId);
		}

		@Override
		public RedditFlairChoice[] newArray(final int size) {
			return new RedditFlairChoice[size];
		}
	};

	@Override
	public String toString() {
		return "RedditFlairChoice(" +
				"text='" + text + '\'' +
				", templateId='" + templateId + '\'' +
				')';
	}

	@Override
	public boolean equals(final Object o) {

		if(this == o) {
			return true;
		}

		if(!(o instanceof RedditFlairChoice)) {
			return false;
		}

		final RedditFlairChoice other = (RedditFlairChoice)o;
		return text.equals(other.text) && templateId.equals(other.templateId);
	}

	@Override
	public int hashCode() {
		return text.hashCode() + 37 * templateId.hashCode();
	}
}
