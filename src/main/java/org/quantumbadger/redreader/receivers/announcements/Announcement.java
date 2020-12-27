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

package org.quantumbadger.redreader.receivers.announcements;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import org.quantumbadger.redreader.common.RRTime;

import java.io.IOException;

public class Announcement {

	private static final String ENTRY_ID = "i";
	private static final String ENTRY_TITLE = "t";
	private static final String ENTRY_MESSAGE = "m";
	private static final String ENTRY_URL = "u";
	private static final String ENTRY_SHOW_UNTIL = "until";

	@NonNull public final String id;

	@NonNull public final String title;
	@Nullable public final String message;

	@NonNull public final String url;

	public final long showUntilUtcMillis;

	private Announcement(
			@NonNull final String id,
			@NonNull final String title,
			@Nullable final String message,
			@NonNull final String url,
			final long showUntilUtcMillis) {
		this.id = id;

		this.title = title;
		this.message = message;
		this.url = url;
		this.showUntilUtcMillis = showUntilUtcMillis;
	}

	@NonNull
	public static Announcement create(
			@NonNull final String id,
			@NonNull final String title,
			@Nullable final String message,
			@NonNull final String url,
			final long durationMs) {

		return new Announcement(
				id,
				title,
				message,
				url,
				RRTime.utcCurrentTimeMillis() + durationMs);
	}

	public boolean isExpired() {
		return showUntilUtcMillis < RRTime.utcCurrentTimeMillis();
	}

	@NonNull
	public Payload toPayload() {

		final Payload result = new Payload();

		result.setString(ENTRY_ID, id);
		result.setString(ENTRY_TITLE, title);

		if(message != null) {
			result.setString(ENTRY_MESSAGE, message);
		}

		result.setString(ENTRY_URL, url);
		result.setLong(ENTRY_SHOW_UNTIL, showUntilUtcMillis);

		return result;
	}

	@NonNull
	public static Announcement fromPayload(@NonNull final Payload payload) throws IOException {

		String id = payload.getString(ENTRY_ID);
		final String title = payload.getString(ENTRY_TITLE);
		final String message = payload.getString(ENTRY_MESSAGE);
		final String url = payload.getString(ENTRY_URL);
		final Long showUntil = payload.getLong(ENTRY_SHOW_UNTIL);

		if(title == null || url == null || showUntil == null) {
			throw new IOException("Required entry missing");
		}

		if(id == null) {
			id = url;
		}

		return new Announcement(id, title, message, url, showUntil);
	}
}
