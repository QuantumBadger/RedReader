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

package org.quantumbadger.redreader.http;

import androidx.annotation.NonNull;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.Optional;
import org.quantumbadger.redreader.jsonwrap.JsonValue;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;

public class FailedRequestBody {

	@NonNull private Optional<byte[]> mBytes;
	@NonNull private Optional<String> mString;
	@NonNull private Optional<JsonValue> mJson;
	private boolean mAttemptedParse = false;

	public FailedRequestBody(
			@NonNull final byte[] bytes) {

		mBytes = Optional.of(bytes);
		mString = Optional.empty();
		mJson = Optional.empty();
	}

	public FailedRequestBody(
			@NonNull final String value) {

		mBytes = Optional.empty();
		mString = Optional.of(value);
		mJson = Optional.empty();
	}

	public FailedRequestBody(
			@NonNull final JsonValue value) {

		mBytes = Optional.empty();
		mString = Optional.empty();
		mJson = Optional.of(value);
	}

	@NonNull
	public static Optional<FailedRequestBody> from(
			@NonNull final InputStream is) {
		try {
			return Optional.of(new FailedRequestBody(General.readWholeStream(is)));
		} catch(final IOException e) {
			return Optional.empty();
		}
	}

	@NonNull
	@Override
	public synchronized String toString() {

		if(!mString.isPresent()) {

			if(mBytes.isPresent()) {
				mString = Optional.of(new String(mBytes.get(), General.CHARSET_UTF8));

			} else if(mJson.isPresent()) {
				mString = Optional.of(mJson.toString());

			} else {
				throw new RuntimeException("No data present");
			}
		}

		return mString.get();
	}

	@NonNull
	public synchronized byte[] toBytes() {

		if(!mBytes.isPresent()) {
			mBytes = Optional.of(toString().getBytes(General.CHARSET_UTF8));
		}

		return mBytes.get();
	}

	@NonNull
	public synchronized Optional<JsonValue> toJson() {

		if(!mJson.isPresent() && !mAttemptedParse) {

			mAttemptedParse = true;

			try {
				mJson = Optional.of(JsonValue.parse(new ByteArrayInputStream(toBytes())));
			} catch(final IOException e) {
				// Ignore this
			}
		}

		return mJson;
	}
}
