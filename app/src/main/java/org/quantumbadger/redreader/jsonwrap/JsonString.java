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

package org.quantumbadger.redreader.jsonwrap;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import org.apache.commons.text.StringEscapeUtils;
import org.quantumbadger.redreader.common.StringUtils;

public class JsonString extends JsonValue {

	@NonNull private final String mValue;

	protected JsonString(@NonNull final String value) {
		mValue = value;
	}

	@Override
	protected void prettyPrint(final int indent, final StringBuilder sb) {
		sb.append('"').append(StringEscapeUtils.escapeJson(mValue)).append('"');
	}

	@Nullable
	@Override
	public Boolean asBoolean() {

		final String lowercase = StringUtils.asciiLowercase(mValue);

		switch(lowercase) {
			case "true":
			case "t":
			case "1":
				return true;
			case "false":
			case "f":
			case "0":
				return false;
		}

		return null;
	}

	@NonNull
	@Override
	public String asString() {
		return mValue;
	}

	@Nullable
	@Override
	public Double asDouble() {

		try {
			return Double.parseDouble(mValue);

		} catch(final NumberFormatException e) {
			return null;
		}
	}

	@Nullable
	@Override
	public Long asLong() {

		try {
			return Long.parseLong(mValue);

		} catch(final NumberFormatException e) {
			return null;
		}
	}
}
