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

public class JsonBoolean extends JsonValue {

	@NonNull public static final JsonBoolean TRUE = new JsonBoolean(true);
	@NonNull public static final JsonBoolean FALSE = new JsonBoolean(false);

	private final boolean mValue;

	private JsonBoolean(final boolean value) {
		mValue = value;
	}

	@Override
	protected void prettyPrint(final int indent, final StringBuilder sb) {
		sb.append(mValue ? "true" : "false");
	}

	@Override
	@NonNull
	public Boolean asBoolean() {
		return mValue;
	}
}
