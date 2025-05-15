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

public class JsonDouble extends JsonValue {

	private final double mValue;

	protected JsonDouble(final double value) {
		mValue = value;
	}

	@Override
	protected void prettyPrint(final int indent, final StringBuilder sb) {
		sb.append(mValue);
	}

	@NonNull
	@Override
	public String asString() {
		return String.valueOf(mValue);
	}

	@NonNull
	@Override
	public Double asDouble() {
		return mValue;
	}

	@NonNull
	@Override
	public Long asLong() {
		return Math.round(mValue);
	}
}
