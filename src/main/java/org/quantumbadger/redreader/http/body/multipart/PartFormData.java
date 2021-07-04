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

package org.quantumbadger.redreader.http.body.multipart;

import androidx.annotation.NonNull;

public class PartFormData implements Part {

	@NonNull public final String name;
	@NonNull public final String value;

	public PartFormData(@NonNull final String name, @NonNull final String value) {
		this.name = name;
		this.value = value;
	}

	@NonNull
	public <E> E visit(@NonNull final Visitor<E> visitor) {
		return visitor.visitPart(this);
	}

}
