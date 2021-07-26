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

package org.quantumbadger.redreader.common;

import android.os.Parcel;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

public class ParcelUtils {

	public static void writeNullableBoolean(
			@NonNull final Parcel parcel,
			@Nullable final Boolean value) {

		if(value == null) {
			parcel.writeInt(0);
		} else if(value) {
			parcel.writeInt(1);
		} else {
			parcel.writeInt(-1);
		}
	}

	@Nullable
	public static Boolean readNullableBoolean(@NonNull final Parcel parcel) {

		final int value = parcel.readInt();

		switch(value) {
			case -1: return false;
			case 0: return null;
			case 1: return true;
		}

		throw new RuntimeException("Invalid value " + value);
	}
}
