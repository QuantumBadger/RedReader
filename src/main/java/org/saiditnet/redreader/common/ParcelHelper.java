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

package org.saiditnet.redreader.common;

import android.os.Parcel;
import org.saiditnet.redreader.image.ImageInfo;

public class ParcelHelper {

	public static boolean readBoolean(final Parcel in) {
		return in.readByte() == 1;
	}

	public static String readNullableString(final Parcel in) {

		final boolean isNull = readBoolean(in);
		if(isNull) return null;

		return in.readString();
	}

	public static ImageInfo.MediaType readNullableEnum(final Parcel in) {

		final boolean isNull = readBoolean(in);
		if(isNull) return null;

		return ImageInfo.MediaType.valueOf(in.readString());
	}

	public static void writeNullableEnum(final Parcel parcel, final ImageInfo.MediaType value) {

		if(value == null) {
			writeBoolean(parcel, false);
		} else {
			writeBoolean(parcel, true);
			parcel.writeString(value.name());
		}
	}

	public static Integer readNullableInt(final Parcel in) {

		final boolean isNull = readBoolean(in);
		if(isNull) return null;

		return in.readInt();
	}

	public static Long readNullableLong(final Parcel in) {

		final boolean isNull = readBoolean(in);
		if(isNull) return null;

		return in.readLong();
	}

	public static Boolean readNullableBoolean(final Parcel in) {

		final boolean isNull = readBoolean(in);
		if(isNull) return null;

		return readBoolean(in);
	}

	public static void writeBoolean(final Parcel parcel, final boolean b) {
		parcel.writeByte((byte)(b ? 1 : 0));
	}

	public static void writeNullableString(final Parcel parcel, final String value) {

		if(value == null) {
			writeBoolean(parcel, false);
		} else {
			writeBoolean(parcel, true);
			parcel.writeString(value);
		}
	}

	public static void writeNullableLong(final Parcel parcel, final Long value) {

		if(value == null) {
			writeBoolean(parcel, false);
		} else {
			writeBoolean(parcel, true);
			parcel.writeLong(value);
		}
	}

	public static void writeNullableBoolean(final Parcel parcel, final Boolean value) {

		if(value == null) {
			writeBoolean(parcel, false);
		} else {
			writeBoolean(parcel, true);
			writeBoolean(parcel, value);
		}
	}
}
