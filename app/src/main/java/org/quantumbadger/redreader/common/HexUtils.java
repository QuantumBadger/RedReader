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

import androidx.annotation.NonNull;

import java.io.IOException;
import java.util.Locale;

public final class HexUtils {

	private HexUtils() {}

	@NonNull
	public static String toHex(@NonNull final byte[] input) {

		final StringBuilder result = new StringBuilder(input.length * 2);

		for(final byte b : input) {
			result.append(String.format(Locale.US, "%02X", b));
		}

		return result.toString();
	}

	public static int fromHex(final char digit) throws IOException {

		if(digit >= '0' && digit <= '9') {
			return digit - '0';
		}

		if(digit >= 'A' && digit <= 'F') {
			return digit + 10 - 'A';
		}

		if(digit >= 'a' && digit <= 'f') {
			return digit + 10 - 'a';
		}

		throw new IOException("Invalid hex digit '" + digit + "'");
	}

	@NonNull
	public static byte[] fromHex(@NonNull final String input) throws IOException {

		final String inputTrimmed = input.trim();

		if(inputTrimmed.length() % 2 != 0) {
			throw new IOException("Hex string length is not even: '" + inputTrimmed + "'");
		}

		final char[] chars = inputTrimmed.toCharArray();

		final byte[] result = new byte[chars.length / 2];

		for(int i = 0; i < result.length; i++) {
			result[i] = (byte)((fromHex(chars[i * 2]) << 4) | fromHex(chars[i * 2 + 1]));
		}

		return result;
	}
}
