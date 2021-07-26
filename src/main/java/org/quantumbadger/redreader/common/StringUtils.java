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
import androidx.annotation.Nullable;

import java.util.Collection;

public final class StringUtils {

	private StringUtils() {}

	@NonNull
	public static Optional<String> removePrefix(
			@NonNull final String input,
			@NonNull final String prefix) {

		if(input.startsWith(prefix)) {
			return Optional.of(input.substring(prefix.length()));

		} else {
			return Optional.empty();
		}
	}

	@NonNull
	public static String asciiUppercase(@NonNull final String input) {

		final char[] chars = input.toCharArray();

		for(int i = 0; i < chars.length; i++) {
			if(chars[i] >= 'a' && chars[i] <= 'z') {
				chars[i] -= 'a';
				chars[i] += 'A';
			}
		}

		return new String(chars);
	}

	@NonNull
	public static String asciiLowercase(@NonNull final String input) {

		final char[] chars = input.toCharArray();

		for(int i = 0; i < chars.length; i++) {
			if(chars[i] >= 'A' && chars[i] <= 'Z') {
				chars[i] -= 'A';
				chars[i] += 'a';
			}
		}

		return new String(chars);
	}

	@NonNull
	public static String join(
			@NonNull final Collection<?> elements,
			@NonNull final CharSequence separator) {

		final StringBuilder result = new StringBuilder();

		boolean first = true;

		for(final Object element : elements) {

			if(!first) {
				result.append(separator);
			}

			result.append(element.toString());
			first = false;
		}

		return result.toString();
	}

	public static boolean isEmpty(@Nullable final CharSequence value) {
		return value == null || value.length() == 0;
	}

	@NonNull
	public static String fromUTF8(@NonNull final byte[] bytes) {
		return new String(bytes, General.CHARSET_UTF8);
	}
}
