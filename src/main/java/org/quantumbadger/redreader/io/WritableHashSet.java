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

package org.quantumbadger.redreader.io;

import androidx.annotation.NonNull;
import org.quantumbadger.redreader.common.UnexpectedInternalStateException;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;

public class WritableHashSet implements WritableObject<String>, Iterable<String> {

	@WritableObjectVersion
	public static int DB_VERSION = 1;

	private transient HashSet<String> hashSet = null;
	@WritableField
	private String serialised;

	@WritableObjectKey
	private final String key;
	@WritableObjectTimestamp
	private final long timestamp;

	public WritableHashSet(final HashSet<String> data, final long timestamp, final String key) {
		this.hashSet = data;
		this.timestamp = timestamp;
		this.key = key;
		serialised = listToEscapedString(hashSet);
	}

	private WritableHashSet(final String serializedData, final long timestamp, final String key) {
		this.timestamp = timestamp;
		this.key = key;
		serialised = serializedData;
	}

	public WritableHashSet(final CreationData creationData) {
		this.timestamp = creationData.timestamp;
		this.key = creationData.key;
	}

	@Override
	public String toString() {
		throw new UnexpectedInternalStateException(
				"Using toString() is the wrong way to serialise a WritableHashSet");
	}

	public String serializeWithMetadata() {
		final ArrayList<String> result = new ArrayList<>(3);
		result.add(serialised);
		result.add(String.valueOf(timestamp));
		result.add(key);
		return listToEscapedString(result);
	}

	public static WritableHashSet unserializeWithMetadata(final String raw) {
		final ArrayList<String> data = escapedStringToList(raw);
		return new WritableHashSet(data.get(0), Long.valueOf(data.get(1)), data.get(2));
	}

	public synchronized HashSet<String> toHashset() {
		if(hashSet != null) {
			return hashSet;
		}
		return (hashSet = new HashSet<>(escapedStringToList(serialised)));
	}

	@Override
	public String getKey() {
		return key;
	}

	@Override
	public long getTimestamp() {
		return timestamp;
	}

	public static String listToEscapedString(final Collection<String> list) {

		if(list.isEmpty()) {
			return "";
		}

		final StringBuilder sb = new StringBuilder();

		for(final String str : list) {
			for(int i = 0; i < str.length(); i++) {

				final char c = str.charAt(i);

				switch(c) {
					case '\\':
						sb.append("\\\\");
						break;
					case ';':
						sb.append("\\;");
						break;
					default:
						sb.append(c);
						break;
				}
			}

			sb.append(';');
		}

		return sb.toString();
	}

	public static ArrayList<String> escapedStringToList(String str) {

		final ArrayList<String> result = new ArrayList<>();

		if(str != null) {

			// Workaround to improve parsing of lists saved by older versions of the app
			if(!str.isEmpty() && !str.endsWith(";")) {
				str += ";";
			}

			boolean isEscaped = false;
			final StringBuilder sb = new StringBuilder();

			for(int i = 0; i < str.length(); i++) {

				final char c = str.charAt(i);

				if(c == ';' && !isEscaped) {
					result.add(sb.toString());
					sb.setLength(0);

				} else if(c == '\\') {
					if(isEscaped) {
						sb.append('\\');
					}

				} else {
					sb.append(c);
				}

				isEscaped = c == '\\' && !isEscaped;
			}
		}

		return result;
	}

	@NonNull
	@Override
	public Iterator<String> iterator() {
		return toHashset().iterator();
	}
}
