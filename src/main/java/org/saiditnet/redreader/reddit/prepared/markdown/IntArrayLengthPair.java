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

package org.saiditnet.redreader.reddit.prepared.markdown;

public final class IntArrayLengthPair {
	public final int[] data;
	public int pos = 0;

	public IntArrayLengthPair(int capacity) {
		this.data = new int[capacity];
	}

	public void clear() {
		pos = 0;
	}

	public void append(final int[] arr) {
		System.arraycopy(arr, 0, data, pos, arr.length);
		pos += arr.length;
	}

	public void append(final char[] arr) {

		for(int i = 0; i < arr.length; i++) {
			data[pos + i] = arr[i];
		}

		pos += arr.length;
	}

	public int[] substringAsArray(int start) {
		final int[] result = new int[pos - start];
		System.arraycopy(data, start, result, 0, result.length);
		return result;
	}
}
