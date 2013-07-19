package org.quantumbadger.redreader.reddit.prepared.markdown;

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
