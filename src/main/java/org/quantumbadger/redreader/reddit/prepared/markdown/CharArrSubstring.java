package org.quantumbadger.redreader.reddit.prepared.markdown;

import android.util.Log;

import java.util.LinkedList;

public final class CharArrSubstring {
	protected final char[] arr;
	protected final int start;
	public final int length;

	CharArrSubstring(char[] arr, int start, int length) {
		this.arr = arr;
		this.start = start;
		this.length = length;
	}

	public static CharArrSubstring generate(final char[] src) {
		return new CharArrSubstring(src, 0, src.length);
	}

	public static CharArrSubstring[] generateFromLines(final char[] src) {

		int curPos = 0;

		final LinkedList<CharArrSubstring> result = new LinkedList<CharArrSubstring>();

		int nextLinebreak;

		while((nextLinebreak = indexOfLinebreak(src, curPos)) != -1) {
			result.add(new CharArrSubstring(src, curPos, nextLinebreak - curPos));
			curPos = nextLinebreak + 1;
		}

		result.add(new CharArrSubstring(src, curPos, src.length - curPos));

		return result.toArray(new CharArrSubstring[result.size()]);
	}

	public CharArrSubstring rejoin(final CharArrSubstring toAppend) {

		if(toAppend.start - 1 != start + length) {
			throw new RuntimeException("Internal error: attempt to join non-consecutive substrings");
		}

		return new CharArrSubstring(arr, start, length + 1 + toAppend.length);
	}

	private static int indexOfLinebreak(final char[] raw, int startPos) {
		for(int i = startPos; i < raw.length; i++) {
			if(raw[i] == '\n') return i;
		}
		return -1;
	}

	public int countSpacesAtStart() {
		for(int i = 0; i < length; i++) {
			if(arr[start + i] != ' ') return i;
		}
		return length;
	}

	public int countSpacesAtEnd() {
		for(int i = 0; i < length; i++) {
			if(arr[start + length - 1 - i] != ' ') return i;
		}
		return length;
	}

	public char charAt(int index) {
		return arr[start + index];
	}

	public int countPrefixLengthIgnoringSpaces(final char c) {
		for(int i = 0; i < length; i++) {
			if(arr[start + i] != ' ' && arr[start + i] != c) return i;
		}
		return length;
	}

	public int countPrefixLevelIgnoringSpaces(final char c) {
		int level = 0;
		for(int i = 0; i < length; i++) {
			if(arr[start + i] != ' ' && arr[start + i] != c) return level;
			else if(arr[start + i] == c) level++; // TODO tidy up
		}
		return length;
	}

	public CharArrSubstring left(int chars) {
		return new CharArrSubstring(arr, start, chars);
	}

	public CharArrSubstring substring(int start) {
		return new CharArrSubstring(arr, this.start + start, length - start);
	}

	public CharArrSubstring substring(int start, int len) {
		return new CharArrSubstring(arr, this.start + start, len);
	}

	public CharArrSubstring readInteger(final int start) {
		for(int i = start; i < length; i++) {
			final char c = arr[this.start + i];
			if(c < '0' || c > '9') return new CharArrSubstring(arr, this.start + start, i - start);
		}
		return new CharArrSubstring(arr, this.start + start, length - start);
	}

	@Override
	public String toString() {
		return new String(arr, start, length);
	}

	public boolean isRepeatingChar(final char c, final int start, final int len) {
		for(int i = 0; i < len; i++) {
			if(arr[i + start + this.start] != c) {
				return false;
			}
		}
		return true;
	}
}
