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

package org.quantumbadger.redreader.reddit.prepared;

public final class MarkdownTokenizer {

	// TODO support double graves

	public static final int
			TOKEN_UNDERSCORE = -1,
			TOKEN_UNDERSCORE_DOUBLE = -2,
			TOKEN_ASTERISK = -3,
			TOKEN_ASTERISK_DOUBLE = -4,
			TOKEN_TILDE_DOUBLE = -5,
			TOKEN_CARET = -6,
			TOKEN_GRAVE = -7,
			TOKEN_BRACKET_SQUARE_OPEN = -8,
			TOKEN_BRACKET_SQUARE_CLOSE = -9,
			TOKEN_PAREN_OPEN = -10,
			TOKEN_PAREN_CLOSE = -11,
			TOKEN_UNICODE_OPEN = -12,
			TOKEN_UNICODE_CLOSE = -13;

	private static final char[][] reverseLookup = new char[20][];

	private static final char[][] linkPrefixes = {
			"http://".toCharArray(),
			"https://".toCharArray(),
			"www.".toCharArray()
	};

	private static final char[][] linkPrefixes_reddit = {
			"/r/".toCharArray(),
			"r/".toCharArray(),
			"/u/".toCharArray(),
			"u/".toCharArray(),
			"/user/".toCharArray()
	};

	static {
		reverseLookup[20 + TOKEN_UNDERSCORE] = new char[] {'_'};
		reverseLookup[20 + TOKEN_UNDERSCORE_DOUBLE] = new char[] {'_', '_'};
		reverseLookup[20 + TOKEN_ASTERISK] = new char[] {'*'};
		reverseLookup[20 + TOKEN_ASTERISK_DOUBLE] = new char[] {'*', '*'};
		reverseLookup[20 + TOKEN_TILDE_DOUBLE] = new char[] {'~', '~'};
		reverseLookup[20 + TOKEN_CARET] = new char[] {'^'};
		reverseLookup[20 + TOKEN_GRAVE] = new char[] {'`'};
		reverseLookup[20 + TOKEN_BRACKET_SQUARE_OPEN] = new char[] {'['};
		reverseLookup[20 + TOKEN_BRACKET_SQUARE_CLOSE] = new char[] {']'};
		reverseLookup[20 + TOKEN_PAREN_OPEN] = new char[] {'('};
		reverseLookup[20 + TOKEN_PAREN_CLOSE] = new char[] {')'};
		reverseLookup[20 + TOKEN_UNICODE_OPEN] = new char[] {'&', '#'};
		reverseLookup[20 + TOKEN_UNICODE_CLOSE] = new char[] {';'};
	}

	public static IntArrayLengthPair tokenize(final MarkdownParser.CharArrSubstring input) {

		final MarkdownTokenizer.IntArrayLengthPair tmp1 = new MarkdownTokenizer.IntArrayLengthPair(input.length * 3);
		final MarkdownTokenizer.IntArrayLengthPair tmp2 = new MarkdownTokenizer.IntArrayLengthPair(input.length * 3);

		tmp1.pos = input.length;
		for(int i = 0; i < input.length; i++) {
			tmp1.data[i] = input.charAt(i);
		}

		// Markdown is evil.

		naiveTokenize(tmp1, tmp2);
		clean(tmp2, tmp1);
		linkify(tmp1, tmp2);
		clean(tmp2, tmp1);

		return tmp1;
	}

	// TODO "untokenize" parameter in clean function
	private static void linkify(final IntArrayLengthPair input, final IntArrayLengthPair output) {

		if(input.data.length > output.data.length * 3) throw new RuntimeException();
		output.clear();

		//boolean ready = true;
		int inBrackets = 0;
		boolean lastCharOk = true;

		for(int i = 0; i < input.pos; i++) {

			final int token = input.data[i];

			switch(token) {

				case TOKEN_BRACKET_SQUARE_OPEN:
				case TOKEN_PAREN_OPEN:
					output.data[output.pos++] = token;
					inBrackets++;
					lastCharOk = true;
					break;

				case TOKEN_BRACKET_SQUARE_CLOSE:
				case TOKEN_PAREN_CLOSE:
					output.data[output.pos++] = token;
					inBrackets--;
					lastCharOk = true;
					break;

				case ' ':
					output.data[output.pos++] = ' ';
					lastCharOk = true;
					break;

				case 'h':
				case 'w':

					if(inBrackets == 0 && lastCharOk) {

						final int linkStartType = getLinkStartType(input.data, i, input.pos);
						if(linkStartType >= 0) {

							// Greedily read to space, or <>, or etc

							final int linkStartPos = i;
							final int linkPrefixEndPos = linkPrefixes[linkStartType].length + linkStartPos;
							int linkEndPos = linkPrefixEndPos;

							while(linkEndPos < input.pos) {

								final int lToken = input.data[linkEndPos];

								final boolean isValidChar =
										lToken != ' '
												&& lToken != '<'
												&& lToken != '>'
												&& lToken != TOKEN_GRAVE
												&& lToken != TOKEN_BRACKET_SQUARE_OPEN
												&& lToken != TOKEN_BRACKET_SQUARE_CLOSE;

								if(isValidChar) {
									linkEndPos++;
								} else {
									break;
								}
							}

							// discard many final chars if they are '.', ',', '?', ';' etc
							// THEN, discard single final char if it is '\'', '"', etc

							while(input.data[linkEndPos - 1] == '.'
									|| input.data[linkEndPos - 1] == ','
									|| input.data[linkEndPos - 1] == '?'
									|| input.data[linkEndPos - 1] == ';') {
								linkEndPos--;
							}

							if(input.data[linkEndPos - 1] == '"') {
								linkEndPos--;
							}

							if(input.data[linkEndPos - 1] == '\'') {
								linkEndPos--;
							}

							if(input.data[linkEndPos - 1] == ')') {
								linkEndPos--;
							}

							if(linkEndPos - linkPrefixEndPos >= 2) {

								final int[] reverted = revert(input.data, linkStartPos, linkEndPos);

								output.data[output.pos++] = TOKEN_BRACKET_SQUARE_OPEN;
								output.append(reverted);
								output.data[output.pos++] = TOKEN_BRACKET_SQUARE_CLOSE;
								output.data[output.pos++] = TOKEN_PAREN_OPEN;
								output.append(reverted);
								output.data[output.pos++] = TOKEN_PAREN_CLOSE;

								i = linkEndPos - 1;

							} else {
								output.data[output.pos++] = token;
							}

						} else {
							output.data[output.pos++] = token;
						}

					} else {
						output.data[output.pos++] = token;
					}

					lastCharOk = false;
					break;

				case 'r':
				case 'u':
				case '/':

					if(inBrackets == 0 && lastCharOk) {

						final int linkStartType = getRedditLinkStartType(input.data, i, input.pos);
						if(linkStartType >= 0) {

							final int linkStartPos = i;
							final int linkPrefixEndPos = linkPrefixes_reddit[linkStartType].length + linkStartPos;
							int linkEndPos = linkPrefixEndPos;

							while(linkEndPos < input.pos) {

								final int lToken = input.data[linkEndPos];

								final boolean isValidChar =
										(lToken >= 'a' && lToken <= 'z')
												|| (lToken >= 'A' && lToken <= 'Z')
												|| (lToken >= '0' && lToken <= '9')
												|| lToken == '_'
												|| lToken == TOKEN_UNDERSCORE
												|| lToken == TOKEN_UNDERSCORE_DOUBLE
												|| lToken == '+'
												|| lToken == '-';

								if(isValidChar) {
									linkEndPos++;
								} else {
									break;
								}
							}

							if(linkEndPos - linkPrefixEndPos > 2) {

								final int[] reverted = revert(input.data, linkStartPos, linkEndPos);

								output.data[output.pos++] = TOKEN_BRACKET_SQUARE_OPEN;
								output.append(reverted);
								output.data[output.pos++] = TOKEN_BRACKET_SQUARE_CLOSE;
								output.data[output.pos++] = TOKEN_PAREN_OPEN;
								output.append(reverted);
								output.data[output.pos++] = TOKEN_PAREN_CLOSE;

								i = linkEndPos - 1;

							} else {
								output.data[output.pos++] = token;
							}

						} else {
							output.data[output.pos++] = token;
						}

					} else {
						output.data[output.pos++] = token;
					}

					lastCharOk = false;
					break;


				default:
					// TODO test this against reddits impl
					lastCharOk = token < 0 || (!Character.isLetterOrDigit(token) && token != '/');
					output.data[output.pos++] = token;
					break;
			}
		}
	}

	private static void clean(final IntArrayLengthPair input, final IntArrayLengthPair output) {

		// TODO use single byte array, flags
		final boolean[] toRevert = new boolean[input.pos];
		final boolean[] toDelete = new boolean[input.pos];

		int lastUnderscore = -1, lastUnderscoreDouble = -1;
		int lastAsterisk = -1, lastAsteriskDouble = -1;
		int lastTildeDouble = -1;

		int lastBracketSquareOpen = -1;

		for(int i = 0; i < input.pos; i++) {

			final int c = input.data[i];

			switch(c) {

				case TOKEN_UNDERSCORE:
					lastUnderscore = lastUnderscore < 0 ? i : -1;
					break;

				case TOKEN_UNDERSCORE_DOUBLE:

					if(lastUnderscoreDouble < 0) {
						lastUnderscoreDouble = i;

					} else {

						if(lastUnderscoreDouble == i - 1) {
							toRevert[lastUnderscoreDouble] = true;
							toRevert[i] = true;
						}

						lastUnderscoreDouble = -1;
					}

					break;

				case TOKEN_ASTERISK:
					lastAsterisk = lastAsterisk < 0 ? i : -1;
					break;

				case TOKEN_ASTERISK_DOUBLE:

					if(lastAsteriskDouble < 0) {
						lastAsteriskDouble = i;

					} else {

						if(lastAsteriskDouble == i - 1) {
							toRevert[lastAsteriskDouble] = true;
							toRevert[i] = true;
						}

						lastAsteriskDouble = -1;
					}

					break;

				case TOKEN_TILDE_DOUBLE:

					if(lastTildeDouble < 0) {
						lastTildeDouble = i;

					} else {

						if(lastTildeDouble == i - 1) {
							toRevert[lastTildeDouble] = true;
							toRevert[i] = true;
						}

						lastTildeDouble = -1;
					}

					break;

				case TOKEN_GRAVE:

					final int openingGrave = i;
					final int closingGrave = indexOf(input.data, TOKEN_GRAVE, i + 1, input.pos);

					if(closingGrave < 0) {
						toRevert[i] = true;
					} else {

						for(int j = openingGrave + 1; j < closingGrave; j++) {
							if(input.data[j] < 0) toRevert[j] = true;
						}

						i = closingGrave;
					}

					break;

				case TOKEN_BRACKET_SQUARE_OPEN:
					if(lastBracketSquareOpen < 0) {
						lastBracketSquareOpen = i;
					} else {
						toRevert[lastBracketSquareOpen] = true;
						lastBracketSquareOpen = i;
					}
					break;

				case TOKEN_BRACKET_SQUARE_CLOSE:

					if(lastBracketSquareOpen < 0) {
						toRevert[i] = true;

					} else {

						final int lastBracketSquareClose = i;

						final int parenOpenPos = indexOf(input.data, TOKEN_PAREN_OPEN,
								lastBracketSquareClose + 1, input.pos);
						boolean linkParseSuccess = false;

						if(parenOpenPos >= 0) {

							if(isSpaces(input.data, lastBracketSquareClose + 1, parenOpenPos)) {

								final int parenClosePos = findParenClosePos(input, parenOpenPos + 1);

								if(parenClosePos >= 0) {

									linkParseSuccess = true;

									for(int j = lastBracketSquareClose + 1; j < parenOpenPos; j++) {
										toDelete[j] = true;
									}

									for(int j = parenOpenPos + 1; j < parenClosePos; j++) {
										if(input.data[j] < 0) {
											toRevert[j] = true;
										} else if(input.data[j] == ' ' && input.data[j-1] == ' ') {
											toDelete[j] = true;
										}
									}

									for(int j = parenOpenPos + 1; input.data[j] == ' '; j++) {
										toDelete[j] = true;
									}

									for(int j = parenClosePos - 1; input.data[j] == ' '; j--) {
										toDelete[j] = true;
									}

									i = parenClosePos;
								}
							}
						}

						if(!linkParseSuccess) {
							toRevert[lastBracketSquareOpen] = true;
							toRevert[lastBracketSquareClose] = true;
							i = lastBracketSquareClose;
						}
					}

					lastBracketSquareOpen = -1;
					break;

				case TOKEN_PAREN_OPEN:
				case TOKEN_PAREN_CLOSE:
				case TOKEN_UNICODE_CLOSE:
					toRevert[i] = true;
					break;

				case TOKEN_UNICODE_OPEN:

					final int openingUnicode = i;
					final int closingUnicode = indexOf(input.data, TOKEN_UNICODE_CLOSE, i + 1,
							Math.min(input.pos, i + 8));

					if(closingUnicode < 0 || !isDigits(input.data, openingUnicode + 1, closingUnicode)) {
						toRevert[i] = true;

					} else {

						final int codePoint = getDecimal(input.data, openingUnicode + 1, closingUnicode);
						input.data[openingUnicode] = codePoint;

						for(int j = openingUnicode + 1; j <= closingUnicode; j++) {
							toDelete[j] = true;
						}

						i = closingUnicode;
					}

					break;

				case ' ':

					if(i < 1 || input.data[i - 1] == ' ') {
						toDelete[i] = true;
					}

					break;
			}
		}

		if(lastUnderscore >= 0) toRevert[lastUnderscore] = true;
		if(lastUnderscoreDouble >= 0) toRevert[lastUnderscoreDouble] = true;
		if(lastAsterisk >= 0) toRevert[lastAsterisk] = true;
		if(lastAsteriskDouble >= 0) toRevert[lastAsteriskDouble] = true;
		if(lastTildeDouble >= 0) toRevert[lastTildeDouble] = true;
		if(lastBracketSquareOpen >= 0) toRevert[lastBracketSquareOpen] = true;

		for(int j = input.pos - 1; input.data[j] == ' '; j--) {
			toDelete[j] = true;
		}

		output.clear();

		for(int i = 0; i < input.pos; i++) {

			if(toDelete[i]) continue;

			if(toRevert[i]) {

				final char[] revertTo = reverseLookup[20 + input.data[i]];
				output.append(revertTo);

			} else {
				output.data[output.pos++] = input.data[i];
			}
		}
	}

	private static int findParenClosePos(final IntArrayLengthPair tokens, int startPos) {

		for(int i = startPos; i < tokens.pos; i++) {

			switch(tokens.data[i]) {

				case TOKEN_PAREN_CLOSE:
					return i;

				case '"':
					i = indexOfIgnoreEscaped(tokens, '"', i + 1);
					if(i < 0) return -1;
					break;
			}
		}

		return -1;
	}

	private static int indexOfIgnoreEscaped(final IntArrayLengthPair haystack, int needle, int startPos) {
		for(int i = startPos; i < haystack.pos; i++) {
			if(haystack.data[i] == '\\') i++;
			else if(haystack.data[i] == needle) return i;
		}
		return -1;
	}

	private static void naiveTokenize(final IntArrayLengthPair input, final IntArrayLengthPair output) {

		output.clear();

		for(int i = 0; i < input.pos; i++) {

			final int c = input.data[i];

			switch(c) {

				case '*':

					if(i < input.pos - 1 && input.data[i + 1] == '*') {
						i++;
						output.data[output.pos++] = TOKEN_ASTERISK_DOUBLE;
					} else {
						output.data[output.pos++] = TOKEN_ASTERISK;
					}

					break;

				case '_':

					// TODO check previous
					if(i < input.pos - 1 && input.data[i + 1] == '_') {
						i++;
						output.data[output.pos++] = TOKEN_UNDERSCORE_DOUBLE;
					} else {
						output.data[output.pos++] = TOKEN_UNDERSCORE;
					}

					break;

				case '~':

					if(i < input.pos - 1 && input.data[i + 1] == '~') {
						i++;
						output.data[output.pos++] = TOKEN_TILDE_DOUBLE;

					} else output.data[output.pos++] = '~';

					break;

				case '^':
					output.data[output.pos++] = TOKEN_CARET;
					break;

				case '`':
					output.data[output.pos++] = TOKEN_GRAVE;
					break;

				case '[':
					output.data[output.pos++] = TOKEN_BRACKET_SQUARE_OPEN;
					break;

				case ']':
					output.data[output.pos++] = TOKEN_BRACKET_SQUARE_CLOSE;
					break;

				case '(':
					output.data[output.pos++] = TOKEN_PAREN_OPEN;
					break;

				case ')':
					output.data[output.pos++] = TOKEN_PAREN_CLOSE;
					break;

				case '&':

					if(i < input.pos - 1 && input.data[i + 1] == '#') {
						i++;
						output.data[output.pos++] = TOKEN_UNICODE_OPEN;

					} else output.data[output.pos++] = '&';

					break;

				case ';':
					output.data[output.pos++] = TOKEN_UNICODE_CLOSE;
					break;

				case '\\':
					if(i < input.pos - 1) output.data[output.pos++] = input.data[++i];
					else output.data[output.pos++] = '\\';
					break;

				case '\t':
				case '\r':
				case '\f':
				case '\n':
					output.data[output.pos++] = ' ';
					break;

				default:
					output.data[output.pos++] = c;
					break;
			}
		}
	}

	private static int indexOf(final int[] haystack, final int needle, final int startInclusive, final int endExclusive) {
		for(int i = startInclusive; i < endExclusive; i++) if(haystack[i] == needle) return i;
		return -1;
	}

	private static boolean isSpaces(final int[] haystack, final int startInclusive, final int endExclusive) {
		for(int i = startInclusive; i < endExclusive; i++) if(haystack[i] != ' ') return false;
		return true;
	}

	private static boolean isDigits(final int[] haystack, final int startInclusive, final int endExclusive) {
		for(int i = startInclusive; i < endExclusive; i++) if(haystack[i] < '0' || haystack[i] > '9') return false;
		return true;
	}

	private static int getDecimal(final int[] chars, final int startInclusive, final int endExclusive) {
		int result = 0;
		for(int i = startInclusive; i < endExclusive; i++) {
			result *= 10;
			result += chars[i] - '0';
		}
		return result;
	}

	private static boolean equals(final int[] haystack, final char[] needle, int startInclusive) {
		for(int i = 0; i < needle.length; i++) if(haystack[startInclusive + i] != needle[i]) return false;
		return true;
	}

	private static int getLinkStartType(final int[] haystack, final int startInclusive, final int endExclusive) {
		final int maxLen = endExclusive - startInclusive;
		for(int type = 0; type < linkPrefixes.length; type++) {
			if(linkPrefixes[type].length <= maxLen && equals(haystack, linkPrefixes[type], startInclusive)) {
				return type;
			}
		}
		return -1;
	}

	private static int getRedditLinkStartType(final int[] haystack, final int startInclusive, final int endExclusive) {
		final int maxLen = endExclusive - startInclusive;
		for(int type = 0; type < linkPrefixes_reddit.length; type++) {
			if(linkPrefixes_reddit[type].length <= maxLen && equals(haystack, linkPrefixes_reddit[type], startInclusive)) {
				return type;
			}
		}
		return -1;
	}

	// TODO avoid generating new array
	private static int[] revert(final int[] tokens, final int startInclusive, final int endExclusive) {

		int outputLen = 0;

		for(int i = startInclusive; i < endExclusive; i++) {
			final int token = tokens[i];
			if(token < 0) {
				outputLen += reverseLookup[20 + token].length;

			} else {
				outputLen++;
			}
		}

		final int[] result = new int[outputLen];
		int resultPos = 0;

		for(int i = startInclusive; i < endExclusive; i++) {
			final int token = tokens[i];
			if(token < 0) {
				for(final char c : reverseLookup[20 + token]) {
					result[resultPos++] = c;
				}

			} else {
				result[resultPos++] = token;
			}
		}

		return result;
	}

	public static class IntArrayLengthPair {
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
	}
}
