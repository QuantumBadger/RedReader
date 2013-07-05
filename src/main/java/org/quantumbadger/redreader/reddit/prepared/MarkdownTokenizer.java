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

import java.util.Arrays;

public final class MarkdownTokenizer {

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
			"www.".toCharArray(),
			"/r/".toCharArray(),
			"r/".toCharArray(),
			"/u/".toCharArray(),
			"u/".toCharArray(),
			"/user/".toCharArray()
	};

	private static final int linkPrefix_minFollowingChars = 2; // e.g. "www.de"

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

	public static int[] tokenizeCleanAndLinkify(final char[] rawArr) {

		final int[] passTwoResult = tokenizeAndClean(rawArr);
		final int passTwoResultLength = passTwoResult.length;

		final int[] passThreeResult = new int[rawArr.length * 3];
		int passThreeResultLength = 0;

		boolean ready = true;

		for(int i = 0; i < passTwoResultLength; i++) {

			final int token = passTwoResult[i];

			switch(token) {

				case TOKEN_BRACKET_SQUARE_OPEN:
				case TOKEN_PAREN_OPEN:
					passThreeResult[passThreeResultLength++] = token;
					ready = false;
					break;

				case TOKEN_BRACKET_SQUARE_CLOSE:
				case TOKEN_PAREN_CLOSE:
				case ' ':
					passThreeResult[passThreeResultLength++] = token;
					ready = true;
					break;

				case 'h':
				case 'w':
				case 'r':
				case 'u':
				case '/':
					if(ready) {

						final int linkStartType = getLinkStartType(passThreeResult, i, passThreeResultLength);
						if(linkStartType >= 0) {
							// TODO read link, then output [X](X) syntax

							int linkEndPos = i + linkPrefixes[linkStartType].length;

							// Greedily read to space, or <>, or etc
							// discard many final chars if they are '.', ',', '?', ';' etc
							// THEN, discard single final char if it is '\'', '"', etc

							// BUT: if it's /r/, /u/ etc, only read alphanums and underscores


						} else {
							passThreeResult[passThreeResultLength++] = token;
						}

					} else {
						passThreeResult[passThreeResultLength++] = token;
					}

					ready = false;
					break;


				default:
					// TODO test this against reddits impl
					ready = token < 0 || (!Character.isAlphabetic(token) && !Character.isDigit(token) && token != '/');
					passThreeResult[passThreeResultLength++] = token;
					break;
			}
		}

		return Arrays.copyOf(passThreeResult, passThreeResultLength);
	}

	// TODO inline, remove copyOf
	public static int[] tokenizeAndClean(final char[] rawArr) {

		final int[] passOneResult = tokenize(rawArr);
		final int passOneResultLength = passOneResult.length;
		final boolean[] toRevert = new boolean[passOneResultLength];
		final boolean[] toDelete = new boolean[passOneResultLength];

		int lastUnderscore = -1, lastUnderscoreDouble = -1;
		int lastAsterisk = -1, lastAsteriskDouble = -1;
		int lastTildeDouble = -1;

		int lastBracketSquareOpen = -1;

		for(int i = 0; i < passOneResultLength; i++) {

			final int c = passOneResult[i];

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
					final int closingGrave = indexOf(passOneResult, TOKEN_GRAVE, i + 1, passOneResultLength);

					if(closingGrave < 0) {
						toRevert[i] = true;
					} else {

						for(int j = openingGrave + 1; j < closingGrave; j++) {
							if(passOneResult[j] < 0) toRevert[j] = true;
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

				// TODO trim contents of ()
				case TOKEN_BRACKET_SQUARE_CLOSE:

					if(lastBracketSquareOpen < 0) {
						toRevert[i] = true;

					} else {

						final int lastBracketSquareClose = i;

						final int parenOpenPos = indexOf(passOneResult, TOKEN_PAREN_OPEN, lastBracketSquareClose + 1, passOneResultLength);
						boolean linkParseSuccess = false;

						if(parenOpenPos >= 0) {

							if(isSpaces(passOneResult, lastBracketSquareClose + 1, parenOpenPos)) {

								final int parenClosePos = indexOf(passOneResult, TOKEN_PAREN_CLOSE, parenOpenPos + 1, passOneResultLength);

								if(parenClosePos >= 0) {

									linkParseSuccess = true;

									for(int j = lastBracketSquareClose + 1; j < parenOpenPos; j++) {
										toDelete[j] = true;
									}

									for(int j = parenOpenPos + 1; j < parenClosePos; j++) {
										if(passOneResult[j] < 0) toRevert[j] = true;
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
					final int closingUnicode = indexOf(passOneResult, TOKEN_UNICODE_CLOSE, i + 1,
							Math.min(passOneResultLength, i + 8));

					if(closingUnicode < 0 || !isDigits(passOneResult, openingUnicode + 1, closingUnicode)) {
						toRevert[i] = true;

					} else {

						final int codePoint = getDecimal(passOneResult, openingUnicode + 1, closingUnicode);
						passOneResult[openingUnicode] = codePoint;

						for(int j = openingUnicode + 1; j <= closingUnicode; j++) {
							toDelete[j] = true;
						}

						i = closingUnicode;
					}

					break;

				case ' ':

					if(i < 1 || passOneResult[i - 1] == ' ') {
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

		final int[] passTwoResult = new int[rawArr.length];
		int passTwoPos = 0;

		for(int i = 0; i < passOneResultLength; i++) {

			if(toDelete[i]) continue;

			if(toRevert[i]) {

				final char[] revertTo = reverseLookup[20 + passOneResult[i]];
				for(final char rCh : revertTo) {
					passTwoResult[passTwoPos++] = rCh;
				}

			} else {
				passTwoResult[passTwoPos++] = passOneResult[i];
			}
		}

		return Arrays.copyOf(passTwoResult, passTwoPos);
	}

	// TODO inline, use resultPos instead of copyOf
	private static int[] tokenize(final char[] rawArr) {

		final int[] result = new int[rawArr.length];
		int resultPos = 0;

		for(int i = 0; i < rawArr.length; i++) {

			final char c = rawArr[i];

			switch(c) {

				case '*':

					if(i < rawArr.length - 1 && rawArr[i + 1] == '*') {
						i++;
						result[resultPos++] = TOKEN_ASTERISK_DOUBLE;
					} else {
						result[resultPos++] = TOKEN_ASTERISK;
					}

					break;

				case '_':

					// TODO check previous
					if(i < rawArr.length - 1 && rawArr[i + 1] == '_') {
						i++;
						result[resultPos++] = TOKEN_UNDERSCORE_DOUBLE;
					} else {
						result[resultPos++] = TOKEN_UNDERSCORE;
					}

					break;

				case '~':

					if(i < rawArr.length - 1 && rawArr[i + 1] == '~') {
						i++;
						result[resultPos++] = TOKEN_TILDE_DOUBLE;

					} else result[resultPos++] = '~';

					break;

				case '^':
					result[resultPos++] = TOKEN_CARET;
					break;

				case '`':
					result[resultPos++] = TOKEN_GRAVE;
					break;

				case '[':
					result[resultPos++] = TOKEN_BRACKET_SQUARE_OPEN;
					break;

				case ']':
					result[resultPos++] = TOKEN_BRACKET_SQUARE_CLOSE;
					break;

				case '(':
					result[resultPos++] = TOKEN_PAREN_OPEN;
					break;

				case ')':
					result[resultPos++] = TOKEN_PAREN_CLOSE;
					break;

				case '&':

					if(i < rawArr.length - 1 && rawArr[i + 1] == '#') {
						i++;
						result[resultPos++] = TOKEN_UNICODE_OPEN;

					} else result[resultPos++] = '&';

					break;

				case ';':
					result[resultPos++] = TOKEN_UNICODE_CLOSE;
					break;

				case '\\':
					if(i < rawArr.length - 1) result[resultPos++] = rawArr[++i];
					else result[resultPos++] = '\\';
					break;

				case '\t':
				case '\r':
				case '\f':
					result[resultPos++] = ' ';
					break;

				default:
					result[resultPos++] = c;
					break;
			}
		}

		return Arrays.copyOf(result, resultPos);
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
}
