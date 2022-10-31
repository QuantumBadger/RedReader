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

import org.apache.commons.lang3.StringEscapeUtils;

import java.util.HashSet;

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
			"/s/".toCharArray(),
			"s/".toCharArray(),
			"/u/".toCharArray(),
			"u/".toCharArray(),
			"/user/".toCharArray()
	};

	private static final HashSet<Integer> unicodeWhitespace = new HashSet<>();

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
		reverseLookup[20 + TOKEN_UNICODE_OPEN] = new char[] {'&'};
		reverseLookup[20 + TOKEN_UNICODE_CLOSE] = new char[] {';'};

		unicodeWhitespace.add(0x0009);
		unicodeWhitespace.add(0x000B);
		unicodeWhitespace.add(0x00A0);
		unicodeWhitespace.add(0x1680);
		unicodeWhitespace.add(0x2000);
		unicodeWhitespace.add(0x2001);
		unicodeWhitespace.add(0x2002);
		unicodeWhitespace.add(0x2003);
		unicodeWhitespace.add(0x2004);
		unicodeWhitespace.add(0x2005);
		unicodeWhitespace.add(0x2006);
		unicodeWhitespace.add(0x2007);
		unicodeWhitespace.add(0x2008);
		unicodeWhitespace.add(0x2009);
		unicodeWhitespace.add(0x200A);
		unicodeWhitespace.add(0x202F);
		unicodeWhitespace.add(0x205F);
		unicodeWhitespace.add(0x3000);
	}

	public static boolean isUnicodeWhitespace(int codepoint) {
		return unicodeWhitespace.contains(codepoint);
	}

	public static IntArrayLengthPair tokenize(final CharArrSubstring input) {

		final IntArrayLengthPair tmp1 = new IntArrayLengthPair(input.length * 3);
		final IntArrayLengthPair tmp2 = new IntArrayLengthPair(input.length * 3);

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

	private static void linkify(final IntArrayLengthPair input, final IntArrayLengthPair output) {

		if(input.data.length > output.data.length * 3) throw new RuntimeException();
		output.clear();

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

							boolean hasOpeningParen = false;

							while(linkEndPos < input.pos) {

								final int lToken = input.data[linkEndPos];

								final boolean isValidChar =
										lToken != ' '
												&& lToken != '<'
												&& lToken != '>'
												&& lToken != TOKEN_GRAVE
												&& lToken != TOKEN_BRACKET_SQUARE_OPEN
												&& lToken != TOKEN_BRACKET_SQUARE_CLOSE;

								if(lToken == '(') {
									hasOpeningParen = true;
								}

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

							if(!hasOpeningParen && input.data[linkEndPos - 1] == ')') {
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

				case 's':
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
					lastCharOk = token < 0 || (!Character.isLetterOrDigit(token));
					output.data[output.pos++] = token;
					break;
			}
		}
	}

	public static void clean(final IntArrayLengthPair input, final IntArrayLengthPair output) {

		// TODO use single byte array, flags
		final boolean[] toRevert = new boolean[input.pos];
		final boolean[] toDelete = new boolean[input.pos];

		int openingUnderscore = -1, openingUnderscoreDouble = -1;
		int openingAsterisk = -1, openingAsteriskDouble = -1;
		int openingTildeDouble = -1;

		int lastBracketSquareOpen = -1;

		for(int i = 0; i < input.pos; i++) {

			final int c = input.data[i];

			final boolean beforeASpace = i + 1 < input.pos && input.data[i + 1] == ' ';
			final boolean afterASpace = i > 0 && input.data[i - 1] == ' ';

			switch(c) {

				case TOKEN_UNDERSCORE:

					if(openingUnderscore < 0) {
						// Opening underscore
						if(beforeASpace) {
							toRevert[i] = true;
						} else {
							openingUnderscore =  i;
						}

					} else {
						// Closing underscore
						if(afterASpace) {
							toRevert[i] = true;
						} else {
							openingUnderscore = -1;
						}
					}

					break;

				case TOKEN_UNDERSCORE_DOUBLE:

					if(i != 0 && openingUnderscoreDouble == i - 1) {
						toRevert[openingUnderscoreDouble] = true;
						toRevert[i] = true;
						openingUnderscoreDouble = -1;

					} else {

						if(openingUnderscoreDouble < 0) {
							// Opening double underscore
							if(beforeASpace) {
								toRevert[i] = true;
							} else {
								openingUnderscoreDouble = i;
							}

						} else {
							// Closing double underscore
							if(afterASpace) {
								toRevert[i] = true;
							} else {
								openingUnderscoreDouble = -1;
							}
						}
					}

					break;

				case TOKEN_ASTERISK:

					if(openingAsterisk < 0) {
						// Opening asterisk
						if(beforeASpace) {
							toRevert[i] = true;
						} else {
							openingAsterisk =  i;
						}

					} else {
						// Closing asterisk
						if(afterASpace) {
							toRevert[i] = true;
						} else {
							openingAsterisk = -1;
						}
					}

					break;

				case TOKEN_ASTERISK_DOUBLE:

					if(i != 0 && openingAsteriskDouble == i - 1) {
						toRevert[openingAsteriskDouble] = true;
						toRevert[i] = true;
						openingAsteriskDouble = -1;

					} else {

						if(openingAsteriskDouble < 0) {
							// Opening double asterisk
							if(beforeASpace) {
								toRevert[i] = true;
							} else {
								openingAsteriskDouble = i;
							}

						} else {
							// Closing double asterisk
							if(afterASpace) {
								toRevert[i] = true;
							} else {
								openingAsteriskDouble = -1;
							}
						}
					}

					break;

				case TOKEN_TILDE_DOUBLE:

					if(i != 0 && openingTildeDouble == i - 1) {
						toRevert[openingTildeDouble] = true;
						toRevert[i] = true;
						openingTildeDouble = -1;

					} else {

						if(openingTildeDouble < 0) {
							// Opening double tilde
							if(beforeASpace) {
								toRevert[i] = true;
							} else {
								openingTildeDouble = i;
							}

						} else {
							// Closing double tilde
							if(afterASpace) {
								toRevert[i] = true;
							} else {
								openingTildeDouble = -1;
							}
						}
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

						// Attempt to parse link text with well-bracketed square brackets

						final int closingSquareBracket = findCloseWellBracketed(
								input.data,
								TOKEN_BRACKET_SQUARE_OPEN,
								TOKEN_BRACKET_SQUARE_CLOSE,
								i,
								input.pos);

						if(closingSquareBracket > i) {

							final int parenOpenPos = indexOf(input.data, TOKEN_PAREN_OPEN, closingSquareBracket + 1, input.pos);

							if(parenOpenPos > closingSquareBracket
									&& isSpaces(input.data, closingSquareBracket + 1, parenOpenPos)) {

								lastBracketSquareOpen = i;

								for(int j = i + 1; j < closingSquareBracket; j++) {
									if(input.data[j] == TOKEN_BRACKET_SQUARE_OPEN) {
										input.data[j] = '[';

									} else if(input.data[j] == TOKEN_BRACKET_SQUARE_CLOSE) {
										input.data[j] = ']';
									}
								}

							} else {
								toRevert[i] = true;
							}

						} else {
							toRevert[i] = true;
						}

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

									for(int j = lastBracketSquareOpen + 1; j < lastBracketSquareClose; j++) {
										if(input.data[j] == TOKEN_BRACKET_SQUARE_OPEN
											|| input.data[j] == TOKEN_BRACKET_SQUARE_CLOSE) {
											toRevert[j] = true;
										}
									}

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
							Math.min(input.pos, i + 20));

					if(closingUnicode < 0) {
						toRevert[i] = true;

					} else if(input.data[i + 1] == '#') {

						if(input.data[i + 2] == 'x' && isHexDigits(input.data, openingUnicode + 3, closingUnicode)) {

							final int codePoint = getHex(input.data, openingUnicode + 3, closingUnicode);

							if(unicodeWhitespace.contains(codePoint)) {
								input.data[openingUnicode] = ' ';
							} else {
								input.data[openingUnicode] = codePoint;
							}

							for(int j = openingUnicode + 1; j <= closingUnicode; j++) {
								toDelete[j] = true;
							}

							i = closingUnicode;

						} else if(isDigits(input.data, openingUnicode + 2, closingUnicode)) {

							final int codePoint = getDecimal(input.data, openingUnicode + 2, closingUnicode);

							if(unicodeWhitespace.contains(codePoint)) {
								input.data[openingUnicode] = ' ';
							} else {
								input.data[openingUnicode] = codePoint;
							}

							for(int j = openingUnicode + 1; j <= closingUnicode; j++) {
								toDelete[j] = true;
							}

							i = closingUnicode;

						} else {
							toRevert[i] = true;
						}

					} else {

						Integer codePoint = null;

						try {

							final String name = new String(input.data, openingUnicode + 1, closingUnicode - openingUnicode - 1);

							final String result = StringEscapeUtils.unescapeHtml4("&" + name + ";");

							if(result.length() == 1) {
								codePoint = (int) result.charAt(0);

							} else if(name.equalsIgnoreCase("apos")) {
								codePoint = (int) '\'';

							} else if(name.equalsIgnoreCase("nsub")) {
								codePoint = (int) '⊄';
							}

						} catch(Throwable ignore) {
							// Ignore this
						}

						if(codePoint != null) {

							if(unicodeWhitespace.contains(codePoint)) {
								input.data[openingUnicode] = ' ';
							} else {
								input.data[openingUnicode] = codePoint;
							}

							for(int j = openingUnicode + 1; j <= closingUnicode; j++) {
								toDelete[j] = true;
							}

							i = closingUnicode;

						} else {
							toRevert[i] = true;
						}
					}

					break;

				case TOKEN_CARET:

					if(input.pos <= i + 1 || input.data[i + 1] == ' ') {
						toRevert[i] = true;
					}

					break;

				case ' ':

					if(i < 1 || input.data[i - 1] == ' ') {
						toDelete[i] = true;
					}

					break;
			}
		}

		if(openingUnderscore >= 0) toRevert[openingUnderscore] = true;
		if(openingUnderscoreDouble >= 0) toRevert[openingUnderscoreDouble] = true;
		if(openingAsterisk >= 0) toRevert[openingAsterisk] = true;
		if(openingAsteriskDouble >= 0) toRevert[openingAsteriskDouble] = true;
		if(openingTildeDouble >= 0) toRevert[openingTildeDouble] = true;
		if(lastBracketSquareOpen >= 0) toRevert[lastBracketSquareOpen] = true;

		for(int j = input.pos - 1; j >= 0 && input.data[j] == ' '; j--) {
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

	public static void naiveTokenize(final IntArrayLengthPair input, final IntArrayLengthPair output) {

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

					if(i < input.pos - 1 && input.data[i + 1] == '_') {
						i++;
						output.data[output.pos++] = TOKEN_UNDERSCORE_DOUBLE;
					} else {
						if ((i < input.pos -1 && input.data[i+1] == ' ')
								|| (i > 0 && input.data[i-1] == ' ')
								|| (i == 0)	|| (i == input.pos - 1)) {
							output.data[output.pos++] = TOKEN_UNDERSCORE;
						} else {
							output.data[output.pos++] = c;
						}
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
					output.data[output.pos++] = TOKEN_UNICODE_OPEN;
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

	private static int reverseIndexOf(final int[] haystack, final int needle, final int startInclusive) {
		for(int i = startInclusive; i >= 0; i--) if(haystack[i] == needle) return i;
		return -1;
	}

	public static int findCloseWellBracketed(
			final int[] haystack,
			final int openBracket,
			final int closeBracket,
			final int startInclusive,
			final int endExclusive) {

		if(haystack[startInclusive] != openBracket) {
			throw new RuntimeException("Internal markdown parser error");
		}

		int b = 1;

		for(int i = startInclusive + 1; i < endExclusive; i++) {
			if(haystack[i] == openBracket) {
				b++;
			} else if(haystack[i] == closeBracket) {
				b--;
			}

			if(b == 0) {
				return i;
			}
		}

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

	private static boolean isHexDigits(final int[] haystack, final int startInclusive, final int endExclusive) {
		for(int i = startInclusive; i < endExclusive; i++) {
			final int c = haystack[i];
			if((c < '0' || c > '9') && (c < 'a' || c > 'f') && (c < 'A' || c > 'F')) return false;
		}
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

	private static int fromHex(int ch) {
		if(ch >= '0' && ch <= '9') return ch - '0';
		if(ch >= 'a' && ch <= 'f') return 10 + ch - 'a';
		return 10 + ch - 'A';
	}

	private static int getHex(final int[] chars, final int startInclusive, final int endExclusive) {
		int result = 0;
		for(int i = startInclusive; i < endExclusive; i++) {
			result *= 16;
			result += fromHex(chars[i]);
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

}
