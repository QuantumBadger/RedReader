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

public final class MarkdownLine {

	public final CharArrSubstring src;
	public final MarkdownParser.MarkdownParagraphType type;
	public final int spacesAtStart, spacesAtEnd;
	public final int prefixLength, level, number;

	MarkdownLine(CharArrSubstring src, MarkdownParser.MarkdownParagraphType type, int spacesAtStart, int spacesAtEnd,
				 int prefixLength, int level, int number) {
		this.src = src;
		this.type = type;
		this.spacesAtStart = spacesAtStart;
		this.spacesAtEnd = spacesAtEnd;
		this.prefixLength = prefixLength;
		this.level = level;
		this.number = number;
	}

	public static MarkdownLine generate(final CharArrSubstring src) {

		final int spacesAtStart = src.countSpacesAtStart();
		final int spacesAtEnd = src.countSpacesAtEnd();

		if(spacesAtStart == src.length) {
			// New paragraph
			return new MarkdownLine(null, MarkdownParser.MarkdownParagraphType.EMPTY, 0, 0, 0, 0, 0);
		}

		if(spacesAtStart >= 4) {
			return new MarkdownLine(src, MarkdownParser.MarkdownParagraphType.CODE, spacesAtStart, spacesAtEnd, 4, 0, 0);
		}

		final char firstNonSpaceChar = src.charAt(spacesAtStart);

		switch(firstNonSpaceChar) {
			case '>': {

				final int level = src.countPrefixLevelIgnoringSpaces('>');
				final int prefixLen = src.countPrefixLengthIgnoringSpaces('>');

				return new MarkdownLine(src, MarkdownParser.MarkdownParagraphType.QUOTE, spacesAtStart, spacesAtEnd, prefixLen, level, 0);
			}

			case '-':
			case '*':
			{

				if(src.length > spacesAtStart + 1 && src.charAt(spacesAtStart + 1) == ' ') {

					return new MarkdownLine(src, MarkdownParser.MarkdownParagraphType.BULLET, spacesAtStart, spacesAtEnd,
							spacesAtStart + 2, spacesAtStart == 0 ? 0 : 1, 0);

				} else if(src.length >= 3 && src.isRepeatingChar('*', spacesAtStart, src.length - spacesAtEnd)) {
					return new MarkdownLine(src, MarkdownParser.MarkdownParagraphType.HLINE, 0, 0, 0, 0, 0);

				} else {
					return new MarkdownLine(src, MarkdownParser.MarkdownParagraphType.TEXT, spacesAtStart, spacesAtEnd,
							spacesAtStart, 0, 0);
				}
			}

			case '0':
			case '1':
			case '2':
			case '3':
			case '4':
			case '5':
			case '6':
			case '7':
			case '8':
			case '9':
			{
				final CharArrSubstring num = src.readInteger(spacesAtStart);

				if(src.length > spacesAtStart + num.length + 2
						&& src.charAt(spacesAtStart + num.length) == '.'
						&& src.charAt(spacesAtStart + num.length + 1) == ' ') {

					return new MarkdownLine(src, MarkdownParser.MarkdownParagraphType.NUMBERED, spacesAtStart, spacesAtEnd,
							spacesAtStart + num.length + 2, spacesAtStart == 0 ? 0 : 1, Integer.parseInt(num.toString()));

				} else {
					return new MarkdownLine(src, MarkdownParser.MarkdownParagraphType.TEXT, spacesAtStart, spacesAtEnd,
							spacesAtStart, 0, 0);
				}
			}

			case '#':
				// TODO prefix and suffix length
				return new MarkdownLine(src, MarkdownParser.MarkdownParagraphType.HEADER, spacesAtStart, spacesAtEnd,
						src.countPrefixLengthIgnoringSpaces('#'), 0, 0);

			default:
				return new MarkdownLine(src, MarkdownParser.MarkdownParagraphType.TEXT, spacesAtStart, spacesAtEnd,
						spacesAtStart, 0, 0);
		}
	}

	public MarkdownLine rejoin(MarkdownLine toAppend) {
		src.arr[src.start + src.length] = ' ';
		return new MarkdownLine(src.rejoin(toAppend.src), type, spacesAtStart, toAppend.spacesAtEnd, prefixLength, level, number);
	}

	public MarkdownParagraph tokenize(final MarkdownParagraph parent) {

		final CharArrSubstring cleanedSrc = prefixLength == 0 ? src : src.substring(prefixLength);

		if(type != MarkdownParser.MarkdownParagraphType.CODE && type != MarkdownParser.MarkdownParagraphType.HLINE) {

			if(isPlainText()) {
				return new MarkdownParagraph(cleanedSrc, parent, type, null, level, number);
			} else {
				final IntArrayLengthPair tokens = MarkdownTokenizer.tokenize(cleanedSrc);
				return new MarkdownParagraph(cleanedSrc, parent, type, tokens.substringAsArray(0), level, number);
			}

		} else {
			return new MarkdownParagraph(cleanedSrc, parent, type, null, level, number);
		}
	}

	private boolean isPlainText() {

		for(int i = prefixLength; i < src.length; i++) {
			switch(src.arr[i + src.start]) {
				case '*':
				case '_':
				case '^':
				case '`':
				case '\\':
				case '[':
				case '~':
				case '#':
				case '&':
					return false;

				case '/':

					if(src.equalAt(i + 1, "u/") || src.equalAt(i + 1, "s/")) {
						return false;
					}

					break;

				case 'h':

					if(src.equalAt(i + 1, "ttp://") || src.equalAt(i + 1, "ttps://")) {
						return false;
					}

					break;

				case 'w':

					if(src.equalAt(i + 1, "ww.")) {
						return false;
					}

					break;

				case 's':
				case 'u':

					if(src.length > i + 1 && src.arr[src.start + i + 1] == '/') {
						return false;
					}

					break;

				default:
			}
		}

		return true;
	}
}
