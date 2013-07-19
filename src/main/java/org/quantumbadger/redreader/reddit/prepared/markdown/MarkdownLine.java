package org.quantumbadger.redreader.reddit.prepared.markdown;

public final class MarkdownLine {

	public final CharArrSubstring src;
	public final MarkdownParser.MarkdownParagraphType type;
	public final int spacesAtStart, spacesAtEnd;
	public final int prefixLength, level;

	MarkdownLine(CharArrSubstring src, MarkdownParser.MarkdownParagraphType type, int spacesAtStart, int spacesAtEnd,
				 int prefixLength, int level) {
		this.src = src;
		this.type = type;
		this.spacesAtStart = spacesAtStart;
		this.spacesAtEnd = spacesAtEnd;
		this.prefixLength = prefixLength;
		this.level = level;
	}

	public static MarkdownLine generate(final CharArrSubstring src) {

		final int spacesAtStart = src.countSpacesAtStart();
		final int spacesAtEnd = src.countSpacesAtEnd();

		if(spacesAtStart == src.length) {
			// New paragraph
			return new MarkdownLine(null, MarkdownParser.MarkdownParagraphType.EMPTY, 0, 0, 0, 0);
		}

		if(spacesAtStart >= 4) {
			return new MarkdownLine(src, MarkdownParser.MarkdownParagraphType.CODE, spacesAtStart, spacesAtEnd, 4, 0);
		}

		final char firstNonSpaceChar = src.charAt(spacesAtStart);

		switch(firstNonSpaceChar) {
			case '>': {

				final int level = src.countPrefixLevelIgnoringSpaces('>');
				final int prefixLen = src.countPrefixLengthIgnoringSpaces('>');

				return new MarkdownLine(src, MarkdownParser.MarkdownParagraphType.QUOTE, spacesAtStart, spacesAtEnd, prefixLen, level);
			}

			case '-':
			case '*':
			{

				if(src.length > spacesAtStart + 1 && src.charAt(spacesAtStart + 1) == ' ') {

					return new MarkdownLine(src, MarkdownParser.MarkdownParagraphType.BULLET, spacesAtStart, spacesAtEnd,
							spacesAtStart + 2, spacesAtStart == 0 ? 0 : 1);

				} else if(src.length >= 3 && src.isRepeatingChar('*', spacesAtStart, src.length - spacesAtEnd)) {
					return new MarkdownLine(src, MarkdownParser.MarkdownParagraphType.HLINE, 0, 0, 0, 0);

				} else {
					return new MarkdownLine(src, MarkdownParser.MarkdownParagraphType.TEXT, spacesAtStart, spacesAtEnd, 0, 0);
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
							spacesAtStart + num.length + 2, spacesAtStart == 0 ? 0 : 1);

				} else {
					return new MarkdownLine(src, MarkdownParser.MarkdownParagraphType.TEXT, spacesAtStart, spacesAtEnd, 0, 0);
				}
			}

			case '#':
				// TODO
				// TODO suffix length
				return new MarkdownLine(src, MarkdownParser.MarkdownParagraphType.TEXT, spacesAtStart, spacesAtEnd, 0, 0);

			default:
				return new MarkdownLine(src, MarkdownParser.MarkdownParagraphType.TEXT, spacesAtStart, spacesAtEnd, 0, 0);
		}
	}

	public MarkdownLine rejoin(MarkdownLine toAppend) {
		return new MarkdownLine(src.rejoin(toAppend.src), type, spacesAtStart, toAppend.spacesAtEnd, prefixLength, level);
	}

	public MarkdownParagraph tokenize(final MarkdownParagraph parent) {
		if(type != MarkdownParser.MarkdownParagraphType.CODE && type != MarkdownParser.MarkdownParagraphType.HLINE) {
			final IntArrayLengthPair tokens = MarkdownTokenizer.tokenize(src);
			return new MarkdownParagraph(src, parent, type, tokens.substringAsArray(prefixLength), level);
		} else {
			return new MarkdownParagraph(src, parent, type, null, level);
		}
	}
}
