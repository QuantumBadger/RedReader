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

import android.graphics.Typeface;
import android.text.SpannableStringBuilder;
import android.text.Spanned;
import android.text.SpannedString;
import android.text.style.StyleSpan;
import android.text.style.TypefaceSpan;

import java.util.LinkedList;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public final class MarkdownParser {

	private static final Pattern
			newlinePattern = Pattern.compile("\\n\\n+"),
			lineStartNumPattern = Pattern.compile("^(([0-9]+)(?:\\. |\\) ?)).*$"),
			lineStartHeaderPattern = Pattern.compile("^((#+) *).*");

	public static MarkdownParagraph[] splitIntoParagraphs(final String rawMarkdown) {

		final String[] rawParagraphs = newlinePattern.split(rawMarkdown);

		// TODO use array
		final LinkedList<MarkdownParagraph> paragraphs = new LinkedList<MarkdownParagraph>();

		for(final String rawParagraph : rawParagraphs) {
			final MarkdownParagraph parent = paragraphs.isEmpty() ? null : paragraphs.getLast();
			final MarkdownParagraph paragraph = MarkdownParagraph.create(rawParagraph, parent);
			if(paragraph != null) paragraphs.addLast(paragraph);
		}

		return paragraphs.toArray(new MarkdownParagraph[paragraphs.size()]);

		// TODO one pass through each raw paragraph, adding raw text to a string builder
		// Record the position of opening */_/**/etc, create span on close
		// Inline code
	}

	private static final class MarkdownParagraph {

		public static enum Type {
			TEXT, CODE, BULLET, NUMBERED, QUOTE, HEADER
		}

		final String raw;
		final MarkdownParagraph parent;
		final Type type;
		final int number;

		public static MarkdownParagraph create(final String rawWithPrefix, final MarkdownParagraph parent) {

			if(rawWithPrefix.trim().length() < 1) return null;

			final char firstChar = rawWithPrefix.charAt(0);

			switch(firstChar) {

				case ' ':
					if(rawWithPrefix.startsWith("    ")) {
						return new MarkdownParagraph(rawWithPrefix.substring(4), parent, Type.CODE);
					}
					break;

				// TODO multiple quote levels
				case '>':
					if(rawWithPrefix.charAt(1) == ' ') {
						return new MarkdownParagraph(rawWithPrefix.substring(2), parent, Type.QUOTE);
					} else {
						return new MarkdownParagraph(rawWithPrefix.substring(1), parent, Type.QUOTE);
					}

				case '*':
				case '-':
				case '+':
					if(rawWithPrefix.charAt(1) == ' ') {
						return new MarkdownParagraph(rawWithPrefix.substring(2), parent, Type.BULLET);
					}
					break;

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
					final Matcher numMatcher = lineStartNumPattern.matcher(rawWithPrefix);
					if(numMatcher.find()) {

						return new MarkdownParagraph(rawWithPrefix.substring(numMatcher.group(1).length()),
								parent, Type.NUMBERED, Integer.parseInt(numMatcher.group(2)));

					}
					break;

				case '#':
					final Matcher headerMatcher = lineStartHeaderPattern.matcher(rawWithPrefix);
					if(headerMatcher.find()) {

						final String hashes = headerMatcher.group(2);
						final int level = hashes.length();
						final String raw;

						if(rawWithPrefix.endsWith(" " + hashes)) {
							raw = rawWithPrefix.substring(headerMatcher.group(1).length(), rawWithPrefix.length() - level - 1);
						} else {
							raw = rawWithPrefix.substring(headerMatcher.group(1).length());
						}

						return new MarkdownParagraph(raw, parent, Type.HEADER, level);
					}
					break;
			}

			return new MarkdownParagraph(rawWithPrefix, parent, Type.TEXT);
		}

		private MarkdownParagraph(final String raw, final MarkdownParagraph parent, final Type type, final int number) {
			this.raw = raw.replace('\n', ' ');
			this.parent = parent;
			this.type = type;
			this.number = number;
		}

		private MarkdownParagraph(String raw, MarkdownParagraph parent, Type type) {
			this(raw, parent, type, 0);
		}

		public float getPaddingDp() {

			// TODO switch on type, parent type
			// TODO detect two spaces at end of raw

			return 10;

		}

		// TODO detect links in code
		public Spanned generatedSpanned() {

			if(type == Type.CODE) {
				final SpannableStringBuilder builder = new SpannableStringBuilder(raw);
				builder.setSpan(new TypefaceSpan("VeraMono"), 0, raw.length(), Spanned.SPAN_INCLUSIVE_EXCLUSIVE);
				return builder;
			}

			// TODO output resized raw for headers

			final SpannableStringBuilder builder = new SpannableStringBuilder();
			final char[] rawArr = raw.toCharArray();

			// TODO ensure these start at builder.length(), not i
			int boldStart = -1, italicStart = -1, strikeStart = -1, linkStart = -1, codeStart = -1;

			for(int i = 0; i < rawArr.length; i++) {

				final char c = rawArr[i];

				switch(c) {

					case '_':
					case '*':

						if(i < rawArr.length - 1 && rawArr[i + 1] == c) {
							// Double: bold

							i++;

							if(boldStart == -1) {

								if(indexOf(rawArr, new char[]{c, c}, i + 1) != -1) {
									boldStart = builder.length();

								} else {
									builder.append(c);
									builder.append(c);
								}

							} else {
								builder.setSpan(new StyleSpan(Typeface.BOLD), boldStart, builder.length(),
										Spanned.SPAN_INCLUSIVE_EXCLUSIVE);
								italicStart = -1;
							}

						} else {
							// Single: italic

							if(italicStart == -1) {

								if(indexOf(rawArr, c, i + 1) != -1) {
									italicStart = builder.length();

								} else {
									builder.append(c);
								}

							} else {
								builder.setSpan(new StyleSpan(Typeface.ITALIC), italicStart, builder.length(),
										Spanned.SPAN_INCLUSIVE_EXCLUSIVE);
								italicStart = -1;
							}

						}

						break;

					case '~':
						// TODO
						break;

					case '`':
						// TODO just add raw until closing `
						break;

					case '[':

						boolean linkSuccess = false;

						if(linkStart == -1) {

							final int closePos = indexOf(rawArr, ']', i + 1);
							if(closePos != -1) {

								final int urlOpenPos = indexOf(rawArr, '(', closePos + 1);
								if(urlOpenPos != -1) {

									if(isSpaces(rawArr, closePos + 1, urlOpenPos - closePos - 1)) {

										final int urlClosePos = indexOf(rawArr, ')', urlOpenPos + 1);
										if(urlClosePos != -1) {

											if(indexOf(rawArr, '[', i + 1, closePos) == -1) {
												linkStart = builder.length();
												linkSuccess = true;
											}
										}
									}
								}
							}
						} else throw new RuntimeException("Internal error: unexpected '[' during parse");

						if(!linkSuccess) builder.append('[');
						break;

					case ']':
						// TODO
						break;

					case '\\':
						// TODO
						break;
				}
			}

			return builder;
		}

		private boolean isSpaces(final char[] haystack, final int start, final int len) {
			final int end = start + len;
			for(int i = start; i < end; i++) {
				if(haystack[i] != ' ') return false;
			}
			return true;
		}

		private int indexOf(char[] haystack, char needle, int fromIndexInclusive, int endIndexExclusive) {
			for(int i = fromIndexInclusive; i < Math.min(endIndexExclusive, haystack.length); i++) {
				if(haystack[i] == needle) return i;
			}
			return -1;
		}

		private int indexOf(char[] haystack, char needle, int fromIndexInclusive) {
			for(int i = fromIndexInclusive; i < haystack.length; i++) {
				if(haystack[i] == needle) return i;
			}
			return -1;
		}

		private int indexOf(char[] haystack, char[] needle, int fromIndexInclusive) {
			final int end = haystack.length - needle.length;
			for(int i = fromIndexInclusive; i <= end; i++) {
				if(equals(haystack, needle, i)) return i;
			}
			return -1;
		}

		private boolean equals(char[] haystack, char[] needle, int haystackPos) {
			for(int i = 0; i < needle.length; i++) {
				if(haystack[haystackPos + i] != needle[i]) return false;
			}
			return true;
		}
	}

}
