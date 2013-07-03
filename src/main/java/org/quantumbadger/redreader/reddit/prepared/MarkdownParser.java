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

import android.content.Context;
import android.graphics.Color;
import android.graphics.Typeface;
import android.text.SpannableStringBuilder;
import android.text.Spanned;
import android.text.style.StrikethroughSpan;
import android.text.style.StyleSpan;
import android.text.style.TypefaceSpan;
import android.view.ViewGroup;
import android.widget.TextView;
import org.holoeverywhere.widget.LinearLayout;

import java.util.LinkedList;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public final class MarkdownParser {

	private static final Pattern
			newlinePattern = Pattern.compile("\\n\\n+"),
			lineStartNumPattern = Pattern.compile("^(([0-9]+)(?:\\. |\\) ?)).*$"),
			lineStartHeaderPattern = Pattern.compile("^((#+) *).*");


	// TODO spoilers
	public static final class MarkdownParagraphGroup {

		private final MarkdownParagraph[] paragraphs;
		private final Spanned[] spanneds;

		public MarkdownParagraphGroup(final String rawMarkdown) {

			final String[] rawParagraphs = newlinePattern.split(rawMarkdown);

			// TODO use array
			final LinkedList<MarkdownParagraph> paragraphs = new LinkedList<MarkdownParagraph>();

			for(final String rawParagraph : rawParagraphs) {
				final MarkdownParagraph parent = paragraphs.isEmpty() ? null : paragraphs.getLast();
				final MarkdownParagraph paragraph = MarkdownParagraph.create(rawParagraph, parent);
				if(paragraph != null) paragraphs.addLast(paragraph);
			}

			this.paragraphs = paragraphs.toArray(new MarkdownParagraph[paragraphs.size()]);

			spanneds = new Spanned[this.paragraphs.length];

			for(int i = 0; i < spanneds.length; i++) {
				spanneds[i] = this.paragraphs[i].generateSpanned();
			}
		}

		// TODO take into account size, colour, link click listener, etc
		public ViewGroup buildView(Context context) {

			final float dpScale = context.getResources().getDisplayMetrics().density;

			final LinearLayout layout = new LinearLayout(context);
			layout.setOrientation(android.widget.LinearLayout.VERTICAL);

			for(int i = 0; i < paragraphs.length; i++) {
				final TextView tv = new TextView(context);
				tv.setTextColor(Color.BLACK);
				tv.setText(spanneds[i]);
				layout.addView(tv);
				((ViewGroup.MarginLayoutParams) tv.getLayoutParams()).topMargin
						= (int) (dpScale * paragraphs[i].getPaddingDp());
			}

			return layout;
		}
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

			return parent == null ? 0 : 10;

		}

		// TODO linkify code
		// TODO linkify in general
		public Spanned generateSpanned() {

			if(type == Type.CODE) {
				final SpannableStringBuilder builder = new SpannableStringBuilder(raw);
				builder.setSpan(new TypefaceSpan("VeraMono"), 0, raw.length(), Spanned.SPAN_INCLUSIVE_EXCLUSIVE);
				return builder;
			}

			// TODO output resized raw for headers

			final SpannableStringBuilder builder = new SpannableStringBuilder();
			final char[] rawArr = raw.toCharArray();

			// TODO double check these start at builder.length(), not i
			// TODO bold/italic using underscores, taking into account special cases (e.g. a_b_c vs ._b_.)
			int boldStart = -1, italicStart = -1, strikeStart = -1, linkStart = -1;

			for(int i = 0; i < rawArr.length; i++) {

				final char c = rawArr[i];

				switch(c) {

					case '*':

						if(i < rawArr.length - 1 && rawArr[i + 1] == c) {
							// Double: bold

							i++;

							if(boldStart == -1) {

								if(indexOfEscaped(rawArr, new char[]{c, c}, i + 1) != -1) {
									boldStart = builder.length();

								} else {
									builder.append(c);
									builder.append(c);
								}

							} else {
								builder.setSpan(new StyleSpan(Typeface.BOLD), boldStart, builder.length(),
										Spanned.SPAN_INCLUSIVE_EXCLUSIVE);
								boldStart = -1;
							}

						} else {
							// Single: italic

							if(italicStart == -1) {

								if(indexOfEscaped(rawArr, c, i + 1) != -1) {
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

						if(i < rawArr.length - 1 && rawArr[i + 1] == '~') {
							// Double: strikethrough

							i++;

							if(strikeStart == -1) {

								if(indexOfEscaped(rawArr, new char[]{'~', '~'}, i + 1) != -1) {
									strikeStart = builder.length();

								} else builder.append("~~");

							} else {
								builder.setSpan(new StrikethroughSpan(), strikeStart, builder.length(),
										Spanned.SPAN_INCLUSIVE_EXCLUSIVE);
								strikeStart = -1;
							}

						} else builder.append('~');

						break;

					case '`':

						final int codeStart = builder.length();
						final int codeEndRaw = indexOfEscaped(rawArr, '`', i + 1);

						if(codeEndRaw > 0) {
							builder.append(raw.substring(i + 1, codeEndRaw));
							i = codeEndRaw;
							builder.setSpan(new TypefaceSpan("VeraMono"), codeStart, builder.length(),
									Spanned.SPAN_INCLUSIVE_EXCLUSIVE);

						} else builder.append('`');

						break;

					case '[':

						boolean linkSuccess = false;

						if(linkStart == -1) {

							final int closePos = indexOfEscaped(rawArr, ']', i + 1);
							if(closePos != -1) {

								final int urlOpenPos = indexOfEscaped(rawArr, '(', closePos + 1);
								if(urlOpenPos != -1) {

									if(isSpaces(rawArr, closePos + 1, urlOpenPos - closePos - 1)) {

										final int urlClosePos = indexOfEscaped(rawArr, ')', urlOpenPos + 1);
										if(urlClosePos != -1) {

											if(indexOfEscaped(rawArr, '[', i + 1, closePos) == -1) {
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
						if(i < rawArr.length - 1) builder.append(rawArr[++i]);
						break;

					default:
						builder.append(c);
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

		private int indexOfEscaped(char[] haystack, char needle, int fromIndexInclusive, int endIndexExclusive) {
			for(int i = fromIndexInclusive; i < Math.min(endIndexExclusive, haystack.length); i++) {
				final char c = haystack[i];
				if(c == '\\') i++;
				else if(c == needle) return i;
			}
			return -1;
		}

		private int indexOfEscaped(char[] haystack, char needle, int fromIndexInclusive) {
			for(int i = fromIndexInclusive; i < haystack.length; i++) {
				final char c = haystack[i];
				if(c == '\\') i++;
				else if(c == needle) return i;
			}
			return -1;
		}

		private int indexOfEscaped(char[] haystack, char[] needle, int fromIndexInclusive) {
			final int end = haystack.length - needle.length;
			for(int i = fromIndexInclusive; i <= end; i++) {
				if(haystack[i] == '\\') i++;
				else if(equals(haystack, needle, i)) return i;
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
