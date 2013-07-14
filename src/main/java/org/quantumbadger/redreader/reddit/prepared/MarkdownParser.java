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

import android.graphics.Color;
import android.graphics.Typeface;
import android.net.Uri;
import android.text.SpannableStringBuilder;
import android.text.Spanned;
import android.text.style.StrikethroughSpan;
import android.text.style.StyleSpan;
import android.text.style.TypefaceSpan;
import android.text.style.URLSpan;
import android.view.View;
import android.view.ViewGroup;
import org.holoeverywhere.app.Activity;
import org.holoeverywhere.widget.LinearLayout;
import org.holoeverywhere.widget.TextView;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.LinkHandler;
import org.quantumbadger.redreader.views.LinkDetailsView;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

public final class MarkdownParser {

	public static enum MarkdownParagraphType {
		TEXT, CODE, BULLET, NUMBERED, QUOTE, HEADER, HLINE, EMPTY
	}

	public static final class MarkdownLine {

		public final CharArrSubstring src;
		public final MarkdownParagraphType type;
		public final int spacesAtStart, spacesAtEnd;
		public final int prefixLength, level;

		private MarkdownLine(CharArrSubstring src, MarkdownParagraphType type, int spacesAtStart, int spacesAtEnd,
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
				return new MarkdownLine(null, MarkdownParagraphType.EMPTY, 0, 0, 0, 0);
			}

			if(spacesAtStart >= 4) {
				return new MarkdownLine(src, MarkdownParagraphType.CODE, spacesAtStart, spacesAtEnd, 4, 0);
			}

			final char firstNonSpaceChar = src.charAt(spacesAtStart);

			switch(firstNonSpaceChar) {
				case '>': {

					final int level = src.countPrefixLevelIgnoringSpaces('>');
					final int prefixLen = src.countPrefixLengthIgnoringSpaces('>');

					return new MarkdownLine(src, MarkdownParagraphType.QUOTE, spacesAtStart, spacesAtEnd, prefixLen, level);
				}

				// TODO check when to start newline after quote

				case '-':
				case '*':
				{

					// TODO hline

					if(src.length > spacesAtStart + 1 && src.charAt(spacesAtStart + 1) == ' ') {

						return new MarkdownLine(src, MarkdownParagraphType.BULLET, spacesAtStart, spacesAtEnd,
								spacesAtStart + 2, spacesAtStart == 0 ? 0 : 1);

					} else {
						return new MarkdownLine(src, MarkdownParagraphType.TEXT, spacesAtStart, spacesAtEnd, 0, 0);
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

						return new MarkdownLine(src, MarkdownParagraphType.NUMBERED, spacesAtStart, spacesAtEnd,
								spacesAtStart + num.length + 2, spacesAtStart == 0 ? 0 : 1);

					} else {
						return new MarkdownLine(src, MarkdownParagraphType.TEXT, spacesAtStart, spacesAtEnd, 0, 0);
					}
				}

				case '#':
					// TODO
					// TODO suffix length
					return new MarkdownLine(src, MarkdownParagraphType.TEXT, spacesAtStart, spacesAtEnd, 0, 0);

				default:
					return new MarkdownLine(src, MarkdownParagraphType.TEXT, spacesAtStart, spacesAtEnd, 0, 0);
			}
		}

		public MarkdownLine rejoin(MarkdownLine toAppend) {
			return new MarkdownLine(src.rejoin(toAppend.src), type, spacesAtStart, toAppend.spacesAtEnd, prefixLength, level);
		}

		public MarkdownParagraph tokenize(final MarkdownParagraph parent) {
			if(type != MarkdownParagraphType.CODE) {
				final MarkdownTokenizer.IntArrayLengthPair tokens = MarkdownTokenizer.tokenize(src);
				return new MarkdownParagraph(src, parent, type, tokens.substringAsArray(prefixLength), level);
			} else {
				return new MarkdownParagraph(src, parent, MarkdownParagraphType.CODE, null, level);
			}
		}
	}

	public static final class CharArrSubstring {
		private final char[] arr;
		private final int start;
		public final int length;

		private CharArrSubstring(char[] arr, int start, int length) {
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
	}

	public static MarkdownParagraphGroup parse(final char[] raw) {

		//final LinkedList<MarkdownParagraph> result = new LinkedList<MarkdownParagraph>();

		// TODO

		// Starts with four spaces? Createa a code paragraph, chop off first four chars
		// Count number of spaces n until first non-space char c
		// If the line is all spaces, or the length is zero, start a new paragraph, consume any subsequent whitespace lines
		// If c == '-', '*', and is followed by a space
		// If n == 0: level 1 list item
		// Else, level 2 list item
		// If c == '>': quote
		// If c == '#': header. Count number, remove from end if present. End at next linebreak.
		// If c in [0-9], check if matches [0-9]+\. + (make sure followed by space)
		// If n == 0: level 1 numbered list item
		// Else, level 2 numbered list item
		// If current selection ends in double space, start a new paragraph
		// Else, continue last line

		final CharArrSubstring[] rawLines = CharArrSubstring.generateFromLines(raw);

		final MarkdownLine[] lines = new MarkdownLine[rawLines.length];

		for(int i = 0; i < rawLines.length; i++) {
			lines[i] = MarkdownLine.generate(rawLines[i]);
		}

		final ArrayList<MarkdownLine> mergedLines = new ArrayList<MarkdownLine>(rawLines.length);
		MarkdownLine currentLine = null;

		for(int i = 0; i < lines.length; i++) {

			if(currentLine != null) {

				switch(lines[i].type) {
					case BULLET:
					case NUMBERED:
					case HEADER:
					case CODE:
					case HLINE:
					case QUOTE:

						mergedLines.add(currentLine);
						currentLine = lines[i];
						break;

					case EMPTY:
						mergedLines.add(currentLine);
						currentLine = null;
						break;

					case TEXT:

						if(i > 0) {

							switch(lines[i - 1].type) {
								case QUOTE:
								case BULLET:
								case NUMBERED:
								case TEXT:

									if(lines[i - 1].spacesAtEnd >= 2) {
										mergedLines.add(currentLine);
										currentLine = lines[i];

									} else {
										currentLine = currentLine.rejoin(lines[i]);
									}
									break;

								case CODE:
								case HEADER:
								case HLINE:
									mergedLines.add(currentLine);
									currentLine = lines[i];
									break;
							}

						} else {
							throw new RuntimeException("Internal error: invalid paragrapher state");
						}

						break;
				}
			} else if(lines[i].type != MarkdownParagraphType.EMPTY) {
				currentLine = lines[i];
			}
		}

		if(currentLine != null) {
			mergedLines.add(currentLine);
		}

		final MarkdownParagraph[] paragraphs = new MarkdownParagraph[mergedLines.size()];

		for(int i = 0; i < paragraphs.length; i++) {
			paragraphs[i] = mergedLines.get(i).tokenize(i > 0 ? paragraphs[i - 1] : null);
		}

		return new MarkdownParagraphGroup(paragraphs);
	}

	public static final class MarkdownParagraphGroup {

		private final MarkdownParagraph[] paragraphs;

		public MarkdownParagraphGroup(final MarkdownParagraph[] paragraphs) {
			this.paragraphs = paragraphs;
		}

		public ViewGroup buildView(final Activity activity, final Integer textColor, final Float textSize) {

			final float dpScale = activity.getResources().getDisplayMetrics().density;

			final int paragraphSpacing = (int) (dpScale * 6);
			final int codeLineSpacing = (int) (dpScale * 3);
			final int quoteBarWidth = (int) (dpScale * 3);
			final int maxQuoteLevel = 5;

			final LinearLayout layout = new LinearLayout(activity);
			layout.setOrientation(android.widget.LinearLayout.VERTICAL);

			for(final MarkdownParagraph paragraph : paragraphs) {

				final TextView tv = new TextView(activity);
				tv.setText(paragraph.spanned);

				if(textColor != null) tv.setTextColor(textColor);
				if(textSize != null) tv.setTextSize(textSize);

				switch(paragraph.type) {

					case BULLET:
						break;

					case NUMBERED:
						break;

					case CODE:
						tv.setTypeface(General.getMonoTypeface(activity));
						tv.setText(paragraph.raw.arr, paragraph.raw.start + 4, paragraph.raw.length - 4);
						layout.addView(tv);
						((ViewGroup.MarginLayoutParams) tv.getLayoutParams()).topMargin
								= (paragraph.parent != null && paragraph.parent.type == MarkdownParagraphType.CODE
								? codeLineSpacing : paragraphSpacing);
						((ViewGroup.MarginLayoutParams) tv.getLayoutParams()).leftMargin = (int) (dpScale * 6);
						break;

					case HEADER:
						break;

					case HLINE: {

						final View hLine = new View(activity);
						layout.addView(hLine);
						final ViewGroup.MarginLayoutParams hLineParams = (ViewGroup.MarginLayoutParams) hLine.getLayoutParams();
						hLineParams.width = ViewGroup.LayoutParams.MATCH_PARENT;
						hLineParams.height = (int) dpScale;
						hLineParams.setMargins((int)(dpScale * 15), paragraphSpacing, (int)(dpScale * 15), 0);
						hLine.setBackgroundColor(Color.rgb(128, 128, 128));
						break;
					}

					case QUOTE: {

						final LinearLayout quoteLayout = new LinearLayout(activity);

						for(int lvl = 0; lvl < Math.min(maxQuoteLevel, paragraph.level); lvl++) {
							final View quoteIndent = new View(activity);
							quoteLayout.addView(quoteIndent);
							quoteIndent.setBackgroundColor(Color.rgb(128, 128, 128));
							quoteIndent.getLayoutParams().width = quoteBarWidth;
							quoteIndent.getLayoutParams().height = ViewGroup.LayoutParams.MATCH_PARENT;
							((ViewGroup.MarginLayoutParams)quoteIndent.getLayoutParams()).rightMargin = quoteBarWidth;
						}

						quoteLayout.addView(tv);
						layout.addView(quoteLayout);

						if(paragraph.parent != null && paragraph.parent.type == MarkdownParagraphType.QUOTE) {
							((ViewGroup.MarginLayoutParams)tv.getLayoutParams()).topMargin = paragraphSpacing;
						} else {
							((ViewGroup.MarginLayoutParams)quoteLayout.getLayoutParams()).topMargin = paragraphSpacing;
						}

						break;
					}

					case TEXT:

						layout.addView(tv);
						((ViewGroup.MarginLayoutParams) tv.getLayoutParams()).topMargin = paragraphSpacing;

						break;

					case EMPTY:
						throw new RuntimeException("Internal error: empty paragraph when building view");
				}

				for(final MarkdownParagraph.Link link : paragraph.links) {

					final LinkDetailsView ldv = new LinkDetailsView(activity, link.title, link.subtitle);
					layout.addView(ldv);

					final int linkMarginPx = Math.round(dpScale * 8);
					((LinearLayout.LayoutParams) ldv.getLayoutParams()).setMargins(0, linkMarginPx, 0, linkMarginPx);
					ldv.getLayoutParams().width = ViewGroup.LayoutParams.MATCH_PARENT;

					ldv.setOnClickListener(new View.OnClickListener() {
						public void onClick(View v) {
							link.onClicked(activity);
						}
					});
				}
			}

			return layout;
		}
	}



	// TODO spoilers
	// TODO number links
	public static final class MarkdownParagraph {

		final CharArrSubstring raw;
		final MarkdownParagraph parent;
		final MarkdownParagraphType type;
		final int[] tokens;
		final int level;

		final Spanned spanned;
		final List<Link> links;

		public class Link {
			private final String title, subtitle, url;

			public Link(String title, String subtitle, String url) {
				this.title = title;
				this.subtitle = subtitle;
				this.url = url;
			}

			public void onClicked(Activity activity) {
				LinkHandler.onLinkClicked(activity, url, false);
			}
		}

		public MarkdownParagraph(CharArrSubstring raw, MarkdownParagraph parent, MarkdownParagraphType type,
								 int[] tokens, int level) {
			this.raw = raw;
			this.parent = parent;
			this.type = type;
			this.tokens = tokens;
			this.level = level;

			links = new ArrayList<Link>();
			spanned = internalGenerateSpanned();
		}

		// TODO superscript
		private Spanned internalGenerateSpanned() {

			if(type == MarkdownParagraphType.CODE) {
				return null;
			}

			final SpannableStringBuilder builder = new SpannableStringBuilder();

			// TODO double check these start at builder.length(), not i
			// TODO bold/italic using underscores, taking into account special cases (e.g. a_b_c vs ._b_.)
			int boldStart = -1, italicStart = -1, strikeStart = -1, linkStart = -1;

			for(int i = 0; i < tokens.length; i++) {

				final int token = tokens[i];

				switch(token) {

					case MarkdownTokenizer.TOKEN_ASTERISK:

						if(italicStart < 0) {
							italicStart = builder.length();
						} else {
							builder.setSpan(new StyleSpan(Typeface.ITALIC), italicStart, builder.length(),
									Spanned.SPAN_INCLUSIVE_EXCLUSIVE);
							italicStart = -1;
						}

						break;

					case MarkdownTokenizer.TOKEN_ASTERISK_DOUBLE:

						if(boldStart < 0) {
							boldStart = builder.length();
						} else {
							builder.setSpan(new StyleSpan(Typeface.BOLD), boldStart, builder.length(),
									Spanned.SPAN_INCLUSIVE_EXCLUSIVE);
							boldStart = -1;
						}

						break;


					case MarkdownTokenizer.TOKEN_TILDE_DOUBLE:

						if(strikeStart == -1) {
							strikeStart = builder.length();

						} else {
							builder.setSpan(new StrikethroughSpan(), strikeStart, builder.length(),
									Spanned.SPAN_INCLUSIVE_EXCLUSIVE);
							strikeStart = -1;
						}

						break;

					case MarkdownTokenizer.TOKEN_GRAVE:

						final int codeStart = builder.length();

						while(tokens[++i] != MarkdownTokenizer.TOKEN_GRAVE) {
							builder.append((char)tokens[i]);
						}

						builder.setSpan(new TypefaceSpan("monospace"), codeStart, builder.length(),
								Spanned.SPAN_INCLUSIVE_EXCLUSIVE);

						break;

					case MarkdownTokenizer.TOKEN_BRACKET_SQUARE_OPEN:
						linkStart = builder.length();
						break;

					case MarkdownTokenizer.TOKEN_BRACKET_SQUARE_CLOSE:

						final int urlStart = indexOf(tokens, MarkdownTokenizer.TOKEN_PAREN_OPEN, i + 1);
						final int urlEnd = indexOf(tokens, MarkdownTokenizer.TOKEN_PAREN_CLOSE, urlStart + 1);

						final StringBuilder urlBuilder = new StringBuilder(urlEnd - urlStart);

						for(int j = urlStart + 1; j < urlEnd; j++) {
							urlBuilder.append((char)tokens[j]);
						}

						final String linkText = String.valueOf(builder.subSequence(linkStart, builder.length()));
						final String url = urlBuilder.toString();

						if(url.startsWith("/spoiler")) {

							builder.delete(linkStart, builder.length());
							builder.append("[Spoiler]");

							final Uri.Builder spoilerUriBuilder = Uri.parse("rr://msg/").buildUpon();
							spoilerUriBuilder.appendQueryParameter("title", "Spoiler");
							spoilerUriBuilder.appendQueryParameter("message", linkText);

							links.add(new Link("Spoiler", null, spoilerUriBuilder.toString()));

						} else if(url.startsWith("#") && url.length() > 3) {

							final String subtitle;
							switch(url.charAt(1)) {
								case 'b':
									subtitle = "Spoiler: Book";
									break;
								case 'g':
									subtitle = "Spoiler: Speculation";
									break;
								case 's':
								default:
									subtitle = "Spoiler";
									break;
							}

							final Uri.Builder spoilerUriBuilder = Uri.parse("rr://msg/").buildUpon();
							spoilerUriBuilder.appendQueryParameter("title", subtitle);
							spoilerUriBuilder.appendQueryParameter("message", url.substring(3));

							links.add(new Link(linkText, subtitle, spoilerUriBuilder.toString()));

						} else {
							links.add(new Link(linkText, url, url));
						}

						// TODO
						//builder.insert(linkStart, "[NUMBER HERE]");

						builder.setSpan(new URLSpan(url), linkStart, builder.length(), Spanned.SPAN_INCLUSIVE_EXCLUSIVE);

						i = urlEnd;

						break;

					case MarkdownTokenizer.TOKEN_CARET:
						// TODO
						builder.append('^');
						break;

					default:
						builder.append((char)token);
						break;
				}
			}

			return builder;
		}

		private static int indexOf(final int[] haystack, final int needle, final int startPos) {
			for(int i = startPos; i < haystack.length; i++) {
				if(haystack[i] == needle) return i;
			}
			return -1;
		}
	}
}
