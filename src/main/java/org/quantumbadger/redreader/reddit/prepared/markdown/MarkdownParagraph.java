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

package org.quantumbadger.redreader.reddit.prepared.markdown;

import android.graphics.Typeface;
import android.net.Uri;
import android.support.v7.app.AppCompatActivity;
import android.text.SpannableString;
import android.text.SpannableStringBuilder;
import android.text.Spanned;
import android.text.style.*;
import org.quantumbadger.redreader.common.LinkHandler;

import java.util.ArrayList;
import java.util.List;

// TODO number links
public final class MarkdownParagraph {

	final CharArrSubstring raw;
	final MarkdownParagraph parent;
	final MarkdownParser.MarkdownParagraphType type;
	final int[] tokens;
	final int level, number;

	final Spanned spanned;
	final List<Link> links;

	public class Link {
		final String title;
		final String subtitle;
		private final String url;

		public Link(String title, String subtitle, String url) {
			this.title = title;
			this.subtitle = subtitle;
			this.url = url;
		}

		public void onClicked(AppCompatActivity activity) {
			LinkHandler.onLinkClicked(activity, url, false);
		}
	}

	public MarkdownParagraph(CharArrSubstring raw, MarkdownParagraph parent, MarkdownParser.MarkdownParagraphType type,
							 int[] tokens, int level, int number) {
		this.raw = raw;
		this.parent = parent;
		this.type = type;
		this.tokens = tokens;
		this.level = level;
		this.number = number;

		links = new ArrayList<>();
		spanned = internalGenerateSpanned();

		if(tokens == null && raw != null) raw.replaceUnicodeSpaces();
	}

	private Spanned internalGenerateSpanned() {

		if(type == MarkdownParser.MarkdownParagraphType.CODE || type == MarkdownParser.MarkdownParagraphType.HLINE) {
			return null;
		}

		if(tokens == null) {
			return new SpannableString(raw.toString());
		}

		final SpannableStringBuilder builder = new SpannableStringBuilder();

		int boldStart = -1, italicStart = -1, strikeStart = -1, linkStart = -1, caretStart = -1;

		for(int i = 0; i < tokens.length; i++) {

			final int token = tokens[i];

			switch(token) {

				case MarkdownTokenizer.TOKEN_ASTERISK:
				case MarkdownTokenizer.TOKEN_UNDERSCORE:

					if(italicStart < 0) {
						italicStart = builder.length();
					} else {
						builder.setSpan(new StyleSpan(Typeface.ITALIC), italicStart, builder.length(),
								Spanned.SPAN_INCLUSIVE_EXCLUSIVE);
						italicStart = -1;
					}

					break;

				case MarkdownTokenizer.TOKEN_ASTERISK_DOUBLE:
				case MarkdownTokenizer.TOKEN_UNDERSCORE_DOUBLE:

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

					} else if(url.length() > 3 && url.charAt(2) == ' '
							&& (url.charAt(0) == '#' || url.charAt(0) == '/')) {

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
					if(caretStart < 0) {
						caretStart = builder.length();
					} else {
						builder.append(' ');
					}
					break;

				case ' ':

					builder.append(' ');

					if(caretStart >= 0) {
						builder.setSpan(new SuperscriptSpan(), caretStart, builder.length(), Spanned.SPAN_INCLUSIVE_EXCLUSIVE);
						builder.setSpan(new RelativeSizeSpan(0.6f), caretStart, builder.length(), Spanned.SPAN_INCLUSIVE_EXCLUSIVE);
						caretStart = -1;
					}

					break;

				default:
					builder.append((char)token);
					break;
			}
		}

		if(caretStart >= 0) {
			builder.setSpan(new SuperscriptSpan(), caretStart, builder.length(), Spanned.SPAN_INCLUSIVE_EXCLUSIVE);
			builder.setSpan(new RelativeSizeSpan(0.6f), caretStart, builder.length(), Spanned.SPAN_INCLUSIVE_EXCLUSIVE);
		}

		if(type == MarkdownParser.MarkdownParagraphType.HEADER) {
			while(builder.length() > 0 && builder.charAt(builder.length() - 1) == '#') {
				builder.delete(builder.length() - 1, builder.length());
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

	public boolean isEmpty() {

		if(type == MarkdownParser.MarkdownParagraphType.HLINE) return false;
		if(type == MarkdownParser.MarkdownParagraphType.EMPTY) return true;

		if(tokens == null) {
			return raw.countSpacesAtStart() == raw.length;

		} else {
			for(final int token : tokens) {
				if(!MarkdownTokenizer.isUnicodeWhitespace(token)) return false;
			}
			return true;
		}
	}
}
