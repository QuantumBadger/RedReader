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

package org.quantumbadger.redreader.reddit.prepared.html;

import android.content.Context;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;
import org.apache.commons.text.StringEscapeUtils;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.common.RRError;
import org.quantumbadger.redreader.reddit.prepared.bodytext.BlockType;
import org.quantumbadger.redreader.reddit.prepared.bodytext.BodyElement;
import org.quantumbadger.redreader.reddit.prepared.bodytext.BodyElementRRError;
import org.quantumbadger.redreader.reddit.prepared.bodytext.BodyElementVerticalSequence;

import java.util.ArrayList;

public class HtmlReader {

	public enum TokenType {
		TAG_START,
		TAG_END,
		TAG_START_AND_END,
		TEXT,
		EOF
	}

	public static class Token {

		public static final Token EOF = new Token(TokenType.EOF, "", null, null, null);

		@NonNull public final TokenType type;
		@NonNull public final String text;
		@Nullable public final String href;
		@Nullable public final String cssClass;
		@Nullable public final String title;

		public Token(
				@NonNull final TokenType type,
				@NonNull final String text,
				@Nullable final String href,
				@Nullable final String cssClass,
				@Nullable final String title) {
			this.type = type;
			this.text = text;
			this.href = href;
			this.cssClass = cssClass;
			this.title = title;
		}

		@NonNull
		@Override
		public String toString() {
			return type.name() + "(" + text + ")";
		}
	}

	@NonNull private final String mHtml;
	private int mPos = 0;

	private boolean mPreformattedTextPending = false;

	public HtmlReader(@NonNull final String html) {
		mHtml = html;
	}

	private static String normaliseWhitespace(@NonNull final String html) {

		final StringBuilder result = new StringBuilder(html.length());

		boolean lastCharWasWhitespace = false;

		for(int i = 0; i < html.length(); i++) {

			final char c = html.charAt(i);

			if(c != '\n' && c != '\r') {
				if(isWhitespace(c)) {
					if(!lastCharWasWhitespace) {
						result.append(" ");
						lastCharWasWhitespace = true;
					}

				} else {
					lastCharWasWhitespace = false;
					result.append(c);
				}
			}
		}

		return result.toString();
	}

	private static boolean isWhitespace(final char c) {
		return c == ' ' || c == '\t' || c == '\r' || c == '\n';
	}

	private static boolean isNameChar(final char c) {

		switch(c) {
			case 0:
			case ' ':
			case '\'':
			case '"':
			case '>':
			case '/':
			case '=':
				return false;

			default:
				return true;
		}
	}

	private String readName() throws MalformedHtmlException {

		final StringBuilder result = new StringBuilder(16);

		try {
			while(isNameChar(mHtml.charAt(mPos))) {
				result.append(mHtml.charAt(mPos));
				mPos++;
			}

		} catch(final IndexOutOfBoundsException e) {
			throw new MalformedHtmlException(
					"Reached EOF while reading name",
					mHtml,
					mPos,
					e);
		}

		if(result.length() == 0) {
			throw new MalformedHtmlException("Got zero-length name", mHtml, mPos);
		}

		return result.toString();
	}

	private String readAndUnescapeUntil(final char endChar) {

		final StringBuilder result = new StringBuilder(64);

		while(mPos < mHtml.length() && mHtml.charAt(mPos) != endChar) {
			result.append(mHtml.charAt(mPos));
			mPos++;
		}

		return StringEscapeUtils.unescapeHtml4(result.toString());
	}

	private boolean tryAccept(final char c) {

		if(mPos < mHtml.length() && mHtml.charAt(mPos) == c) {
			mPos++;
			return true;
		}

		return false;
	}

	private void accept(final char c) throws MalformedHtmlException {

		try {
			if(mHtml.charAt(mPos) != c) {
				throw new MalformedHtmlException("Expecting " + c, mHtml, mPos);
			}

		} catch(final IndexOutOfBoundsException e) {
			throw new MalformedHtmlException("Unexpected EOF", mHtml, mPos, e);
		}

		mPos++;
	}

	private void skipWhitespace() {

		while(mPos < mHtml.length() && isWhitespace(mHtml.charAt(mPos))) {
			mPos++;
		}
	}

	private void skipNewlines() {

		while(mPos < mHtml.length() && mHtml.charAt(mPos) == '\n') {
			mPos++;
		}
	}

	@NonNull
	public Token readNext() throws MalformedHtmlException {

		try {

			mainLoop:
			while(true) {

				skipNewlines();

				if(mPos >= mHtml.length()) {
					// End of data
					return Token.EOF;
				}

				if(mHtml.charAt(mPos) == '<') {

					mPos++;
					skipWhitespace();

					final TokenType type;

					if(mHtml.charAt(mPos) == '!') {

						// Comment
						mPos++;
						accept('-');
						accept('-');

						while(true) {

							if(mHtml.charAt(mPos) == '-'
									&& mHtml.charAt(mPos + 1) == '-'
									&& mHtml.charAt(mPos + 2) == '>') {

								mPos += 3;
								continue mainLoop;

							} else {
								mPos++;
							}
						}

					}

					if(mHtml.charAt(mPos) == '/') {
						type = TokenType.TAG_END;
						mPos++;
						skipWhitespace();

					} else {
						type = TokenType.TAG_START;
					}

					final String tagName = readName();
					@Nullable String href = null;
					@Nullable String cssClass = null;
					@Nullable String title = null;

					if(tagName.equalsIgnoreCase("pre")) {
						mPreformattedTextPending = true;
					}

					skipWhitespace();

					while(mHtml.charAt(mPos) != '>') {

						if(tryAccept('/')) {
							skipWhitespace();
							accept('>');
							return new Token(
									TokenType.TAG_START_AND_END,
									tagName,
									href,
									cssClass,
									title);
						}

						final String propertyName = readName();

						if(tryAccept('=')) {
							accept('"');
							final String value = readAndUnescapeUntil('"');
							accept('"');
							skipWhitespace();

							if(propertyName.equalsIgnoreCase("href")) {
								href = value;
							} else if(propertyName.equalsIgnoreCase("class")) {
								cssClass = value;
							} else if(propertyName.equalsIgnoreCase("title")) {
								title = value;
							}
						}
					}

					accept('>');

					return new Token(type, tagName, href, cssClass, title);

				} else {

					if(mPreformattedTextPending) {

						mPreformattedTextPending = false;

						String preformattedText = readAndUnescapeUntil('<');

						if(preformattedText.endsWith("\n")) {
							preformattedText = preformattedText.substring(
									0,
									preformattedText.length() - 1);
						}

						return new Token(
								TokenType.TEXT,
								preformattedText,
								null,
								null,
								null);
					}

					// Raw text
					return new Token(
							TokenType.TEXT,
							normaliseWhitespace(readAndUnescapeUntil('<')),
							null,
							null,
							null);
				}
			}

		} catch(final IndexOutOfBoundsException e) {
			throw new MalformedHtmlException("Unexpected EOF", mHtml, mPos, e);
		}
	}

	public static BodyElement parse(
			@Nullable String html,
			@NonNull final AppCompatActivity activity) {

		if(html == null) {
			html = "";
		}

		final Context applicationContext = activity.getApplicationContext();

		try {
			final HtmlReaderPeekable reader
					= new HtmlReaderPeekable(new HtmlReader(html));

			HtmlRawElement rootElement;

			if(reader.peek().type == TokenType.EOF) {
				// Empty comment
				rootElement = new HtmlRawElementPlainText("");

			} else {
				rootElement = HtmlRawElement.readFrom(reader);
			}

			if(!(rootElement instanceof HtmlRawElementBlock)) {
				rootElement = new HtmlRawElementBlock(BlockType.NORMAL_TEXT, rootElement);
			}

			final HtmlRawElementBlock reduced
					= ((HtmlRawElementBlock)rootElement).reduce(
					new HtmlTextAttributes(),
					activity);

			final ArrayList<BodyElement> generated = new ArrayList<>();

			reduced.generate(activity, generated);

			return new BodyElementVerticalSequence(generated);

		} catch(final MalformedHtmlException e) {
			return new BodyElementRRError(
					new RRError(
							applicationContext.getString(R.string.error_title_malformed_html),
							applicationContext.getString(R.string.error_message_malformed_html),
							true,
							e));

		} catch(final Exception e) {
			return new BodyElementRRError(
					new RRError(
							applicationContext.getString(R.string.error_parse_title),
							applicationContext.getString(R.string.error_parse_message),
							true,
							e));
		}
	}

	@NonNull
	public String getHtml() {
		return mHtml;
	}

	public int getPos() {
		return mPos;
	}
}
