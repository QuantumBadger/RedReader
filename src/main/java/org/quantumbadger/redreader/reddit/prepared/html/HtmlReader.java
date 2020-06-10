package org.quantumbadger.redreader.reddit.prepared.html;

import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.support.v7.app.AppCompatActivity;
import org.apache.commons.lang3.StringEscapeUtils;
import org.quantumbadger.redreader.reddit.prepared.bodytext.BodyTextElement;
import org.quantumbadger.redreader.reddit.prepared.bodytext.BodyTextElementVerticalSequence;

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

		public static final Token EOF = new Token(TokenType.EOF, "", null);

		@NonNull public final TokenType type;
		@NonNull public final String text;
		@Nullable public final String href;

		public Token(
				@NonNull final TokenType type,
				@NonNull final String text,
				@Nullable final String href) {
			this.type = type;
			this.text = text;
			this.href = href;
		}

		@NonNull
		@Override
		public String toString() {
			return type.name() + "(" + text + ")";
		}
	}

	@NonNull private final String mHtml;
	private int mPos = 0;

	public HtmlReader(@NonNull final String html) {
		mHtml = html;
	}

	private static String normaliseWhitespace(@NonNull final String html) {

		final StringBuilder result = new StringBuilder(html.length());

		boolean lastCharWasWhitespace = false;

		for(int i = 0; i < html.length(); i++) {

			final char c = html.charAt(i);

			if(c == '\n' || c == '\r') {
				// Ignore
				// TODO removes linebreaks inside <pre>!

			} else if(isWhitespace(c)) {
				if(!lastCharWasWhitespace) {
					result.append(" ");
					lastCharWasWhitespace = true;
				}

			} else {
				lastCharWasWhitespace = false;
				result.append(c);
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

		// TODO could optimise by checking existing string

		final StringBuilder result = new StringBuilder(16);

		try {
			while(isNameChar(mHtml.charAt(mPos))) {
				result.append(mHtml.charAt(mPos));
				mPos++;
			}

		} catch(final IndexOutOfBoundsException e) {
			throw new MalformedHtmlException("Reached EOF while reading name", mHtml, mPos);
		}

		if(result.length() == 0) {
			throw new MalformedHtmlException("Got zero-length name", mHtml, mPos);
		}

		return result.toString();
	}

	private String readAndUnescapeUntil(final char endChar) {

		// TODO could optimise by using substr?

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
			throw new MalformedHtmlException("Unexpected EOF", mHtml, mPos);
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

		if(mPos >= mHtml.length()) {
			// End of data
			return Token.EOF;
		}

		skipNewlines();

		if(mHtml.charAt(mPos) == '<') {

			mPos++;
			skipWhitespace();

			final TokenType type;

			try {

				if(mHtml.charAt(mPos) == '/') {
					type = TokenType.TAG_END;
					mPos++;
					skipWhitespace();

				} else {
					type = TokenType.TAG_START;
				}

				final String tagName = readName();
				@Nullable String href = null;

				skipWhitespace();

				while(mHtml.charAt(mPos) != '>') {

					if(tryAccept('/')) {
						skipWhitespace();
						accept('>');
						return new Token(TokenType.TAG_START_AND_END, tagName, href);
					}

					final String propertyName = readName();

					if(tryAccept('=')) {
						accept('"');
						final String value = readAndUnescapeUntil('"');
						accept('"');
						skipWhitespace();

						if(propertyName.equalsIgnoreCase("href")) {
							href = value;
						}
					}
				}

				accept('>');

				return new Token(type, tagName, href);

			} catch(final IndexOutOfBoundsException e) {
				throw new MalformedHtmlException("Unexpected EOF", mHtml, mPos);
				// TODO show error message per-comment, rather than failing whole comment list
			}


		} else {
			// Raw text
			return new Token(TokenType.TEXT, normaliseWhitespace(readAndUnescapeUntil('<')), null);
		}
	}

	// TODO put this elsewhere?
	public static BodyTextElement parse(
			@NonNull final String html,
			@NonNull final AppCompatActivity activity) throws MalformedHtmlException {

		final HtmlRawElement rootElement
				= HtmlRawElement.readFrom(new HtmlReaderPeekable(new HtmlReader(html)));

		final ArrayList<HtmlRawElement> reduced = new ArrayList<>();

		rootElement.reduce(new HtmlTextAttributes(), activity, reduced);

		final ArrayList<BodyTextElement> generated = new ArrayList<>();

		for(final HtmlRawElement element : reduced) {
			element.generate(activity, generated);
		}

		return new BodyTextElementVerticalSequence(generated);
	}

	@NonNull
	public String getHtml() {
		return mHtml;
	}

	public int getPos() {
		return mPos;
	}
}
