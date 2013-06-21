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
import android.text.SpannableStringBuilder;
import android.view.View;
import android.view.ViewGroup;
import com.laurencedawson.activetextview.ActiveTextView;
import org.holoeverywhere.app.AlertDialog;
import org.holoeverywhere.widget.Button;
import org.holoeverywhere.widget.LinearLayout;
import org.quantumbadger.redreader.common.BetterSSB;
import org.quantumbadger.redreader.common.General;

import java.util.LinkedList;

// This parser would be less horrible if it could simply reject invalid markdown. Instead, it does
// its best to output something reasonable.
public final class RedditCommentTextParser {

	public static interface ViewGenerator {
		public ViewGroup generate(Context context, float textSize, Integer textCol, ActiveTextView.OnLinkClickedListener listener, Object attachment);
	}

	private static enum TokenType {
		TEXT, ASTERISK, DOUBLE_ASTERISK, STRIKETHROUGH, LINK_TEXT_OPEN, LINK_TEXT_CLOSE, LINK_URL_OPEN, NEWLINE, LINK_URL_CLOSE
	}

	private static final class Token {
		private final TokenType type;
		private final String data;

		private Token(final TokenType type, final String data) {
			this.type = type;
			this.data = data;
		}
	}

	private static final class ParserState {
		public int flags = 0;
		public boolean afterNewLine = true;
	}

	private static final class Tokenizer {

		private final char[] input;
		private int pos = 0;

		private Tokenizer(final char[] input) {
			this.input = input;
		}

		public Token read() {

			if(pos >= input.length) return null;

			final char c = input[pos++];

			// TODO handle code
			// TODO handle quotes
			// TODO handle headers
			// TODO handle tables

			switch(c) {

				case '\\':
					if(input.length > pos) {
						return new Token(TokenType.TEXT, new String(new char[] {input[pos++]}));
					} else {
						// Invalid markdown.
						return new Token(TokenType.TEXT, "\\");
					}

				case '*':
					if(input.length > pos && input[pos] == '*') {
						pos++;
						return new Token(TokenType.DOUBLE_ASTERISK, "**");

					} else {
						return new Token(TokenType.ASTERISK, "*");
					}

				case '~':
					if(input.length > pos && input[pos] == '~') {
						pos++;
						return new Token(TokenType.STRIKETHROUGH, "~");

					} else {
						return new Token(TokenType.TEXT, "~");
					}

				case '[': return new Token(TokenType.LINK_TEXT_OPEN, "[");
				case ']': return new Token(TokenType.LINK_TEXT_CLOSE, "]");
				case '(': return new Token(TokenType.LINK_URL_OPEN, "(");
				case ')': return new Token(TokenType.LINK_URL_CLOSE, ")");

				case '\n':
					return new Token(TokenType.NEWLINE, "\n");


				default:

					final StringBuilder sb = new StringBuilder(128);
					sb.append(c);

					while(input.length > pos
							&& input[pos] != '*'
							&& input[pos] != '~'
							&& input[pos] != '['
							&& input[pos] != ']'
							&& input[pos] != '('
							&& input[pos] != ')'
							&& input[pos] != '\\'
							&& input[pos] != '\n') {

						sb.append(input[pos++]);
					}

					return new Token(TokenType.TEXT, sb.toString());
			}
		}
	}

	public static ViewGenerator parse(final String markdown) {

		final char[] input = markdown.toCharArray();
		final LinkedList<Object> output = new LinkedList<Object>();

		try {

			final Tokenizer tokenizer = new Tokenizer(input);
			Token t;

			BetterSSB ssb = new BetterSSB();

			final ParserState state = new ParserState();

			while((t = tokenizer.read()) != null) {

				switch(t.type) {

					case DOUBLE_ASTERISK: state.flags ^= BetterSSB.BOLD; break;
					case STRIKETHROUGH: state.flags ^= BetterSSB.STRIKETHROUGH; break;

					case ASTERISK:
						if(state.afterNewLine) {
							ssb.append(" • ", 0);

						} else {
							state.flags ^= BetterSSB.ITALIC;
						}

						break;

					case LINK_TEXT_OPEN:

						final LinkedList<Token> linkText = new LinkedList<Token>();

						while((t = tokenizer.read()) != null && t.type != TokenType.LINK_TEXT_CLOSE) {
							linkText.add(t);
						}

						if(t == null) {

							// Invalid markdown. Just dump what we can.
							linkText.add(t);
							dumpAfterInvalid(state, linkText, ssb);

						} else {

							final Token t2 = tokenizer.read();
							Token t3 = null;

							if(t2 != null
									&& (t2.type == TokenType.LINK_URL_OPEN
									|| (t2.type == TokenType.TEXT
									&& (t3 = tokenizer.read()) != null
									&& t3.type == TokenType.LINK_URL_OPEN))) {

								final Token linkUrlToken = tokenizer.read();

								if(linkUrlToken == null || linkUrlToken.type != TokenType.TEXT) {

									// Invalid markdown. Just dump what we can.

									linkText.add(t);
									if(t2 != null) linkText.add(t2);
									if(t3 != null) linkText.add(t3);

									dumpAfterInvalid(state, linkText, ssb);

								} else {

									// Valid markdown. Output the link text.

									// First, check for spoilers

									final String linkUrlStr = linkUrlToken.data;

									if(linkUrlStr.equalsIgnoreCase("/spoiler")) {

										ssb.linkify();
										output.add(ssb.get());
										ssb = new BetterSSB();

										final StringBuilder sb = new StringBuilder();
										for(Token spoilerToken : linkText) sb.append(spoilerToken.data);

										output.add(new Spoiler("Spoiler", sb.toString()));

										tokenizer.read(); // Last bracket

									} else if(linkUrlStr.startsWith("/s ") || linkUrlStr.startsWith("/b ")
											|| linkUrlStr.startsWith("#s ") || linkUrlStr.startsWith("#b ")) {

										ssb.linkify();
										output.add(ssb.get());
										ssb = new BetterSSB();

										final StringBuilder sbText = new StringBuilder();
										for(Token spoilerToken : linkText) sbText.append(spoilerToken.data);

										Token tSpoiler;
										final StringBuilder sbUrl = new StringBuilder(linkUrlStr);

										while((tSpoiler = tokenizer.read()) != null && (tSpoiler.type != TokenType.LINK_URL_CLOSE || sbUrl.charAt(sbUrl.length() - 1) != '"')) {
											sbUrl.append(tSpoiler.data);
										}

										if(linkUrlStr.charAt(1) == 'b') {
											output.add(new Spoiler("Book spoiler: " + sbText.toString(), sbUrl.toString().substring(3)));
										} else {
											output.add(new Spoiler("Spoiler: " + sbText.toString(), sbUrl.toString().substring(3)));
										}

									} else {

										for(final Token t4 : linkText) {
											switch(t4.type) {

												case DOUBLE_ASTERISK: state.flags ^= BetterSSB.BOLD; break;
												case ASTERISK: state.flags ^= BetterSSB.ITALIC; break;
												case STRIKETHROUGH: state.flags ^= BetterSSB.STRIKETHROUGH; break;

												default: {
													ssb.append(t4.data, state.flags | BetterSSB.UNDERLINE, 0, 0, 1.0f, linkUrlToken.data);
													break;
												}
											}
										}

										tokenizer.read(); // Last bracket
									}
								}

							} else {

								// Invalid markdown. Just dump what we can.

								ssb.append("[", state.flags);
								linkText.add(t);
								if(t2 != null) linkText.add(t2);
								if(t3 != null) linkText.add(t3);

								dumpAfterInvalid(state, linkText, ssb);
							}
						}

						break;

					case NEWLINE:
						// TODO handle this properly
						// One newline should do nothing, unless in a bullet list.
						// Two or more should cause a line break.
						if(!state.afterNewLine) {
							ssb.linkify();
							output.add(ssb.get());
							ssb = new BetterSSB();
						}
						break;

					default: {
						ssb.append(t.data, state.flags);
					}
				}

				state.afterNewLine = t.type == TokenType.NEWLINE;
			}

			ssb.linkify();
			output.add(ssb.get());

		} catch(Throwable t) {
			// Invalid markdown
			t.printStackTrace();
		}

		return new ViewGenerator() {

			public ViewGroup generate(final Context context, final float textSize, final Integer textCol,
									  final ActiveTextView.OnLinkClickedListener listener, final Object attachment) {

				final LinearLayout ll = new LinearLayout(context);
				ll.setOrientation(LinearLayout.VERTICAL);

				boolean notFirst = false;

				for(final Object paragraph : output) {

					if(paragraph instanceof SpannableStringBuilder) {

						final ActiveTextView tv = new ActiveTextView(context);
						tv.setAttachment(attachment);
						if(notFirst) tv.setPadding(0, General.dpToPixels(context, 8), 0, 0);
						tv.setText((SpannableStringBuilder)paragraph);
						tv.setTextSize(textSize);
						if(textCol != null) tv.setTextColor(textCol);
						tv.setLinkClickedListener(listener);
						ll.addView(tv);
						notFirst = true;

					} else if(paragraph instanceof Spoiler) {

						final Spoiler spoiler = (Spoiler)paragraph;

						final Button spoilerButton = new Button(context);
						spoilerButton.setText(spoiler.title);

						spoilerButton.setOnClickListener(new View.OnClickListener() {
							public void onClick(View v) {
								AlertDialog.Builder builder = new AlertDialog.Builder(context);
								builder.setTitle(spoiler.title);
								builder.setMessage(spoiler.message);
								AlertDialog alert = builder.create();
								alert.show();
							}
						});

						ll.addView(spoilerButton);
						notFirst = true;
					}
				}

				return ll;
			}
		};
	}

	private static void dumpAfterInvalid(ParserState state, LinkedList<Token> toDump, BetterSSB ssb) {

		for(final Token t : toDump) {
			switch(t.type) {

				case DOUBLE_ASTERISK: state.flags ^= BetterSSB.BOLD; break;
				case ASTERISK: state.flags ^= BetterSSB.ITALIC; break;
				case STRIKETHROUGH: state.flags ^= BetterSSB.STRIKETHROUGH; break;

				default: {
					ssb.append(t.data, state.flags);
					break;
				}
			}
		}

	}

	private final static class Spoiler {
		public final String title, message;

		private Spoiler(String title, String message) {
			this.title = title;
			this.message = message;
		}
	}
}
