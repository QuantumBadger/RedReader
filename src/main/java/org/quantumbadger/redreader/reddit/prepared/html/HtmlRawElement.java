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

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;
import org.quantumbadger.redreader.common.StringUtils;
import org.quantumbadger.redreader.reddit.prepared.bodytext.BlockType;
import org.quantumbadger.redreader.reddit.prepared.bodytext.BodyElement;

import java.util.ArrayList;
import java.util.Objects;

public abstract class HtmlRawElement {

	// TODO potential improvements:
	//		- Profile performance
	//		- Test left/right swiping interaction with table scrollview

	public static class LinkButtonDetails {

		@Nullable public final String name;
		@NonNull public final String url;


		public LinkButtonDetails(
				@Nullable final String name,
				@NonNull final String url) {
			this.name = name;
			this.url = url;
		}

		@NonNull
		public final String getButtonTitle() {

			if(name == null || name.isEmpty()) {
				return url;
			} else {
				return name;
			}
		}

		@Nullable
		public final String getButtonSubtitle() {

			if(name == null || name.isEmpty()) {
				return null;
			} else {
				return url;
			}
		}
	}

	@NonNull
	public final String getPlainText() {
		final StringBuilder sb = new StringBuilder();
		getPlainText(sb);
		return sb.toString();
	}

	public abstract void getPlainText(@NonNull final StringBuilder stringBuilder);


	public abstract void reduce(
			@NonNull HtmlTextAttributes activeAttributes,
			@NonNull AppCompatActivity activity,
			@NonNull ArrayList<HtmlRawElement> destination,
			@NonNull ArrayList<LinkButtonDetails> linkButtons);

	public abstract void generate(
			@NonNull final AppCompatActivity activity,
			@NonNull ArrayList<BodyElement> destination);

	@NonNull
	public static HtmlRawElement readFrom(@NonNull final HtmlReaderPeekable reader)
			throws MalformedHtmlException {

		final HtmlReader.Token startToken = reader.peek();
		reader.advance();

		if(startToken.type == HtmlReader.TokenType.TAG_START_AND_END) {

			switch(startToken.text) {
				case "hr":
					return new HtmlRawElementTagHorizontalRule();

				case "br":
					return new HtmlRawElementBreak();

				default:
					return HtmlRawElementInlineErrorMessage.create(
							"Error: Unexpected tag <" + startToken.text + "/>");
			}

		} else if(startToken.type == HtmlReader.TokenType.TAG_START) {

			final ArrayList<HtmlRawElement> children = new ArrayList<>();

			while(reader.peek().type != HtmlReader.TokenType.TAG_END
					&& reader.peek().type != HtmlReader.TokenType.EOF) {

				children.add(HtmlRawElement.readFrom(reader));
			}

			{
				final HtmlReader.Token endToken = reader.peek();

				// Reddit sometimes doesn't close tags properly :'(
				if(endToken.text.equalsIgnoreCase(startToken.text)) {
					reader.advance();
				}
			}

			final HtmlRawElement result;

			switch(StringUtils.asciiLowercase(startToken.text)) {
				case "code":
					result = new HtmlRawElementTagCode(children);
					break;
				case "del":
					result = new HtmlRawElementTagDel(children);
					break;
				case "em":
					result = new HtmlRawElementTagEmphasis(children);
					break;
				case "div":
					result = new HtmlRawElementBlock(
							BlockType.VERTICAL_SEQUENCE,
							children);
					break;
				case "h1":
					result = new HtmlRawElementBlock(
							BlockType.HEADER,
							new HtmlRawElementTagH1(children));
					break;
				case "h2":
					result = new HtmlRawElementBlock(
							BlockType.HEADER,
							new HtmlRawElementTagH2(children));
					break;
				case "h3":
					result = new HtmlRawElementBlock(
							BlockType.HEADER,
							new HtmlRawElementTagH3(children));
					break;
				case "h4":
					result = new HtmlRawElementBlock(
							BlockType.HEADER,
							new HtmlRawElementTagH4(children));
					break;
				case "h5":
					result = new HtmlRawElementBlock(
							BlockType.HEADER,
							new HtmlRawElementTagH5(children));
					break;
				case "h6":
					result = new HtmlRawElementBlock(
							BlockType.HEADER,
							new HtmlRawElementTagH6(children));
					break;
				case "strong":
					result = new HtmlRawElementTagStrong(children);
					break;
				case "p":
					result = new HtmlRawElementBlock(BlockType.NORMAL_TEXT, children);
					break;
				case "th":
				case "td":
					result = new HtmlRawElementTableCell(new HtmlRawElementBlock(
							BlockType.TABLE_CELL,
							children));
					break;
				case "sup":
					result = new HtmlRawElementTagSuperscript(children);
					break;
				case "a": {

					final String href = Objects.requireNonNull(startToken.href);

					if(href.startsWith("/spoiler")) {
						// Old spoiler syntax
						result = new HtmlRawElementSpoiler(new HtmlRawElementBlock(
								BlockType.BUTTON,
								children));

					} else if(href.length() == 2
							&& (href.charAt(0) == '#' || href.charAt(0) == '/')
							&& startToken.title != null) {

						// Another old spoiler syntax

						children.add(new HtmlRawElementSpoiler(
								new HtmlRawElementBlock(
										BlockType.NORMAL_TEXT,
										new HtmlRawElementPlainText(startToken.title))));

						result = new HtmlRawElementTagPassthrough(children);

					} else if(href.startsWith("#")) {
						// Probably an emote: pass through the text, but don't make a link
						result = new HtmlRawElementTagPassthrough(children);

					} else {
						result = new HtmlRawElementTagAnchor(children, href);
					}
					break;
				}
				case "pre":
					result = new HtmlRawElementBlock(
							BlockType.CODE_BLOCK,
							new HtmlRawElementTagCode(children));
					break;
				case "ul":
					result = new HtmlRawElementBulletList(children);
					break;
				case "ol":
					result = new HtmlRawElementNumberedList(children);
					break;
				case "li":
					result = new HtmlRawElementBlock(BlockType.LIST_ELEMENT, children);
					break;
				case "blockquote":
					result = new HtmlRawElementQuote(new HtmlRawElementBlock(
							BlockType.QUOTE,
							children));
					break;
				case "span":

					if("md-spoiler-text".equalsIgnoreCase(startToken.cssClass)) {
						result = new HtmlRawElementSpoiler(new HtmlRawElementBlock(
								BlockType.BUTTON,
								children));

					} else {
						result = new HtmlRawElementTagPassthrough(children);
					}
					break;

				case "thead":
					result = new HtmlRawElementTagStrong(children);
					break;

				case "tbody":
					result = new HtmlRawElementTagPassthrough(children);
					break;

				case "table":
					result = new HtmlRawElementTable(children);
					break;

				case "tr":
					result = new HtmlRawElementTableRow(children);
					break;

				case "img":
					result = new HtmlRawElementPlainText("Image");
					break;

				default:
					return HtmlRawElementInlineErrorMessage.appendError(
							"Error: Unexpected tag start <" + startToken.text + ">",
							new HtmlRawElementBlock(BlockType.NORMAL_TEXT, children));
			}

			return result;

		} else if(startToken.type == HtmlReader.TokenType.TEXT) {
			return new HtmlRawElementPlainText(startToken.text);

		} else if(startToken.type == HtmlReader.TokenType.EOF) {
			throw new MalformedHtmlException(
					"Unexpected EOF",
					reader.getHtml(),
					reader.getPos());

		} else {
			return HtmlRawElementInlineErrorMessage.create(
					"Error: Unexpected token type " + startToken.type);
		}
	}
}
