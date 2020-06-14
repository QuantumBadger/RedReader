package org.quantumbadger.redreader.reddit.prepared.html;

import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.support.v7.app.AppCompatActivity;
import org.quantumbadger.redreader.reddit.prepared.bodytext.BlockType;
import org.quantumbadger.redreader.reddit.prepared.bodytext.BodyElement;

import java.util.ArrayList;
import java.util.Objects;

public abstract class HtmlRawElement {
	// TODO use for inbox messages and self text
	// TODO maybe remove the old markdown classes (and comment preview?!)
	// TODO remove HTML source from comment view
	// TODO add HTML source to properties popup?
	// TODO handle MalformedHtmlExceptions on a per-comment basis
	// TODO search for TODOs

	// TODO link buttons

	// TODO profile compared to markdown parser?

	public abstract void reduce(
			@NonNull HtmlTextAttributes activeAttributes,
			@NonNull AppCompatActivity activity,
			@NonNull ArrayList<HtmlRawElement> destination);

	public abstract void generate(
			@NonNull AppCompatActivity activity,
			@Nullable Integer textColor,
			@Nullable Float textSize,
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
					return HtmlRawElementInlineErrorMessage.create("Error: Unexpected tag <" + startToken.text + "/>");
			}

		} else if(startToken.type == HtmlReader.TokenType.TAG_START) {

			final ArrayList<HtmlRawElement> children = new ArrayList<>();

			while(reader.peek().type != HtmlReader.TokenType.TAG_END) {
				children.add(HtmlRawElement.readFrom(reader));
			}

			final HtmlReader.Token endToken = reader.peek();
			reader.advance();

			final HtmlRawElement result;

			switch(startToken.text.toLowerCase()) {
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
					result = new HtmlRawElementBlock(BlockType.VERTICAL_SEQUENCE, children);
					break;
				case "h1":
					result = new HtmlRawElementBlock(BlockType.HEADER, new HtmlRawElementTagH1(children));
					break;
				case "h2":
					result = new HtmlRawElementBlock(BlockType.HEADER, new HtmlRawElementTagH2(children));
					break;
				case "h3":
					result = new HtmlRawElementBlock(BlockType.HEADER, new HtmlRawElementTagH3(children));
					break;
				case "h4":
					result = new HtmlRawElementBlock(BlockType.HEADER, new HtmlRawElementTagH4(children));
					break;
				case "strong":
					result = new HtmlRawElementTagStrong(children);
					break;
				case "p":
					result = new HtmlRawElementBlock(BlockType.NORMAL_TEXT, children);
					break;
				case "th":
				case "td":
					result = new HtmlRawElementTableCell(new HtmlRawElementBlock(BlockType.TABLE_CELL, children));
					break;
				case "sup":
					result = new HtmlRawElementTagSuperscript(children);
					break;
				case "a": {

					final String href = Objects.requireNonNull(startToken.href);

					if(href.startsWith("/spoiler")) {
						// Old spoiler syntax
						result = new HtmlRawElementSpoiler(new HtmlRawElementBlock(BlockType.BUTTON, children));

					} else if(href.length() == 2
							&& (href.charAt(0) == '#' || href.charAt(0) == '/')
							&& startToken.title != null) {

						// Another old spoiler syntax

						children.add(new HtmlRawElementSpoiler(
								new HtmlRawElementBlock(
										BlockType.NORMAL_TEXT,
										new HtmlRawElementPlainText(startToken.title))));

						result = new HtmlRawElementTagPassthrough(children);

					} else {
						result = new HtmlRawElementTagAnchor(
								children,
								href);
					}
					break;
				}
				case "pre":
					result = new HtmlRawElementBlock(BlockType.CODE_BLOCK, new HtmlRawElementTagCode(children));
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
					result = new HtmlRawElementQuote(new HtmlRawElementBlock(BlockType.QUOTE, children));
					break;
				case "span":

					if("md-spoiler-text".equalsIgnoreCase(startToken.cssClass)) {
						result = new HtmlRawElementSpoiler(new HtmlRawElementBlock(BlockType.BUTTON, children));

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

				default:
					// TODO ignore this and just pass through
					return HtmlRawElementInlineErrorMessage.create(
							"Error: Unexpected tag start <" + startToken.text + ">");
			}

			if(!endToken.text.equalsIgnoreCase(startToken.text)) {
				return HtmlRawElementInlineErrorMessage.appendError(
						"Error: Mismatched end tag (start <"
								+ startToken.text
								+ ">, end </"
								+ endToken.text
								+ ">)",
						result);
			}

			return result;

		} else if(startToken.type == HtmlReader.TokenType.TEXT) {
			return new HtmlRawElementPlainText(startToken.text);

		} else if(startToken.type == HtmlReader.TokenType.EOF) {
			throw new MalformedHtmlException("Unexpected EOF", reader.getHtml(), reader.getPos());

		} else {
			return HtmlRawElementInlineErrorMessage.create(
					"Error: Unexpected token type " + startToken.type);
		}
	}
}
