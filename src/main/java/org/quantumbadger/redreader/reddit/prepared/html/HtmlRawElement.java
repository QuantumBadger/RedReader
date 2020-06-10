package org.quantumbadger.redreader.reddit.prepared.html;

import android.support.annotation.NonNull;
import android.support.v7.app.AppCompatActivity;
import org.quantumbadger.redreader.reddit.prepared.bodytext.BlockType;
import org.quantumbadger.redreader.reddit.prepared.bodytext.BodyElement;

import java.util.ArrayList;
import java.util.Objects;

public abstract class HtmlRawElement {
	// TODO table/td/tr/thead/tbody/th, hr, "quote", <ol> for numbered list
	// link buttons
	// spoilers

	public abstract void reduce(
			@NonNull HtmlTextAttributes activeAttributes,
			@NonNull AppCompatActivity activity,
			@NonNull ArrayList<HtmlRawElement> destination);

	public abstract void generate(
			@NonNull AppCompatActivity activity,
			@NonNull ArrayList<BodyElement> destination);

	@NonNull
	public static HtmlRawElement readFrom(@NonNull final HtmlReaderPeekable reader)
			throws MalformedHtmlException {

		final HtmlReader.Token startToken = reader.peek();
		reader.advance();

		if(startToken.type == HtmlReader.TokenType.TAG_START_AND_END) {

			if(startToken.text.equals("hr")) {
				return new HtmlRawElementTagHorizontalRule();

			} else {
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
				// TODO <span> (spoiler), <table>/etc, <ol> for numbered list, link buttons, spacing, blockquote
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
				case "sup":
					result = new HtmlRawElementTagSuperscript(children);
					break;
				case "a":
					result = new HtmlRawElementTagAnchor(children, Objects.requireNonNull(startToken.href));
					break;
				case "pre":
					result = new HtmlRawElementBlock(BlockType.CODE_BLOCK, new HtmlRawElementTagCode(children));
					break;
				case "ul":
					result = new HtmlRawElementTagPassthrough(children);
					break;
				case "li":
					result = new HtmlRawElementBullet(new HtmlRawElementBlock(BlockType.BULLET, children));
					break;

				default:
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
