package org.quantumbadger.redreader.reddit.prepared.html;

import android.support.annotation.NonNull;

import java.util.ArrayList;

public abstract class HtmlRawElement {
	// a, p, table/td/tr/thead/tbody/th, hr, strong, em, "code", "headings", "underline", "strikethrough", "quote", ul/li etc
	// link buttons
	// spoilers

	@NonNull
	public static HtmlRawElement readFrom(@NonNull final HtmlReaderPeekable reader)
			throws MalformedHtmlException {

		final HtmlReader.Token startToken = reader.peek();

		if(startToken.type == HtmlReader.TokenType.TAG_START_AND_END) {

			if(startToken.text.equals("hr")) {
				// TODO Horizontal rule
				return new HtmlRawElementInlineErrorMessage("Error: Horizontal rule currently unsupported");

			} else {
				return new HtmlRawElementInlineErrorMessage("Error: Unexpected tag <" + startToken.text + "/>");
			}

		} else if(startToken.type == HtmlReader.TokenType.TAG_START) {

			final ArrayList<HtmlRawElement> children = new ArrayList<>();

			while(reader.advance().type != HtmlReader.TokenType.TAG_END) {
				children.add(HtmlRawElement.readFrom(reader));
			}

			final HtmlReader.Token endToken = reader.peek();
			reader.advance();

			final HtmlRawElement result;

			switch(startToken.text.toLowerCase()) {
				// TODO <a>, <span> (spoiler), <pre>, <table>/etc, <ul>/etc
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
					result = new HtmlRawElementTagPassthrough(children);
					break;
				case "h1":
					result = new HtmlRawElementTagH1(children);
					break;
				case "h2":
					result = new HtmlRawElementTagH2(children);
					break;
				case "h3":
					result = new HtmlRawElementTagH3(children);
					break;
				case "h4":
					result = new HtmlRawElementTagH4(children);
					break;
				case "strong":
					result = new HtmlRawElementTagStrong(children);
					break;
				case "p":
					result = new HtmlRawElementTagPassthrough(children);
					break;

				default:
					return new HtmlRawElementInlineErrorMessage(
							"Error: Unexpected tag start <" + startToken.text + ">");
			}

			if(!endToken.text.equalsIgnoreCase(startToken.text)) {
				return HtmlRawElementInlineErrorMessage.prependError(
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
			return new HtmlRawElementInlineErrorMessage(
					"Error: Unexpected token type " + startToken.type);
		}
	}
}
