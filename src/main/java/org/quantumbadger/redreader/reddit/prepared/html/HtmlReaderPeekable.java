package org.quantumbadger.redreader.reddit.prepared.html;

import androidx.annotation.NonNull;

public class HtmlReaderPeekable {

	@NonNull private final HtmlReader mHtmlReader;

	@NonNull private HtmlReader.Token mNext;

	public HtmlReaderPeekable(@NonNull final HtmlReader htmlReader) throws MalformedHtmlException {
		mHtmlReader = htmlReader;
		mNext = mHtmlReader.readNext();
	}

	public HtmlReader.Token peek() {
		return mNext;
	}

	public HtmlReader.Token advance() throws MalformedHtmlException {
		mNext = mHtmlReader.readNext();
		return mNext;
	}

	@NonNull
	public String getHtml() {
		return mHtmlReader.getHtml();
	}

	public int getPos() {
		return mHtmlReader.getPos();
	}
}
