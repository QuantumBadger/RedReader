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

public class HtmlReaderPeekable {

	@NonNull private final HtmlReader mHtmlReader;

	@NonNull private HtmlReader.Token mNext;

	public HtmlReaderPeekable(@NonNull final HtmlReader htmlReader) throws
			MalformedHtmlException {
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
