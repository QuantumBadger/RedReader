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

package org.quantumbadger.redreader.settings;

import org.xmlpull.v1.XmlPullParser;
import org.xmlpull.v1.XmlPullParserException;

import java.io.IOException;
import java.util.HashMap;

public final class XmlParserWrapper {

	private final XmlPullParser parser;

	public XmlParserWrapper(XmlPullParser parser) {
		this.parser = parser;
	}

	public HashMap<String, String> getAttributeMap() {

		final int count = parser.getAttributeCount();
		final HashMap<String, String> result = new HashMap<String, String>();

		for(int i = 0; i < count; i++) {
			result.put(parser.getAttributeName(i), parser.getAttributeValue(i));
		}

		return result;
	}

	public RRParseException newException(Throwable t) {
		return new RRParseException(t);
	}

	public String getName() {
		return parser.getName();
	}

	public int next() throws IOException, XmlPullParserException {

		int next = XmlPullParser.TEXT;
		while(next == XmlPullParser.TEXT) next = parser.next();
		//StackTraceElement[] trace = Thread.currentThread().getStackTrace();
		//StackTraceElement parent = trace[3];
		//Log.i("RRXML", "Read " + next + " at " + parent.getFileName() + ", line " + parent.getLineNumber());
		return next;
	}

	public int getEventType() throws XmlPullParserException {
		return parser.getEventType();
	}

	public class RRParseException extends Exception {

		public RRParseException(Throwable t) {
			super("Exception while parsing, line " + parser.getLineNumber() + ", col " + parser.getColumnNumber() + ".", t);
		}
	}
}
