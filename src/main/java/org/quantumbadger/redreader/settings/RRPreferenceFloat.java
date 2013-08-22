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

import org.xmlpull.v1.XmlPullParserException;

import java.io.IOException;
import java.util.HashMap;

public class RRPreferenceFloat extends RRPreference {

	private volatile String value;
	private volatile float floatValue;

	protected static RRPreferenceFloat parse(RRPrefs preferenceManager, HashMap<String, String> attributes, ItemSource itemSource) throws NoSuchFieldException, IllegalAccessException, IOException, XmlPullParserException {

		final String id = attributes.get("id");

		final String defaultValue = attributes.get("default");
		final String userValue = preferenceManager.getRawUserPreference(id);

		final String value = userValue != null ? userValue : defaultValue;

		return new RRPreferenceFloat(preferenceManager, attributes, itemSource, value);
	}

	private RRPreferenceFloat(RRPrefs preferenceManager, HashMap<String, String> attributes, ItemSource itemSource, String value) throws NoSuchFieldException, IllegalAccessException {
		super(preferenceManager, attributes, itemSource);
		this.value = value;
		this.floatValue = Float.parseFloat(value);
	}

	public String getRaw() {
		return value;
	}

	public float get() {
		return floatValue;
	}

	public synchronized void set(String value) {
		this.value = value;
		this.floatValue = Float.parseFloat(value);
		setRawUserPreference(String.valueOf(value));
	}
}
