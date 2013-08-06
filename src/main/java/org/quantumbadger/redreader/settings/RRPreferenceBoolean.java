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

import android.net.Uri;
import org.xmlpull.v1.XmlPullParserException;

import java.io.IOException;
import java.util.HashMap;

public class RRPreferenceBoolean extends RRPreference {

	private boolean value;

	public Uri getUri() {
		return null;
	}

	protected static RRPreferenceBoolean parse(RRPrefs preferenceManager, HashMap<String, String> attributes, ItemSource itemSource) throws NoSuchFieldException, IllegalAccessException, IOException, XmlPullParserException {

		final String id = attributes.get("id");

		final String defaultValue = attributes.get("default");
		final String userValue = preferenceManager.getRawUserPreference(id);

		final boolean value = (userValue != null ? userValue : defaultValue).toLowerCase().equals("true");

		return new RRPreferenceBoolean(preferenceManager, attributes, itemSource, value);
	}

	private RRPreferenceBoolean(RRPrefs preferenceManager, HashMap<String, String> attributes, ItemSource itemSource, boolean value) throws NoSuchFieldException, IllegalAccessException {
		super(preferenceManager, attributes, itemSource);
		this.value = value;
	}

	public boolean get() {
		return value;
	}

	public void set(boolean value) {
		this.value = value;
		setRawUserPreference(value ? "true" : "false");
	}
}
