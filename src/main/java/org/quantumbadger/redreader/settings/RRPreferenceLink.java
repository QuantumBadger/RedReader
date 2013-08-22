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

import java.util.HashMap;

public class RRPreferenceLink extends RRPreference {

	private final Uri uri;

	@Override
	public Uri getUri() {
		return uri;
	}

	protected static RRPreferenceLink parse(RRPrefs preferenceManager, HashMap<String, String> attributes, ItemSource itemSource)
			throws NoSuchFieldException, IllegalAccessException {

		final Uri uri = Uri.parse(attributes.get("uri"));

		return new RRPreferenceLink(preferenceManager, attributes, itemSource, uri);
	}

	protected RRPreferenceLink(RRPrefs preferenceManager, HashMap<String, String> attributes, ItemSource itemSource, Uri uri) throws NoSuchFieldException, IllegalAccessException {
		super(preferenceManager, attributes, itemSource);
		this.uri = uri;
	}
}
