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

public class RRPreferenceHeader extends RRPreference {

	public Uri getUri() {
		return null;
	}

	private RRPreferenceHeader(RRPrefs preferenceManager, HashMap<String, String> attributes, ItemSource itemSource) throws NoSuchFieldException, IllegalAccessException {
		super(preferenceManager, attributes, itemSource);
	}

	public static RRPreferenceHeader parse(RRPrefs preferenceManager, HashMap<String, String> attributes, ItemSource itemSource) throws NoSuchFieldException, IllegalAccessException {
		return new RRPreferenceHeader(preferenceManager, attributes, itemSource);
	}

	public boolean isGreyedOut() {
		return true;
	}
}
