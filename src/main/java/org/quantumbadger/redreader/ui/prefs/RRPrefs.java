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

package org.quantumbadger.redreader.ui.prefs;

import android.content.Context;
import android.util.Log;
import org.xmlpull.v1.XmlPullParser;
import org.xmlpull.v1.XmlPullParserException;
import org.xmlpull.v1.XmlPullParserFactory;

import java.io.IOException;

public final class RRPrefs {

	private static RRPrefs prefs;

	private final SQLiteHashMap prefMap;

	public RRPreferenceBoolean pref_test_bool1;
	public RRPreferenceFloat pref_test_float1;
	public RRPreferenceEnum<RRPreferenceEnum.TestEnum> pref_test_enum1;

	public static synchronized RRPrefs getPrefs(Context context) throws IOException, XmlPullParserException,
			XmlParserWrapper.RRParseException, IllegalAccessException, NoSuchFieldException {

		if(prefs != null) return prefs;

		prefs = new RRPrefs(context);
		return prefs;
	}

	private RRPrefs(Context context) throws IOException, XmlPullParserException, XmlParserWrapper.RRParseException,
			IllegalAccessException, NoSuchFieldException {

		prefMap = new SQLiteHashMap(context, "prefs.db");

		final XmlPullParser xmlResourceParser = XmlPullParserFactory.newInstance().newPullParser();
		xmlResourceParser.setInput(context.getAssets().open("prefs/prefs.xml"), "UTF-8");
		final XmlParserWrapper parser = new XmlParserWrapper(xmlResourceParser);

		if(parser.next() != XmlPullParser.START_TAG) throw new RuntimeException("Expected: start tag");
		if(parser.getName().equals("PrefsPage")) throw new RuntimeException("Expected: PrefsPage tag");

		int type;
		while((type = parser.next()) != XmlPullParser.END_TAG) {

			final RRPreference preference = RRPreference.parse(this, parser);
			Log.i("RRXML", "id = " + preference.id);
			Log.i("RRXML", "title = " + context.getString(preference.titleString));
			Log.i("RRXML", "item count = " + preference.getItems().length);
			for(int i = 0; i < preference.getItems().length; i++) {
				Log.i("RRXML", "item " + i + " = " + preference.getItems()[i].getName(context));
			}
			if(preference instanceof RRPreferenceEnum) Log.i("RRXML", "set to = " + ((RRPreferenceEnum) preference).get().name());

			addPreference(preference);
		}




		// TODO
	}

	public String getRawUserPreference(String id) {
		return prefMap.get(id);
	}

	public void setRawUserPreference(String id, String value) {
		prefMap.set(id, value);
	}

	public RRPreference getPreferenceByName(String name) throws NoSuchFieldException, IllegalAccessException {
		return (RRPreference) getClass().getField(name).get(this);
	}

	private void addPreference(RRPreference preference) throws NoSuchFieldException, IllegalAccessException {
		getClass().getField(preference.id).set(this, preference);
	}
}
