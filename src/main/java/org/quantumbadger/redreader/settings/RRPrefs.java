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

import android.content.Context;
import android.net.Uri;
import android.util.Log;
import org.quantumbadger.redreader.common.Constants;
import org.quantumbadger.redreader.common.collections.UniqueSynchronizedQueue;
import org.xmlpull.v1.XmlPullParser;
import org.xmlpull.v1.XmlPullParserException;
import org.xmlpull.v1.XmlPullParserFactory;

import java.io.IOException;
import java.lang.reflect.Field;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;

public final class RRPrefs {

	private static RRPrefs prefs;

	private final SQLiteHashMap prefMap;

	private final HashMap<String, LinkedList<RRPreference>> prefPages = new HashMap<String, LinkedList<RRPreference>>();

	public RRPreferenceBoolean pref_test_bool1;
	public RRPreferenceFloat pref_test_float1;
	public RRPreferenceEnum<RRPreferenceEnum.TestEnum> pref_test_enum1;

	public static synchronized RRPrefs getPrefs(Context context) {

		if(prefs != null) return prefs;

		try {
			prefs = new RRPrefs(context);
		} catch(Exception e) {
			throw new RuntimeException(e);
		}

		return prefs;
	}

	private RRPrefs(Context context) throws IOException, XmlPullParserException, XmlParserWrapper.RRParseException,
			IllegalAccessException, NoSuchFieldException {

		prefMap = new SQLiteHashMap(context, "prefs.db");

		final UniqueSynchronizedQueue<String> remainingFiles = new UniqueSynchronizedQueue<String>();
		remainingFiles.enqueue(getPrefsFileFromUri(Constants.Internal.getUri(Constants.Internal.URI_HOST_PREFSPAGE)));

		final HashSet<String> handledFiles = new HashSet<String>();

		while(!remainingFiles.isEmpty()) {

			final String next = remainingFiles.dequeue();

			if(!handledFiles.contains(next)) {
				handledFiles.add(next);
				remainingFiles.enqueue(buildFromFile(context, next));
			}
		}
	}

	private HashSet<String> buildFromFile(Context context, String filename) throws XmlPullParserException,
			IOException, XmlParserWrapper.RRParseException, IllegalAccessException, NoSuchFieldException {

		final XmlPullParser xmlResourceParser = XmlPullParserFactory.newInstance().newPullParser();
		xmlResourceParser.setInput(context.getAssets().open(filename), "UTF-8");
		final XmlParserWrapper parser = new XmlParserWrapper(xmlResourceParser);

		final HashSet<String> linkedFiles = new HashSet<String>();
		final LinkedList<RRPreference> prefPage = new LinkedList<RRPreference>();

		if(parser.next() != XmlPullParser.START_TAG) throw new RuntimeException("Expected: start tag");
		if(parser.getName().equals("PrefsPage")) throw new RuntimeException("Expected: PrefsPage tag");

		while(parser.next() != XmlPullParser.END_TAG) {

			final RRPreference preference = RRPreference.parse(this, parser);
			Log.i("RRXML", "id = " + (preference.id == null ? "null" : preference.id));
			Log.i("RRXML", "title = " + context.getString(preference.titleString));
			Log.i("RRXML", "item count = " + preference.getItems().length);
			for(int i = 0; i < preference.getItems().length; i++) {
				Log.i("RRXML", "item " + i + " = " + preference.getItems()[i].getName(context));
			}
			if(preference instanceof RRPreferenceEnum) Log.i("RRXML", "set to = " + ((RRPreferenceEnum) preference).get().name());

			prefPage.add(preference);

			if(preference instanceof RRPreferenceLink) {

				final String linkedFilename = getPrefsFileFromUri(((RRPreferenceLink)preference).getUri());
				if(linkedFilename != null) linkedFiles.add(linkedFilename);

			} else if(preference.id != null) {
				final Field prefField = getClass().getField(preference.id);

				if(prefField != null) {
					prefField.set(this, preference);
				}
			}
		}

		prefPages.put(filename, prefPage);

		return linkedFiles;
	}

	public List<RRPreference> getPrefPage(final Uri uri) {

		final String filename = getPrefsFileFromUri(uri);
		if(filename == null) return null;

		return prefPages.get(filename);
	}

	private String getPrefsFileFromUri(Uri uri) {

		if(!uri.getScheme().equals(Constants.Internal.URI_SCHEME)
				|| !uri.getAuthority().equals(Constants.Internal.URI_HOST_PREFSPAGE)) {
			return null;
		}

		final List<String> segments = uri.getPathSegments();

		final StringBuilder filename = new StringBuilder("prefs/prefs");

		for(String segment : segments) {
			filename.append('.');
			filename.append(segment);
		}

		filename.append(".xml");
		return filename.toString();
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

	public RRPreference getPref(Uri uri) {

		if(!uri.getScheme().equals(Constants.Internal.URI_SCHEME)
				|| !uri.getAuthority().equals(Constants.Internal.URI_HOST_PREF)) {
			return null;
		}

		try {
			return getPreferenceByName(uri.getPath().replaceAll("^/|/$", ""));
		} catch(Exception e) {
			throw new RuntimeException(e);
		}
	}
}
