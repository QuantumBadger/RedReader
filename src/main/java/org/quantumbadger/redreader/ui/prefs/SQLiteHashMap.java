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

import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteOpenHelper;

import java.util.HashMap;

public final class SQLiteHashMap {

	private final Context context;
	private final String filename;
	private final DbManager dbManager;
	private final HashMap<String, String> contents;

	public SQLiteHashMap(Context context, String filename) {
		this.context = context;
		this.filename = filename;
		dbManager = new DbManager();
		contents = dbManager.readAll();
	}

	public synchronized String get(String key) {
		return contents.get(key);
	}

	public synchronized void set(final String key, final String value) {
		contents.put(key, value);

		// TODO use pool
		// TODO low priority
		new Thread() {
			@Override
			public void run() {
				dbManager.set(key, value);
			}
		}.start();
	}

	private final class DbManager extends SQLiteOpenHelper {

		private static final String TABLE = "keyval",
				FIELD_KEY = "key",
				FIELD_VALUE = "val",
				FIELD_LASTUPDATE = "lastUpdate";

		private static final int DB_VERSION = 1;

		public DbManager() {
			super(context, filename, null, DB_VERSION);
		}

		@Override
		public void onCreate(final SQLiteDatabase db) {

			final String queryString = String.format(
					"CREATE TABLE %s (" +
							"%s TEXT PRIMARY KEY ON CONFLICT REPLACE," +
							"%s TEXT," +
							"%s INTEGER)",
					TABLE,
					FIELD_KEY,
					FIELD_VALUE,
					FIELD_LASTUPDATE);

			db.execSQL(queryString);
		}

		@Override
		public void onUpgrade(final SQLiteDatabase db, final int oldVersion, final int newVersion) {
			throw new RuntimeException("Attempt to upgrade on first version of the database!");
		}

		private synchronized HashMap<String, String> readAll() {

			final HashMap<String, String> result = new HashMap<String, String>();

			final String[] fields = {FIELD_KEY, FIELD_VALUE};

			final SQLiteDatabase db = getReadableDatabase();

			final Cursor cursor = db.query(TABLE, fields, null, null, null, null, null);

			while(cursor.moveToNext()) {
				result.put(cursor.getString(0), cursor.getString(1));
			}

			cursor.close();
			db.close();

			return result;
		}

		private synchronized void set(final String key, final String value){

			final SQLiteDatabase db = this.getWritableDatabase();

			final ContentValues row = new ContentValues();

			row.put(FIELD_KEY, key);
			row.put(FIELD_VALUE, value);
			row.put(FIELD_LASTUPDATE, System.currentTimeMillis());

			db.insert(TABLE, null, row);
		}
	}

}
