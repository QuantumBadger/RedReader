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

package org.quantumbadger.redreader.cache;

import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteOpenHelper;
import android.util.Log;
import androidx.annotation.NonNull;
import org.quantumbadger.redreader.common.Optional;
import org.quantumbadger.redreader.common.RRTime;

import java.io.IOException;
import java.net.URI;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.UUID;

final class CacheDbManager extends SQLiteOpenHelper {

	private static final String CACHE_DB_FILENAME = "cache.db",
			TABLE = "web";

	public static final String
			FIELD_URL = "url",
			FIELD_ID = "id",
			FIELD_TIMESTAMP = "timestamp",
			FIELD_SESSION = "session",
			FIELD_USER = "user",
			FIELD_STATUS = "status",
			FIELD_TYPE = "type",
			FIELD_MIMETYPE = "mimetype";

	private static final int STATUS_MOVING = 1, STATUS_DONE = 2;

	private static final int CACHE_DB_VERSION = 1;

	CacheDbManager(final Context context) {
		super(context, CACHE_DB_FILENAME, null, CACHE_DB_VERSION);
	}

	@Override
	public void onCreate(final SQLiteDatabase db) {

		final String queryString = String.format(
				"CREATE TABLE %s (" +
						"%s INTEGER PRIMARY KEY AUTOINCREMENT," +
						"%s TEXT NOT NULL," +
						"%s TEXT NOT NULL," +
						"%s TEXT NOT NULL," +
						"%s INTEGER," +
						"%s INTEGER," +
						"%s INTEGER," +
						"%s TEXT," +
						"UNIQUE (%s, %s, %s) ON CONFLICT REPLACE)",
				TABLE,
				FIELD_ID,
				FIELD_URL,
				FIELD_USER,
				FIELD_SESSION,
				FIELD_TIMESTAMP,
				FIELD_STATUS,
				FIELD_TYPE,
				FIELD_MIMETYPE,
				FIELD_USER, FIELD_URL, FIELD_SESSION);

		db.execSQL(queryString);
	}

	@Override
	public void onUpgrade(
			final SQLiteDatabase db,
			final int oldVersion,
			final int newVersion) {
		throw new RuntimeException(
				"Attempt to upgrade database in first version of the app!");
	}

	synchronized Optional<CacheEntry> selectById(final long id) {

		final SQLiteDatabase db = getReadableDatabase();

		try(Cursor cursor = db.query(
				TABLE,
				CacheEntry.DB_FIELDS,
				String.format(Locale.US, "%s=?", FIELD_ID),
				new String[] {String.valueOf(id)},
				null,
				null,
				FIELD_TIMESTAMP + " DESC")) {

			final List<CacheEntry> entries = readEntriesFromCursor(cursor);

			if(entries.isEmpty()) {
				return Optional.empty();
			} else {
				return Optional.of(entries.get(0));
			}
		}
	}

	synchronized List<CacheEntry> select(
			final URI url,
			final String user,
			final UUID session) {

		final SQLiteDatabase db = getReadableDatabase();

		final String queryString;
		final String[] queryParams;

		if(session == null) {
			queryString = String.format(
					Locale.US,
					"%s=%d AND %s=? AND %s=?",
					FIELD_STATUS,
					STATUS_DONE,
					FIELD_URL,
					FIELD_USER);
			queryParams = new String[] {url.toString(), user};

		} else {
			queryString = String.format(
					Locale.US,
					"%s=%d AND %s=? AND %s=? AND %s=?",
					FIELD_STATUS,
					STATUS_DONE,
					FIELD_URL,
					FIELD_USER,
					FIELD_SESSION);
			queryParams = new String[] {url.toString(), user, session.toString()};
		}

		try(Cursor cursor = db.query(
				TABLE,
				CacheEntry.DB_FIELDS,
				queryString,
				queryParams,
				null,
				null,
				FIELD_TIMESTAMP + " DESC")) {

			return readEntriesFromCursor(cursor);
		}
	}

	@NonNull
	private List<CacheEntry> readEntriesFromCursor(@NonNull final Cursor cursor) {

		final ArrayList<CacheEntry> result = new ArrayList<>();

		while(cursor.moveToNext()) {
			result.add(new CacheEntry(cursor));
		}

		return result;
	}

	synchronized long newEntry(
			final CacheRequest request,
			final UUID session,
			final String mimetype) throws IOException {

		if(session == null) {
			throw new RuntimeException("No session to write");
		}

		final SQLiteDatabase db = this.getWritableDatabase();

		final ContentValues row = new ContentValues();

		row.put(FIELD_URL, request.url.toString());
		row.put(FIELD_USER, request.user.username);
		row.put(FIELD_SESSION, session.toString());
		row.put(FIELD_TYPE, request.fileType);
		row.put(FIELD_STATUS, STATUS_MOVING);
		row.put(FIELD_TIMESTAMP, RRTime.utcCurrentTimeMillis());
		row.put(FIELD_MIMETYPE, mimetype);

		final long result = db.insert(TABLE, null, row);

		if(result < 0) {
			throw new IOException("DB insert failed");
		}

		return result;
	}

	synchronized void setEntryDone(final long id) {
		final SQLiteDatabase db = this.getWritableDatabase();

		final ContentValues row = new ContentValues();
		row.put(FIELD_STATUS, STATUS_DONE);

		db.update(TABLE, row, FIELD_ID + "=?", new String[] {String.valueOf(id)});
	}

	synchronized int delete(final long id) {
		final SQLiteDatabase db = this.getWritableDatabase();
		return db.delete(TABLE, FIELD_ID + "=?", new String[] {String.valueOf(id)});
	}

	public synchronized ArrayList<Long> getFilesToPrune(
			final HashSet<Long> currentFiles,
			final HashMap<Integer, Long> maxAge,
			final long defaultMaxAge) {

		final SQLiteDatabase db = this.getWritableDatabase();

		final long currentTime = RRTime.utcCurrentTimeMillis();

		final Cursor cursor = db.query(
				TABLE,
				new String[] {FIELD_ID, FIELD_TIMESTAMP, FIELD_TYPE},
				null,
				null,
				null,
				null,
				null,
				null);

		final HashSet<Long> currentEntries = new HashSet<>();
		final ArrayList<Long> entriesToDelete = new ArrayList<>();
		final ArrayList<Long> filesToDelete = new ArrayList<>(32);

		while(cursor.moveToNext()) {

			final long id = cursor.getLong(0);
			final long timestamp = cursor.getLong(1);
			final int type = cursor.getInt(2);

			final long pruneIfBeforeMs;

			if(maxAge.containsKey(type)) {
				pruneIfBeforeMs = currentTime - maxAge.get(type);
			} else {
				Log.e("RR DEBUG cache", "Using default age! Filetype " + type);
				pruneIfBeforeMs = currentTime - defaultMaxAge;
			}

			if(!currentFiles.contains(id)) {
				entriesToDelete.add(id);

			} else if(timestamp < pruneIfBeforeMs) {
				entriesToDelete.add(id);
				filesToDelete.add(id);

			} else {
				currentEntries.add(id);
			}
		}

		for(final long id : currentFiles) {
			if(!currentEntries.contains(id)) {
				filesToDelete.add(id);
			}
		}

		if(!entriesToDelete.isEmpty()) {

			final StringBuilder query = new StringBuilder(String.format(
					Locale.US,
					"DELETE FROM %s WHERE %s IN (",
					TABLE,
					FIELD_ID));

			query.append(entriesToDelete.remove(entriesToDelete.size() - 1));

			for(final long id : entriesToDelete) {
				query.append(",").append(id);
				if(query.length() > 512 * 1024) {
					break;
				}
			}

			query.append(')');

			db.execSQL(query.toString());
		}

		cursor.close();

		return filesToDelete;
	}

	public synchronized void emptyTheWholeCache() {
		final SQLiteDatabase db = this.getWritableDatabase();
		db.execSQL(String.format(Locale.US, "DELETE FROM %s", TABLE));
	}

	public synchronized HashMap<Long, Integer> getFilesToSize() {
		final SQLiteDatabase db = this.getWritableDatabase();

		final Cursor cursor = db.query(
				TABLE,
				new String[] {FIELD_ID, FIELD_TYPE},
				null,
				null,
				null,
				null,
				null,
				null);

		final HashMap<Long, Integer> filesToCheck = new HashMap<>(32);

		while(cursor.moveToNext()) {
			final long id = cursor.getLong(0);
			final int type = cursor.getInt(1);

			filesToCheck.put(id, type);
		}

		cursor.close();

		return filesToCheck;
	}
}
