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

package org.quantumbadger.redreader.reddit.prepared;

import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteOpenHelper;
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.common.Constants;
import org.quantumbadger.redreader.common.RRTime;

import java.util.HashMap;
import java.util.HashSet;

public class RedditChangeDataManager extends SQLiteOpenHelper {

	private static final String USERDATA_DB_FILENAME = "change_data.db";
	private static final int DB_VERSION = 1;

	private static final String TABLE_ACTIONS = "actions",
			FIELD_ACTIONS_ID = "id",
			FIELD_ACTIONS_USER = "user",
			FIELD_ACTIONS_PARENT = "parent",
			FIELD_ACTIONS_VOTEDIRECTION = "votedirection",
			FIELD_ACTIONS_SAVED = "saved",
			FIELD_ACTIONS_HIDDEN = "hidden",
			FIELD_ACTIONS_READ = "read",
			FIELD_ACTIONS_DIRTY = "dirty", // For future use
			FIELD_ACTIONS_LASTUPDATE = "lastupdate";

	private static RedditChangeDataManager singleton;

	public static synchronized RedditChangeDataManager getInstance(final Context context) {
		if(singleton == null) singleton = new RedditChangeDataManager(context.getApplicationContext());
		return singleton;
	}

	private RedditChangeDataManager(final Context context) {
		super(context.getApplicationContext(), USERDATA_DB_FILENAME, null, DB_VERSION);
	}

	@Override
	public void onCreate(final SQLiteDatabase db) {

		final String queryString = String.format(
				"CREATE TABLE %s (" +
						"%s TEXT PRIMARY KEY ON CONFLICT REPLACE," +
						"%s TEXT NOT NULL," +
						"%s TEXT," +
						"%s TEXT," +
						"%s INTEGER," +
						"%s INTEGER," +
						"%s INTEGER," +
						"%s INTEGER," +
						"%s INTEGER)",
				TABLE_ACTIONS,
				FIELD_ACTIONS_ID,
				FIELD_ACTIONS_USER,
				FIELD_ACTIONS_PARENT,
				FIELD_ACTIONS_READ,
				FIELD_ACTIONS_VOTEDIRECTION,
				FIELD_ACTIONS_SAVED,
				FIELD_ACTIONS_HIDDEN,
				FIELD_ACTIONS_DIRTY,
				FIELD_ACTIONS_LASTUPDATE);

		db.execSQL(queryString);
	}

	@Override
	public void onUpgrade(final SQLiteDatabase db, final int oldVersion, final int newVersion) {
		throw new RuntimeException("Attempt to upgrade V1 of the database");
	}

	public void prune(HashMap<Integer, Long> maxage) {

		final long earliestDate = RRTime.utcCurrentTimeMillis() - maxage.get(Constants.FileType.POST_LIST);

		getWritableDatabase().delete(TABLE_ACTIONS, FIELD_ACTIONS_LASTUPDATE + "<" + earliestDate, null);
	}

	public synchronized HashSet<String> getChangedForParent(String parent, RedditAccount user) {

		final SQLiteDatabase db = getReadableDatabase();

		final Cursor cursor = db.query(TABLE_ACTIONS, new String[]{FIELD_ACTIONS_ID}, String.format("%s=? AND %s=?", FIELD_ACTIONS_PARENT, FIELD_ACTIONS_USER), new String[]{parent, user.username}, null, null, null);

		if(cursor == null) {
			throw new NullPointerException("Cursor was null after query");
		}

		final HashSet<String> result = new HashSet<String>(16);

		try {
			while(cursor.moveToNext()) {
				result.add(cursor.getString(0));
			}

		} finally {
			cursor.close();
		}

		return result;
	}

	public synchronized void update(final String parent, final RedditAccount user, final RedditPreparedPost post, boolean writeIfNotFound) {

		final String[] fields = {
				FIELD_ACTIONS_LASTUPDATE,
				FIELD_ACTIONS_VOTEDIRECTION,
				FIELD_ACTIONS_SAVED,
				FIELD_ACTIONS_HIDDEN,
				FIELD_ACTIONS_READ
		};

		final SQLiteDatabase db = getReadableDatabase();

		final Cursor cursor = db.query(TABLE_ACTIONS, fields, String.format("%s=? AND %s=?", FIELD_ACTIONS_ID, FIELD_ACTIONS_USER), new String[]{post.idAndType, user.username}, null, null, null);

		if(cursor == null) {
			throw new NullPointerException("Cursor was null after query");
		}

		try {

			if(cursor.moveToNext()) {

				final long dbTimestamp = cursor.getLong(0);

				if(dbTimestamp > post.lastChange) {
					post.updateFromChangeDb(dbTimestamp, cursor.getInt(1), cursor.getInt(2) == 1, cursor.getInt(3) == 1, cursor.getInt(4) == 1);

				} else {
					post.setRead(cursor.getInt(4) == 1);
					insert(post, parent, user);
				}

			} else {
				if(writeIfNotFound) insert(post, parent, user);
			}

		} finally {
			cursor.close();
		}
	}

	private void insert(final RedditPreparedPost post, final String parent, final RedditAccount user) {

		final SQLiteDatabase db = this.getWritableDatabase();

		final ContentValues row = new ContentValues();

		row.put(FIELD_ACTIONS_ID, post.idAndType);
		row.put(FIELD_ACTIONS_USER, user.username);
		if(parent != null) row.put(FIELD_ACTIONS_PARENT, parent);
		row.put(FIELD_ACTIONS_READ, post.isRead() ? 1 : 0);
		row.put(FIELD_ACTIONS_VOTEDIRECTION, post.getVoteDirection());
		row.put(FIELD_ACTIONS_SAVED, post.isSaved() ? 1 : 0);
		row.put(FIELD_ACTIONS_HIDDEN, post.isHidden() ? 1 : 0);
		row.put(FIELD_ACTIONS_LASTUPDATE, post.lastChange);

		db.insert(TABLE_ACTIONS, null, row);
	}

	public synchronized void update(final String parent, final RedditAccount user, final RedditPreparedComment comment, boolean writeIfNotFound) {

		final String[] fields = {
				FIELD_ACTIONS_LASTUPDATE,
				FIELD_ACTIONS_VOTEDIRECTION
		};

		final SQLiteDatabase db = getReadableDatabase();

		final Cursor cursor = db.query(TABLE_ACTIONS, fields, String.format("%s=? AND %s=?", FIELD_ACTIONS_ID, FIELD_ACTIONS_USER), new String[]{comment.idAndType, user.username}, null, null, null);

		if(cursor == null) {
			throw new NullPointerException("Cursor was null after query");
		}

		try {

			if(cursor.moveToNext()) {

				final long dbTimestamp = cursor.getLong(0);

				if(dbTimestamp > comment.lastChange) {
					comment.updateFromChangeDb(dbTimestamp, cursor.getInt(1));

				} else {
					insert(comment, parent, user);
				}

			} else {
				if(writeIfNotFound) insert(comment, parent, user);
			}

		} finally {
			cursor.close();
		}
	}

	private void insert(final RedditPreparedComment comment, final String parent, final RedditAccount user) {

		final SQLiteDatabase db = this.getWritableDatabase();

		final ContentValues row = new ContentValues();

		row.put(FIELD_ACTIONS_ID, comment.idAndType);
		row.put(FIELD_ACTIONS_USER, user.username);
		row.put(FIELD_ACTIONS_PARENT, parent);
		row.put(FIELD_ACTIONS_VOTEDIRECTION, comment.getVoteDirection());
		row.put(FIELD_ACTIONS_LASTUPDATE, comment.lastChange);

		long pos = db.insert(TABLE_ACTIONS, null, row);
	}
}
