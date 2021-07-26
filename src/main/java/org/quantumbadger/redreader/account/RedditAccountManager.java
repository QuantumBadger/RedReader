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

package org.quantumbadger.redreader.account;

import android.annotation.SuppressLint;
import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteOpenHelper;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import org.quantumbadger.redreader.activities.BugReportActivity;
import org.quantumbadger.redreader.common.UpdateNotifier;
import org.quantumbadger.redreader.reddit.api.RedditOAuth;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;

public final class RedditAccountManager extends SQLiteOpenHelper {

	private List<RedditAccount> accountsCache = null;
	private RedditAccount defaultAccountCache = null;

	private static final RedditAccount ANON = new RedditAccount(
			"",
			null,
			true,
			10);

	private final Context context;

	private final UpdateNotifier<RedditAccountChangeListener> updateNotifier
			= new UpdateNotifier<RedditAccountChangeListener>() {
		@Override
		protected void notifyListener(final RedditAccountChangeListener listener) {
			listener.onRedditAccountChanged();
		}
	};

	private static final String ACCOUNTS_DB_FILENAME = "accounts_oauth2.db";
	private static final String TABLE = "accounts_oauth2";
	private static final String FIELD_USERNAME = "username";
	private static final String FIELD_REFRESH_TOKEN = "refresh_token";
	private static final String FIELD_PRIORITY = "priority";
	private static final String FIELD_USES_NEW_CLIENT_ID = "uses_new_client_id";

	private static final int ACCOUNTS_DB_VERSION = 3;

	@SuppressLint("StaticFieldLeak") private static RedditAccountManager singleton;

	public static synchronized RedditAccountManager getInstance(final Context context) {
		if(singleton == null) {
			singleton = new RedditAccountManager(context.getApplicationContext());
		}
		return singleton;
	}

	public static RedditAccount getAnon() {
		return ANON;
	}

	private RedditAccountManager(final Context context) {
		super(
				context.getApplicationContext(),
				ACCOUNTS_DB_FILENAME,
				null,
				ACCOUNTS_DB_VERSION);
		this.context = context;
	}

	@Override
	public void onCreate(final SQLiteDatabase db) {

		final String queryString = String.format(
				"CREATE TABLE %s (" +
						"%s TEXT NOT NULL PRIMARY KEY ON CONFLICT REPLACE," +
						"%s TEXT," +
						"%s INTEGER," +
						"%s BOOLEAN NOT NULL)",
				TABLE,
				FIELD_USERNAME,
				FIELD_REFRESH_TOKEN,
				FIELD_PRIORITY,
				FIELD_USES_NEW_CLIENT_ID);

		db.execSQL(queryString);

		addAccount(getAnon(), db);
	}

	@Override
	public void onUpgrade(
			final SQLiteDatabase db,
			final int oldVersion,
			final int newVersion) {

		if(oldVersion < 2) {

			db.execSQL(String.format(
					Locale.US,
					"UPDATE %s SET %2$s=TRIM(%2$s) WHERE %2$s <> TRIM(%2$s)",
					TABLE,
					FIELD_USERNAME));

		}

		if(oldVersion < 3) {

			db.execSQL(String.format(
					Locale.US,
					"ALTER TABLE %s ADD COLUMN %s BOOLEAN NOT NULL DEFAULT 0",
					TABLE,
					FIELD_USES_NEW_CLIENT_ID));
		}
	}

	public synchronized void addAccount(final RedditAccount account) {
		addAccount(account, null);
	}

	private synchronized void addAccount(
			final RedditAccount account,
			final SQLiteDatabase inDb) {

		final SQLiteDatabase db;
		if(inDb == null) {
			db = getWritableDatabase();
		} else {
			db = inDb;
		}

		final ContentValues row = new ContentValues();

		row.put(FIELD_USERNAME, account.username);

		if(account.refreshToken == null) {
			row.putNull(FIELD_REFRESH_TOKEN);
		} else {
			row.put(FIELD_REFRESH_TOKEN, account.refreshToken.token);
		}

		row.put(FIELD_PRIORITY, account.priority);
		row.put(FIELD_USES_NEW_CLIENT_ID, account.usesNewClientId);

		db.insert(TABLE, null, row);

		reloadAccounts(db);
		updateNotifier.updateAllListeners();

		if(inDb == null) {
			db.close();
		}
	}

	public synchronized ArrayList<RedditAccount> getAccounts() {

		if(accountsCache == null) {
			final SQLiteDatabase db = getReadableDatabase();
			reloadAccounts(db);
			db.close();
		}

		return new ArrayList<>(accountsCache);
	}

	@Nullable
	public RedditAccount getAccount(@NonNull final String username) {

		if("".equals(username)) {
			return getAnon();
		}

		final ArrayList<RedditAccount> accounts = getAccounts();
		RedditAccount selectedAccount = null;

		for(final RedditAccount account : accounts) {
			if(!account.isAnonymous() && account.username.equalsIgnoreCase(username)) {
				selectedAccount = account;
				break;
			}
		}

		return selectedAccount;
	}

	public synchronized RedditAccount getDefaultAccount() {

		if(defaultAccountCache == null) {
			final SQLiteDatabase db = getReadableDatabase();
			reloadAccounts(db);
			db.close();
		}

		return defaultAccountCache;
	}

	public synchronized void setDefaultAccount(final RedditAccount newDefault) {

		final SQLiteDatabase db = getWritableDatabase();

		db.execSQL(
				String.format(
						Locale.US,
						"UPDATE %s SET %s=(SELECT MIN(%s)-1 FROM %s) WHERE %s=?",
						TABLE,
						FIELD_PRIORITY,
						FIELD_PRIORITY,
						TABLE,
						FIELD_USERNAME),
				new String[] {newDefault.username});

		reloadAccounts(db);
		db.close();

		updateNotifier.updateAllListeners();
	}

	private synchronized void reloadAccounts(final SQLiteDatabase db) {

		final String[] fields = {
				FIELD_USERNAME,
				FIELD_REFRESH_TOKEN,
				FIELD_PRIORITY,
				FIELD_USES_NEW_CLIENT_ID};

		final Cursor cursor = db.query(
				TABLE,
				fields,
				null,
				null,
				null,
				null,
				FIELD_PRIORITY + " ASC");

		accountsCache = new LinkedList<>();
		defaultAccountCache = null;

		if(cursor != null) {

			while(cursor.moveToNext()) {

				final String username = cursor.getString(0);

				final RedditOAuth.RefreshToken refreshToken;
				if(cursor.isNull(1)) {
					refreshToken = null;
				} else {
					refreshToken = new RedditOAuth.RefreshToken(cursor.getString(1));
				}

				final long priority = cursor.getLong(2);
				final boolean usesNewClientId = cursor.getInt(3) != 0;

				final RedditAccount account = new RedditAccount(
						username,
						refreshToken,
						usesNewClientId,
						priority);

				accountsCache.add(account);

				if(defaultAccountCache == null
						|| account.priority < defaultAccountCache.priority) {
					defaultAccountCache = account;
				}
			}

			cursor.close();

		} else {
			BugReportActivity.handleGlobalError(context, "Cursor was null after query");
		}
	}

	public void addUpdateListener(final RedditAccountChangeListener listener) {
		updateNotifier.addListener(listener);
	}

	public void deleteAccount(final RedditAccount account) {

		final SQLiteDatabase db = getWritableDatabase();
		db.delete(TABLE, FIELD_USERNAME + "=?", new String[] {account.username});
		reloadAccounts(db);
		updateNotifier.updateAllListeners();
		db.close();
	}
}
