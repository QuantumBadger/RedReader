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

import android.database.Cursor;
import org.quantumbadger.redreader.common.General;

import java.net.URI;
import java.util.UUID;

public final class CacheEntry {

	public static final String[] DB_FIELDS = {
			CacheDbManager.FIELD_ID,
			CacheDbManager.FIELD_URL,
			CacheDbManager.FIELD_SESSION,
			CacheDbManager.FIELD_TIMESTAMP,
			CacheDbManager.FIELD_MIMETYPE};

	public final long id;
	public final URI url;
	public final UUID session;
	public final long timestamp;
	public final String mimetype;

	CacheEntry(final Cursor cursor) {

		id = cursor.getLong(0);
		url = General.uriFromString(cursor.getString(1));
		session = UUID.fromString(cursor.getString(2));
		timestamp = cursor.getLong(3);
		mimetype = cursor.getString(4);
	}
}
