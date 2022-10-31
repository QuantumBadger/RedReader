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

package org.saiditnet.redreader.cache;

import android.database.Cursor;

import java.util.UUID;

public final class CacheEntry {

	public final long id;
	//private final URI url;
	//private final String user;
	public final UUID session;

	public final long timestamp;
	//private final int status;
	//private final int type;
	public final String mimetype;

	CacheEntry(final Cursor cursor) {

		id = cursor.getLong(0);
		//url = General.uriFromString(cursor.getString(1));
		//user = cursor.getString(2);
		session = UUID.fromString(cursor.getString(3));
		timestamp = cursor.getLong(4);
		//status = cursor.getInt(5);
		//type = cursor.getInt(6);
		mimetype = cursor.getString(7);
	}
}
