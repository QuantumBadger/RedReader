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

import androidx.annotation.NonNull;

public enum CacheCompressionType {

	NONE(0),
	ZSTD(1);

	public final int databaseId;

	CacheCompressionType(final int databaseId) {
		this.databaseId = databaseId;
	}

	@NonNull
	public static CacheCompressionType fromDatabaseId(final int databaseId) {

		for(final CacheCompressionType type : values()) {
			if(type.databaseId == databaseId) {
				return type;
			}
		}

		throw new RuntimeException("Unknown compression type " + databaseId);
	}
}
