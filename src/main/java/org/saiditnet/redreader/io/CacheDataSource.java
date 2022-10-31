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

package org.saiditnet.redreader.io;

import org.saiditnet.redreader.common.TimestampBound;

import java.util.Collection;
import java.util.HashMap;

public interface CacheDataSource<K, V, F> {
	void performRequest(K key, final TimestampBound timestampBound, RequestResponseHandler<V, F> handler);

	void performRequest(Collection<K> keys, final TimestampBound timestampBound, RequestResponseHandler<HashMap<K, V>, F> handler);

	void performWrite(V value);

	void performWrite(Collection<V> values);
}
