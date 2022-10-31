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

package org.saiditnet.redreader.common.collections;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

public class WeakReferenceListHashMapManager<K, V> {

	private final HashMap<K, WeakReferenceListManager<V>> mData = new HashMap<>();

	private byte mCleanupCounter = 0;

	public synchronized void add(final K key, final V value) {

		WeakReferenceListManager<V> list = mData.get(key);

		if(list == null) {
			list = new WeakReferenceListManager<>();
			mData.put(key, list);
		}

		list.add(value);

		// Perform cleanup once for each 256 values which are added
		if(++mCleanupCounter == 0) {
			clean();
		}
	}

	public synchronized void remove(final K key, final V value) {

		WeakReferenceListManager<V> list = mData.get(key);

		if(list != null) {
			list.remove(value);
		}
	}

	public synchronized void map(
			final K key,
			final WeakReferenceListManager.Operator<V> operator) {

		WeakReferenceListManager<V> list = mData.get(key);

		if(list != null) {
			list.map(operator);
		}
	}

	public synchronized <A> void map(
			final K key,
			final WeakReferenceListManager.ArgOperator<V, A> operator,
			final A arg) {

		WeakReferenceListManager<V> list = mData.get(key);

		if(list != null) {
			list.map(operator, arg);
		}
	}

	public synchronized void clean() {

		final Iterator<Map.Entry<K, WeakReferenceListManager<V>>> iterator = mData.entrySet().iterator();

		while(iterator.hasNext()) {

			final Map.Entry<K, WeakReferenceListManager<V>> entry = iterator.next();

			final WeakReferenceListManager<V> list = entry.getValue();
			list.clean();

			if(list.isEmpty()) {
				iterator.remove();
			}
		}
	}
}
