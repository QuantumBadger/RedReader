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

package org.quantumbadger.redreader.common.collections;

import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.Set;

import static java.util.Collections.synchronizedSet;

public class UniqueSynchronizedQueue<E> {
	private final Set<E> uniqueSynchronizedQueue = synchronizedSet(new LinkedHashSet<E>());

	public void enqueue(E object) {
		uniqueSynchronizedQueue.add(object);
	}

	public E dequeue() {
		Iterator<E> i = uniqueSynchronizedQueue.iterator();
		final E result = i.next();
		i.remove();

		return result;
	}
}
