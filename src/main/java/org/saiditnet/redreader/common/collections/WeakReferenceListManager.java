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

import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.Iterator;

public final class WeakReferenceListManager<E> {

	private final ArrayList<WeakReference<E>> data = new ArrayList<>();

	public synchronized int size() {
		return data.size();
	}

	public synchronized void add(final E object) {
		data.add(new WeakReference<>(object));
	}

	public synchronized void map(final Operator<E> operator) {

		final Iterator<WeakReference<E>> iterator = data.iterator();

		while(iterator.hasNext()) {
			final E object = iterator.next().get();

			if(object == null) {
				iterator.remove();
			} else {
				operator.operate(object);
			}
		}
	}

	public synchronized <A> void map(final ArgOperator<E, A> operator, final A arg) {

		final Iterator<WeakReference<E>> iterator = data.iterator();

		while(iterator.hasNext()) {
			final E object = iterator.next().get();

			if(object == null) {
				iterator.remove();
			} else {
				operator.operate(object, arg);
			}
		}
	}

	public synchronized void remove(final E object) {
		final Iterator<WeakReference<E>> iterator = data.iterator();

		while(iterator.hasNext()) {
			if(iterator.next().get() == object) iterator.remove();
		}
	}

	public synchronized void clean() {

		final Iterator<WeakReference<E>> iterator = data.iterator();

		while(iterator.hasNext()) {
			final E object = iterator.next().get();

			if(object == null) {
				iterator.remove();
			}
		}
	}

	public synchronized boolean isEmpty() {
		return data.isEmpty();
	}

	public interface Operator<E> {
		void operate(E object);
	}

	public interface ArgOperator<E, A> {
		void operate(E object, A arg);
	}
}
