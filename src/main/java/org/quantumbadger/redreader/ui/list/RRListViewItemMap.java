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

package org.quantumbadger.redreader.ui.list;

import java.util.Arrays;

public final class RRListViewItemMap<E> {

	@SuppressWarnings("unchecked")
	private final Entries<E>[] entries = new Entries[1024];

	public void set(long id, E value) {

		final int pos = (int) (id % 1024);
		final Entries<E> e = entries[pos];

		if(e == null) {
			entries[pos] = new Entries<E>(id, value);
		} else {
			e.add(id, value);
		}
	}

	public E get(long id) {
		return entries[(int) (id % 1024)].get(id);
	}

	private static final class Entries<E> {

		@SuppressWarnings("unchecked")
		private E[] values = (E[]) new Object[1];
		private long[] ids = new long[1];

		public Entries(long id, E value) {
			ids[0] = id;
			values[0] = value;
		}

		public void add(long id, E value) {
			ids = Arrays.copyOf(ids, ids.length + 1);
			values = Arrays.copyOf(values, values.length + 1);
			ids[ids.length - 1] = id;
			values[values.length - 1] = value;
		}

		public E get(long id) {

			for(int i = 0; i < ids.length; i++) {
				if(id == ids[i]) return values[i];
			}

			throw new RuntimeException();
		}
	}

}
