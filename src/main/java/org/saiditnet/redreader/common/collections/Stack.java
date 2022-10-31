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

import java.util.ArrayList;

public class Stack<E> {

	private final ArrayList<E> mData;

	public Stack(int initialCapacity) {
		mData = new ArrayList<>(initialCapacity);
	}

	public void push(E obj) {
		mData.add(obj);
	}

	public E pop() {
		return mData.remove(mData.size() - 1);
	}

	public boolean isEmpty() {
		return mData.isEmpty();
	}

	public boolean remove(E obj) {
		return mData.remove(obj);
	}
}
