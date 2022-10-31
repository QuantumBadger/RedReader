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

package org.saiditnet.redreader.common;

import java.lang.ref.WeakReference;
import java.util.Iterator;
import java.util.LinkedList;

public abstract class UpdateNotifier<E> {

	private final LinkedList<WeakReference<E>> listeners = new LinkedList<>();

	public synchronized void addListener(final E updateListener) {
		listeners.add(new WeakReference<>(updateListener));
	}

	public synchronized void updateAllListeners() {

		final Iterator<WeakReference<E>> iter = listeners.iterator();

		while(iter.hasNext()) {
			final E listener = iter.next().get();

			if(listener == null) {
				iter.remove();
			} else {
				notifyListener(listener);
			}
		}

	}

	protected abstract void notifyListener(E listener);
}
