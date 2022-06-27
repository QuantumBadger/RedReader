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

package org.quantumbadger.redreader.common;

import androidx.annotation.Nullable;

import java.util.HashSet;

public class EventListenerSet<E> {

	public interface Listener<E> {
		void onEvent(E event);
	}

	@Nullable private E mMostRecentEvent;

	private final ThreadCheckedVar<HashSet<Listener<E>>> mListeners
			= new ThreadCheckedVar<>(new HashSet<>());

	public void send(final E event) {
		mMostRecentEvent = event;
		for(final Listener<E> listener : mListeners.get()) {
			listener.onEvent(event);
		}
	}

	public E register(final Listener<E> listener) {
		mListeners.get().add(listener);
		return mMostRecentEvent;
	}

	public void unregister(final Listener<E> listener) {
		mListeners.get().remove(listener);
	}
}
