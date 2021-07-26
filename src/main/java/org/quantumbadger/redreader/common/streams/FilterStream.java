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

package org.quantumbadger.redreader.common.streams;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

public class FilterStream<E> implements Stream<E> {

	@NonNull private final Stream<E> mInner;
	@NonNull private final Predicate<E> mPredicate;

	private boolean mHasNext = true;
	@Nullable private E mNext;

	public FilterStream(
			@NonNull final Stream<E> inner,
			@NonNull final Predicate<E> predicate) {

		mInner = inner;
		mPredicate = predicate;
		moveToNext();
	}

	private void moveToNext() {

		while(mInner.hasNext()) {

			mNext = mInner.next();

			if(mPredicate.matches(mNext)) {
				return;
			}
		}

		mNext = null;
		mHasNext = false;
	}

	@Override
	public boolean hasNext() {
		return mHasNext;
	}

	@Override
	public E next() {
		final E result = mNext;
		moveToNext();
		return result;
	}
}
