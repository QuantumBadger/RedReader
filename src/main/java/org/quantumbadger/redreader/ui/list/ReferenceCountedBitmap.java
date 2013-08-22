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

import android.graphics.Bitmap;

import java.util.concurrent.atomic.AtomicInteger;

public final class ReferenceCountedBitmap {

	private final AtomicInteger references = new AtomicInteger(1);
	private final Bitmap bitmap;

	public ReferenceCountedBitmap(Bitmap bitmap) {
		this.bitmap = bitmap;
	}

	public Bitmap lock() {
		return !bitmap.isRecycled() && doLock() ? bitmap : null;
	}

	private boolean doLock() {

		while(true) {
			final int refCount = references.get();
			if(refCount == 0) return false;
			if(references.compareAndSet(refCount, refCount + 1)) return true;
		}
	}

	public void release() {

		if(references.decrementAndGet() == 0) {
			bitmap.recycle();
		}
	}
}
