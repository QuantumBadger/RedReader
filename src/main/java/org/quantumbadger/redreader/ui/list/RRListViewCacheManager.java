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

import android.util.Log;

import java.util.HashSet;

public final class RRListViewCacheManager {

	private final HashSet<RRListViewItem> cached = new HashSet<RRListViewItem>(64);

	public synchronized void update(RRListViewContents.RRListViewFlattenedContents fc,
									int firstItemVisible, int lastItemVisible, int cacheSizeEitherSide,
									RRListView.RenderThread thread) {

		// TODO make local vars for fc.items, fc.items[i]

		final HashSet<RRListViewItem> noLongerCached = new HashSet<RRListViewItem>(cached);

		final int firstItemToCache = Math.max(0, firstItemVisible - cacheSizeEitherSide);
		final int lastItemToCache = Math.min(fc.itemCount - 1, lastItemVisible + cacheSizeEitherSide);

		for(int i = firstItemToCache; i <= lastItemToCache; i++) {

			if(!noLongerCached.remove(fc.items[i])) {
				fc.items[i].setCache(true, thread);
				cached.add(fc.items[i]);
				Log.i("SCROLL CACHE", "Item added to cache: " + i + ", #" + fc.items[i].globalItemId);
			}
		}

		for(final RRListViewItem item : noLongerCached) {
			item.setCache(false, null);
			cached.remove(item);
			Log.i("SCROLL CACHE", "Item removed from cache, #" + item.globalItemId);
		}
	}
}
