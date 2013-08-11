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

public final class RRListViewCacheManager {

	private ListItemArrayLengthPair cached = new ListItemArrayLengthPair(1024);
	private ListItemArrayLengthPair tmp = new ListItemArrayLengthPair(1024);

	public synchronized void update(RRListViewContents.RRListViewFlattenedContents fc,
									int firstItemVisible, int lastItemVisible, int cacheSizeEitherSide,
									RRListView.RenderThread thread) {

		final int cacheStart = Math.max(0, firstItemVisible - cacheSizeEitherSide);
		final int cacheEnd = Math.min(fc.itemCount, lastItemVisible + cacheSizeEitherSide);
		final int cacheLen = cacheEnd - cacheStart;

		System.arraycopy(fc.items, cacheStart, tmp.data, 0, cacheLen);
		tmp.pos = cacheLen;
		tmp.bubbleSort();

		int cachedPos = 0, tmpPos = 0;

		while(cachedPos < cached.pos && tmpPos < tmp.pos) {

			final int cachedId = cached.data[cachedPos].globalItemId;
			final int tmpId = tmp.data[tmpPos].globalItemId;

			if(cachedId == tmpId) {
				cachedPos++;
				tmpPos++;

			} else if(cachedId > tmpId) {
				tmp.data[tmpPos].setCache(true, thread);
				tmpPos++;

			} else {
				cached.data[cachedPos].setCache(false, null);
				cachedPos++;
			}
		}

		while(cachedPos < cached.pos) {
			cached.data[cachedPos].setCache(false, null);
			cachedPos++;
		}

		while(tmpPos < tmp.pos) {
			tmp.data[tmpPos].setCache(true, thread);
			tmpPos++;
		}

		cached.clear();

		final ListItemArrayLengthPair newCached = tmp;
		tmp = cached;
		cached = newCached;
	}

	private final class ListItemArrayLengthPair {

		public final RRListViewItem[] data;
		public int pos = 0;

		public ListItemArrayLengthPair(int capacity) {
			this.data = new RRListViewItem[capacity];
		}

		public void clear() {
			Arrays.fill(data, 0, pos, null); // Avoids memory leaks
			pos = 0;
		}

		// We expect the list to be in order almost all of the time
		public void bubbleSort() {

			boolean isSorted = false;

			while(!isSorted) {

				isSorted = true;

				for(int i = 1; i < pos; i++) {
					if(data[i].globalItemId < data[i - 1].globalItemId) {
						final RRListViewItem tmp = data[i - 1];
						data[i - 1] = data[i];
						data[i] = tmp;
					}
				}
			}
		}
	}
}
