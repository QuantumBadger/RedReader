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

import java.util.LinkedList;

public final class RRListViewContents {

	private final RRListView parent;

	private final LinkedList<RRListViewItem> items = new LinkedList<RRListViewItem>();

	private RRListViewFlattenedContents flattenedContents
			= new RRListViewFlattenedContents(new RRListViewItem[0], 0, new RRListViewItemToIntMap());

	public RRListViewContents(RRListView parent) {
		this.parent = parent;
	}

	public synchronized void appendChild(RRListViewItem item) {

		items.add(item);

		if(item.isVisible()) {

			final RRListViewItem[] items = flattenedContents.items;
			final int itemCount = flattenedContents.itemCount;
			final RRListViewItemToIntMap reverseMap = flattenedContents.reverseMap;

			if(itemCount < items.length) {

				items[itemCount] = item;
				reverseMap.set(item.globalItemId, itemCount);

				flattenedContents = new RRListViewFlattenedContents(items, itemCount + 1, reverseMap);

				parent.onChildAppended();

			} else {
				// TODO optimise?
				recomputeFlattenedContents();
			}
		}
	}

	public synchronized void appendChildAfter(RRListViewItem parent, RRListViewItem item) {

		items.add(items.indexOf(parent) + 1, item);
		if(item.isVisible()) recomputeFlattenedContents();
	}

	public synchronized void recomputeFlattenedContents() {

		final RRListViewItem[] result = new RRListViewItem[Math.max(256, items.size() * 2 + 1)];
		int itemCount = 0;
		final RRListViewItemToIntMap map = new RRListViewItemToIntMap();

		for(final RRListViewItem item : items) {
			if(item.isVisible()) {
				map.set(item.globalItemId, itemCount);
				result[itemCount] = item;
				itemCount++;
			}
		}

		flattenedContents = new RRListViewFlattenedContents(result, itemCount, map);
		parent.onChildrenRecomputed();
	}

	public RRListViewFlattenedContents getFlattenedContents() {
		return flattenedContents;
	}

}
