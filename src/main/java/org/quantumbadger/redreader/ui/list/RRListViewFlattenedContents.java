package org.quantumbadger.redreader.ui.list;

public final class RRListViewFlattenedContents {

	final RRListViewItem[] items;
	final int itemCount;
	final RRListViewItemToIntMap reverseMap;

	public RRListViewFlattenedContents(RRListViewItem[] items, int itemCount, RRListViewItemToIntMap reverseMap) {
		this.items = items;
		this.itemCount = itemCount;
		this.reverseMap = reverseMap;
	}
}
