package org.quantumbadger.redreader.ui.list;

public class RRListViewCacheBlockRing {

	private final RRListViewCacheBlock[] blocks;

	// TODO do as little as possible here, move outside

	public RRListViewCacheBlockRing(final int listViewWidth, final int listViewHeight) {

		final int minBlocksOnScreen = 3;

		final int blockHeight = listViewHeight / minBlocksOnScreen;
		final int blockCount = minBlocksOnScreen + 3;

		blocks = new RRListViewCacheBlock[blockCount];

		for(int i = 0; i < blockCount; i++) {
			blocks[i] = new RRListViewCacheBlock(listViewWidth, blockHeight);
		}
	}


}
