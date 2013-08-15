package org.quantumbadger.redreader.ui.list;

public final class RRListViewCacheBlockRing {

	private final RRListViewCacheBlock[] blocks;
	private int pos = 0;

	private final int blockHeight;

	public RRListViewCacheBlockRing(final int blockWidth, final int blockHeight, final int blockCount) {

		this.blockHeight = blockHeight;
		blocks = new RRListViewCacheBlock[blockCount];

		for(int i = 0; i < blockCount; i++) {
			blocks[i] = new RRListViewCacheBlock(blockWidth, blockHeight);
		}
	}

	public RRListViewCacheBlock getRelative(int relativeIndex) {
		return blocks[mod(pos + relativeIndex, blocks.length)];
	}

	public void moveRelative(int relativeIndex) {
		pos = mod(pos + relativeIndex, blocks.length);
	}

	private static int mod(int a, int b) {
		return (a % b + b) % b;
	}

	public void assign(final RRListViewFlattenedContents data, final int firstVisibleItemPos, final int pxInFirstVisibleItem) {

		for(int i = -1; i < blocks.length - 1; i++) {
			getRelative(0).assign(data, firstVisibleItemPos, pxInFirstVisibleItem + i * blockHeight);
		}
	}
}
