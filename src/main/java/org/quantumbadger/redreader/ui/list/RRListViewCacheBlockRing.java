package org.quantumbadger.redreader.ui.list;

import android.graphics.Canvas;
import android.graphics.Color;
import org.quantumbadger.redreader.common.UnexpectedInternalStateException;

public final class RRListViewCacheBlockRing {

	private final RRListViewCacheBlock[] blocks;
	private int pos = 0;

	public final int blockHeight;

	private RRListViewFlattenedContents data;

	public RRListViewCacheBlockRing(final int blockWidth, final int blockHeight, final int blockCount) {

		this.blockHeight = blockHeight;
		blocks = new RRListViewCacheBlock[blockCount];

		for(int i = 0; i < blockCount; i++) {
			blocks[i] = new RRListViewCacheBlock(blockWidth, blockHeight, debugBlockCol(i));
		}
	}

	private int debugBlockCol(int id) {
		switch(id) {
			case 0: return Color.RED;
			case 1: return Color.GREEN;
			case 2: return Color.BLUE;
			case 3: return Color.GRAY;
			default: throw new UnexpectedInternalStateException();
		}
	}

	private RRListViewCacheBlock getRelative(int relativeIndex) {
		return blocks[mod(pos + relativeIndex, blocks.length)];
	}

	private void moveRelative(int relativeIndex) {
		pos = mod(pos + relativeIndex, blocks.length);
	}

	private static int mod(int a, int b) {
		return (a % b + b) % b;
	}

	public synchronized void assign(final RRListViewFlattenedContents data, final int firstVisibleItemPos, final int pxInFirstVisibleItem) {

		this.data = data;

		for(int i = -1; i < blocks.length - 1; i++) {
			getRelative(i).assign(data, firstVisibleItemPos, pxInFirstVisibleItem + i * blockHeight);
		}
	}

	public synchronized void moveForward() {
		moveRelative(1);
		final RRListViewCacheBlock penultimateBlock = getRelative(-3);
		getRelative(-2).assign(data, penultimateBlock.firstVisibleItemPos, penultimateBlock.pxInFirstVisibleItem + blockHeight);
	}

	public synchronized void moveBackward() {
		moveRelative(-1);
		final RRListViewCacheBlock secondBlock = getRelative(0);
		getRelative(-1).assign(data, secondBlock.firstVisibleItemPos, secondBlock.pxInFirstVisibleItem - blockHeight);
	}

	public synchronized boolean draw(final Canvas canvas, final int canvasHeight) {

		boolean drawSuccessful = true;
		int totalHeight = 0;
		int block = 0;

		canvas.save();

		while(totalHeight < canvasHeight) {
			if(!getRelative(block++).draw(canvas)) drawSuccessful = false;
			canvas.translate(0, blockHeight);
			totalHeight += blockHeight;
		}

		canvas.restore();

		return drawSuccessful;
	}
}
