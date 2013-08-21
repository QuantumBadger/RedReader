package org.quantumbadger.redreader.ui.list;

import android.graphics.Canvas;
import android.graphics.Color;
import org.quantumbadger.redreader.common.UnexpectedInternalStateException;
import org.quantumbadger.redreader.common.collections.FixedCircularList;

public final class RRListViewCacheBlockRing {

	private final FixedCircularList<RRListViewCacheBlock> blocks;
	public final int blockHeight;

	private RRListViewFlattenedContents data;

	public RRListViewCacheBlockRing(final int blockWidth, final int blockHeight, final int blockCount) {

		this.blockHeight = blockHeight;
		blocks = new FixedCircularList<RRListViewCacheBlock>(blockCount);

		for(int i = 0; i < blockCount; i++) {
			blocks.setRelative(i, new RRListViewCacheBlock(blockWidth, blockHeight, debugBlockCol(i)));
		}
	}

	public synchronized void assign(final RRListViewFlattenedContents data, final int firstVisibleItemPos, final int pxInFirstVisibleItem) {

		this.data = data;

		for(int i = -1; i < blocks.size - 1; i++) {
			blocks.getRelative(i).assign(data, firstVisibleItemPos, pxInFirstVisibleItem + i * blockHeight);
		}
	}

	public synchronized void moveForward() {
		blocks.moveRelative(1);
		final RRListViewCacheBlock penultimateBlock = blocks.getRelative(-3);
		blocks.getRelative(-2).assign(data, penultimateBlock.firstVisibleItemPos, penultimateBlock.pxInFirstVisibleItem + blockHeight);
	}

	public synchronized void moveBackward() {
		blocks.moveRelative(-1);
		final RRListViewCacheBlock secondBlock = blocks.getRelative(0);
		blocks.getRelative(-1).assign(data, secondBlock.firstVisibleItemPos, secondBlock.pxInFirstVisibleItem - blockHeight);
	}

	public synchronized boolean draw(final Canvas canvas, final int canvasHeight) {

		boolean drawSuccessful = true;
		int totalHeight = 0;
		int block = 0;

		canvas.save();

		while(totalHeight < canvasHeight) {
			if(!blocks.getRelative(block++).draw(canvas)) drawSuccessful = false;
			canvas.translate(0, blockHeight);
			totalHeight += blockHeight;
		}

		canvas.restore();

		return drawSuccessful;
	}

	private int debugBlockCol(int id) {
		switch(id) {
			case 0: return Color.RED;
			case 1: return Color.GREEN;
			case 2: return Color.BLUE;
			case 3: return Color.YELLOW;
			case 4: return Color.GRAY;
			default: throw new UnexpectedInternalStateException();
		}
	}
}
