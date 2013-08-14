package org.quantumbadger.redreader.ui.list;

import android.graphics.Bitmap;
import android.graphics.Canvas;
import android.graphics.Color;

public class RRListViewCacheBlock {

	private final Bitmap cache;
	private final Canvas canvas;

	private final int width, height;

	private static final int backgroundCol = Color.TRANSPARENT;

	private RRListViewContents.RRListViewFlattenedContents data = null;
	private int firstVisibleItemPos = -1, lastVisibleItemPos = -1, pxInFirstVisibleItem = -1;

	public RRListViewCacheBlock(final int width, final int height) {
		cache = Bitmap.createBitmap(width, height, Bitmap.Config.ARGB_8888);
		canvas = new Canvas(cache);
		this.width = width;
		this.height = height;
	}

	public void assign(RRListViewContents.RRListViewFlattenedContents data, int firstVisibleItemPos, int pxInFirstVisibleItem) {

		this.data = data;
		this.firstVisibleItemPos = firstVisibleItemPos;
		this.pxInFirstVisibleItem = pxInFirstVisibleItem;

		int pos = data.items[firstVisibleItemPos].setWidth(width) - pxInFirstVisibleItem;
		int lastVisibleItemPos = firstVisibleItemPos;

		while(pos <= height && lastVisibleItemPos < data.itemCount - 1) {
			lastVisibleItemPos++;
			pos += data.items[lastVisibleItemPos].setWidth(width);
		}

		this.lastVisibleItemPos = lastVisibleItemPos;

		canvas.drawColor(backgroundCol);

		canvas.translate(0, -pxInFirstVisibleItem);

		for(int i = firstVisibleItemPos; i <= lastVisibleItemPos; i++) {
			data.items[i].draw(canvas, width);
			canvas.translate(0, data.items[i].getHeight());
		}
	}

	public Bitmap getCache() {
		return cache;
	}
}
