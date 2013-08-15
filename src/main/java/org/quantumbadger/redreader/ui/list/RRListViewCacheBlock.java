package org.quantumbadger.redreader.ui.list;

import android.graphics.Bitmap;
import android.graphics.Canvas;
import android.graphics.Color;

public class RRListViewCacheBlock {

	private final Bitmap cache;
	private final Canvas canvas;

	private final int width, height;

	private static final int backgroundCol = Color.TRANSPARENT;

	public RRListViewCacheBlock(final int width, final int height) {
		cache = Bitmap.createBitmap(width, height, Bitmap.Config.ARGB_8888);
		canvas = new Canvas(cache);
		this.width = width;
		this.height = height;
	}

	public void assign(RRListViewFlattenedContents data, int firstVisibleItemPos, int pxInFirstVisibleItem) {

		final RRListViewItem[] items = data.items;

		while(pxInFirstVisibleItem < 0 && firstVisibleItemPos > 0) {
			firstVisibleItemPos--;
			pxInFirstVisibleItem += items[firstVisibleItemPos].getHeight();
		}

		while(pxInFirstVisibleItem >= items[firstVisibleItemPos].getHeight()) {

			if(firstVisibleItemPos >= items.length - 1) {
				firstVisibleItemPos = -1;
				break;
			} else {
				pxInFirstVisibleItem -= items[firstVisibleItemPos].getHeight();
				firstVisibleItemPos++;
			}
		}

		int pos = data.items[firstVisibleItemPos].setWidth(width) - pxInFirstVisibleItem;
		int lastVisibleItemPos = firstVisibleItemPos;

		while(pos <= height && lastVisibleItemPos < data.itemCount - 1) {
			lastVisibleItemPos++;
			pos += data.items[lastVisibleItemPos].setWidth(width);
		}

		canvas.save();
		canvas.drawColor(backgroundCol);
		canvas.translate(0, -pxInFirstVisibleItem);

		for(int i = firstVisibleItemPos; i <= lastVisibleItemPos; i++) {
			data.items[i].draw(canvas, width);
			canvas.translate(0, data.items[i].getHeight());
		}

		canvas.restore();
	}

	public Bitmap getCache() {
		return cache;
	}
}
