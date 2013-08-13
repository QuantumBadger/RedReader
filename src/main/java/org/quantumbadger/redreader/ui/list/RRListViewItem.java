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

import android.graphics.Bitmap;
import android.graphics.Canvas;
import android.os.Handler;
import android.os.Looper;
import android.os.Message;

import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

public abstract class RRListViewItem {

	private static final AtomicInteger maxItemId = new AtomicInteger(0);
	public final int globalItemId = maxItemId.incrementAndGet();

	private volatile boolean shouldCacheView = false;
	private final AtomicReference<Bitmap> cachedView = new AtomicReference<Bitmap>();

	protected int width = -1;
	public int height = -1;

	private RRListView parent;

	private float xVel = 0, xPos = 0; // TODO account for dpi, fps

	private static final Handler recycleHandler = new Handler(Looper.getMainLooper()) {
		@Override
		public void handleMessage(Message msg) {
			((Bitmap)msg.obj).recycle();
		}
	};

	private void updateCache(Bitmap newCache) {
		final Bitmap cache = cachedView.getAndSet(newCache);
		if(cache != null) recycleHandler.sendMessage(Message.obtain(recycleHandler, 0, cache));
	}

	protected synchronized final int setWidth(int width) {

		if(width == this.width) return height;

		this.width = width;
		height = onMeasureHeight(width);

		updateCache(null);

		return height;
	}

	protected abstract int onMeasureHeight(int width);

	public final void setParent(RRListView parent) {
		this.parent = parent;
	}

	public final void draw(Canvas c, int width) {

		if(width != this.width) setWidth(width);

		final Bitmap cache = cachedView.get();

		if(cache == null || cache.isRecycled()) {
			if(shouldCacheView) {
				final Bitmap newCache = doCacheRender(width, true);
				c.drawBitmap(newCache, 0, 0, null);
			} else {
				onRender(c);
			}
		} else {
			c.drawBitmap(cache, 0, 0, null);
		}
	}

	protected abstract void onRender(Canvas c);

	@Override
	public final int hashCode() {
		return globalItemId;
	}

	public abstract boolean isVisible();

	public final void setCache(final boolean enabled, final RRListView.RenderThread renderThread) {

		shouldCacheView = enabled;

		if(enabled) {
			if(cachedView.get() == null && renderThread != null) renderThread.add(this);

		} else {
			updateCache(null);
		}
	}

	public final Bitmap doCacheRender(final int width, final boolean evenIfCacheIsDisabled) {

		if(width == 0) return null;

		if(!evenIfCacheIsDisabled && !shouldCacheView) return null;

		if(width != this.width || height < 0) setWidth(width);

		final Bitmap newCache = Bitmap.createBitmap(width, height, Bitmap.Config.ARGB_8888);
		onRender(new Canvas(newCache));

		if(shouldCacheView && this.width == width) {
			updateCache(newCache);
		}

		return newCache;
	}

	public final void invalidate() {

		updateCache(null);

		width = -1;
		height = -1;

		// TODO notify parent that this view is invalidated
	}
}
