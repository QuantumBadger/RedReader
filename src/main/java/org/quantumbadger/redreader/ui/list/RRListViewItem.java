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

import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.AtomicReference;

public abstract class RRListViewItem {

	private static final AtomicLong maxItemId = new AtomicLong(0);
	public final long globalItemId = maxItemId.incrementAndGet();

	private volatile boolean shouldCacheView = true; // TODO should be false
	private final AtomicReference<ReferenceCountedBitmap> cachedView = new AtomicReference<ReferenceCountedBitmap>();

	protected int width = -1;
	public int height = -1;

	private RRListView parent;

	private boolean dividingLine;
	private float xVel = 0, xPos = 0; // TODO account for dpi, fps

	protected int measureHeight(int width) {

		if(width == this.width) return height;

		this.width = width;
		height = onMeasureHeight(width);

		final ReferenceCountedBitmap cache = cachedView.getAndSet(null);
		if(cache != null) cache.release();

		return height;
	}

	protected abstract int onMeasureHeight(int width);

	public final void setParent(RRListView parent) {
		this.parent = parent;
	}

	// TODO return height:
	public final void draw(Canvas c, int width) {

		if(width != this.width) measureHeight(width);

		final ReferenceCountedBitmap currentCachedView = cachedView.get();
		final Bitmap cache = currentCachedView == null ? null : currentCachedView.lock();

		try {

			if(cache == null) {
				if(shouldCacheView) {
					final ReferenceCountedBitmap newlyCachedView = doCacheRender(width, true);
					final Bitmap newCache = newlyCachedView.lock();
					c.drawBitmap(newCache, 0, 0, null);
					newlyCachedView.release();
				} else {
					onRender(c);
				}
			} else {
				c.drawBitmap(cache, 0, 0, null);
			}

		} finally {
			if(currentCachedView != null) currentCachedView.release();
		}
	}

	protected abstract void onRender(Canvas c);

	@Override
	public int hashCode() {
		return (int)globalItemId;
	}

	public abstract boolean isVisible();

	public void setCache(final boolean enabled, final RRListView.RenderThread renderThread) {

		shouldCacheView = enabled;

		if(enabled) {
			if(cachedView.get() == null && renderThread != null) renderThread.add(this);

		} else {

			final ReferenceCountedBitmap cache = cachedView.getAndSet(null);
			if(cache != null) cache.release();
		}
	}

	public ReferenceCountedBitmap doCacheRender(final int width, final boolean evenIfCacheIsDisabled) {

		if(width == 0) return null;

		if(!evenIfCacheIsDisabled && !shouldCacheView) return null;

		final int currentWidth = this.width;

		if(width != currentWidth || height < 0) measureHeight(width);

		final Bitmap newCache = Bitmap.createBitmap(width, height, Bitmap.Config.ARGB_8888);
		onRender(new Canvas(newCache));

		final ReferenceCountedBitmap newCachedView = new ReferenceCountedBitmap(newCache);
		newCachedView.lock();

		if(shouldCacheView && this.width == width) {
			final ReferenceCountedBitmap oldCachedView = cachedView.getAndSet(newCachedView);
			if(oldCachedView != null) oldCachedView.release();
		}

		return newCachedView;
	}
}
