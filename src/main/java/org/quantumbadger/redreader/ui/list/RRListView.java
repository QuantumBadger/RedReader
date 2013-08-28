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

import android.graphics.Canvas;
import android.os.SystemClock;
import org.quantumbadger.redreader.common.RRSchedulerManager;
import org.quantumbadger.redreader.common.UnexpectedInternalStateException;
import org.quantumbadger.redreader.ui.frag.RRFragmentContext;
import org.quantumbadger.redreader.ui.views.RRView;
import org.quantumbadger.redreader.ui.views.RRViewParent;
import org.quantumbadger.redreader.ui.views.touch.RRClickHandler;
import org.quantumbadger.redreader.ui.views.touch.RRHSwipeHandler;
import org.quantumbadger.redreader.ui.views.touch.RRSingleTouchViewWrapper;
import org.quantumbadger.redreader.ui.views.touch.RRVSwipeHandler;

// TODO require that all operations are performed in UI thread, to avoid synchronization/volatility
public final class RRListView extends RRSingleTouchViewWrapper implements RRViewParent, RRVSwipeHandler {

	private final RRListViewContents contents = new RRListViewContents(this);
	private volatile RRListViewFlattenedContents flattenedContents;

	private volatile int width, height;

	private volatile int firstVisibleItemPos = 0, lastVisibleItemPos = -1, pxInFirstVisibleItem = 0;

	private boolean isPaused = true, isCacheEnabled = false, isMeasured = false;

	private volatile RRListViewCacheBlockRing cacheRing = null;
	private int pxInFirstCacheBlock = 0;

	private final RRSchedulerManager.RRSingleTaskScheduler cacheEnableTimer;
	private final Runnable cacheEnableRunnable = new Runnable() {
		public void run() {
			enableCache();
		}
	};

	private float velocity = 0;
	private static final float minVelocity = 1;

	private enum SHMLockType { UNLOCKED, LOCKED_TOP, LOCKED_BOTTOM, LOCKED_NONE }
	private SHMLockType shmLock = SHMLockType.UNLOCKED;

	public RRListView(RRFragmentContext context) {
		super(context);
		cacheEnableTimer = context.scheduler.obtain();
		setWillNotDraw(false);
	}

	public synchronized void clearCacheRing() {

		brieflyDisableCache();

		if(cacheRing != null) {
			cacheRing.onPause();
			cacheRing.recycle();
			cacheRing = null;
		}
	}

	public void onChildAppended() {

		flattenedContents = contents.getFlattenedContents();
		if(flattenedContents.itemCount - 2 == lastVisibleItemPos) {
			brieflyDisableCache();
			recalculateLastVisibleItem();
			postInvalidate();
			// TODO account for new cache manager
		}
	}

	public void onChildInserted() {
		// TODO
		throw new UnsupportedOperationException();
	}

	public void onChildrenRecomputed() {
		// TODO invalidate cache. If previous top child is now invisible, go to the next one visible one after it in the list
		brieflyDisableCache();
		flattenedContents = contents.getFlattenedContents();
		recalculateLastVisibleItem();
		postInvalidate();
	}

	public RRListViewContents getContents() {
		return contents;
	}

	private synchronized void enableCache() {

		if(!isMeasured) throw new UnexpectedInternalStateException();

		cacheEnableTimer.cancel();
		isCacheEnabled = true;

		if(cacheRing == null) {
			cacheRing = new RRListViewCacheBlockRing(width, height / 2, 5);
		}

		pxInFirstCacheBlock = 0;
		cacheRing.assign(flattenedContents, firstVisibleItemPos, pxInFirstVisibleItem);
		cacheRing.onResume();

		postInvalidate();
	}

	private synchronized void brieflyDisableCache() {
		disableCache();
		cacheEnableTimer.setSchedule(cacheEnableRunnable, 250);
	}

	public void rrStartAnimation(RRView child) {
		// TODO detect if child is on screen
		brieflyDisableCache();
	}

	private synchronized void disableCache() {
		cacheEnableTimer.cancel();
		isCacheEnabled = false;
		if(cacheRing != null) cacheRing.onPause();
		postInvalidate();
	}

	@Override
	protected synchronized void onMeasure(int widthMeasureSpec, int heightMeasureSpec) {

		width = MeasureSpec.getSize(widthMeasureSpec);
		height = MeasureSpec.getSize(heightMeasureSpec);
		setMeasuredDimension(width, height);
		isMeasured = true;

		recalculateLastVisibleItem();

		clearCacheRing();
	}

	public synchronized void scrollBy(float px) {
		pxInFirstVisibleItem += px;
		pxInFirstCacheBlock += px;
		recalculateLastVisibleItem();
	}

	public synchronized void recalculateLastVisibleItem() {

		if(!isMeasured) return;

		final RRListViewItem[] items = flattenedContents.items;

		final int width = this.width;

		while(pxInFirstVisibleItem < 0 && firstVisibleItemPos > 0) {
			pxInFirstVisibleItem += items[--firstVisibleItemPos].setWidth(width);
		}

		while(pxInFirstVisibleItem >= items[firstVisibleItemPos].setWidth(width)
				&& firstVisibleItemPos < flattenedContents.itemCount - 1) {
			pxInFirstVisibleItem -= items[firstVisibleItemPos++].getOuterHeight();
		}

		if(false && isCacheEnabled && cacheRing != null) {
			while(pxInFirstCacheBlock < 0) {
				pxInFirstCacheBlock += cacheRing.blockHeight;
				cacheRing.moveBackward();
			}

			while(pxInFirstCacheBlock >= cacheRing.blockHeight) {
				pxInFirstCacheBlock -= cacheRing.blockHeight;
				cacheRing.moveForward();
			}
		}

		int pos = items[firstVisibleItemPos].setWidth(width) - pxInFirstVisibleItem;
		int lastVisibleItemPos = firstVisibleItemPos;

		while(pos <= height && lastVisibleItemPos < flattenedContents.itemCount - 1) {
			lastVisibleItemPos++;
			pos += items[lastVisibleItemPos].setWidth(width);
		}

		this.lastVisibleItemPos = lastVisibleItemPos;
	}


	public synchronized void onResume() {
		if(!isPaused) throw new UnexpectedInternalStateException();
		isPaused = false;
		if(isMeasured) brieflyDisableCache();
	}

	public synchronized void onPause() {
		if(isPaused) throw new UnexpectedInternalStateException();
		isPaused = true;
		cacheEnableTimer.cancel();
		disableCache();
	}

	private int calculatePxBeforeListStart() {

		if(firstVisibleItemPos == 0) {
			return -pxInFirstVisibleItem;
		}

		int totalHeight = pxInFirstVisibleItem;
		for(int i = 0; i < firstVisibleItemPos; i++) totalHeight += flattenedContents.items[i].setWidth(width);
		return -totalHeight;
	}

	// TODO where the full list height is shorter than the listview, everything will snap to the bottom
	private int calculatePxAfterListEnd() {

		int totalHeight = -pxInFirstVisibleItem;
		for(int i = firstVisibleItemPos; i < flattenedContents.itemCount; i++) {
			totalHeight += flattenedContents.items[i].setWidth(width);
		}

		return height - totalHeight;
	}

	@Override
	protected void onDraw(Canvas canvas) {

		if(flattenedContents == null) return;

		long uptimeMillis = -1;
		boolean invalidate = false, brieflyDisableCache = false;

		switch(shmLock) {
			case UNLOCKED:
				if(firstVisibleItemPos == 0 && pxInFirstVisibleItem < 0) shmLock = SHMLockType.LOCKED_TOP;
				else if(lastVisibleItemPos == flattenedContents.itemCount - 1) {
					if(calculatePxAfterListEnd() > 0) shmLock = SHMLockType.LOCKED_BOTTOM;
				}
				break;

			case LOCKED_BOTTOM: {
				velocity *= 0.92;
				final float acceleration = -calculatePxAfterListEnd() * 25;
				velocity += acceleration / 60f; // TODO take into account elapsed time...
				invalidate = true;
				break;
			}

			case LOCKED_TOP: {
				velocity *= 0.92;
				final float acceleration = calculatePxBeforeListStart() * 25;
				velocity += acceleration / 60f; // TODO take into account elapsed time...
				invalidate = true;
				break;
			}
		}

		if(Math.abs(velocity) > minVelocity) {

			uptimeMillis = SystemClock.uptimeMillis();

			scrollBy(velocity / 60f); // TODO detect time elapsed since last draw
			velocity *= 0.975;
			velocity += 0.05 * (velocity > 0 ? -1 : 1); // TODO take into account dpi
			invalidate = true;
		} else {
			velocity = 0;
		}

		final RRListViewFlattenedContents fc = flattenedContents;

		for(int i = firstVisibleItemPos; i <= lastVisibleItemPos; i++) {
			if(fc.items[i].isAnimating()) {
				if(uptimeMillis == -1) uptimeMillis = SystemClock.uptimeMillis();
				if(fc.items[i].rrUpdateAnimation(uptimeMillis)) {
					brieflyDisableCache = true;
				}
			}
		}

		if(!isCacheEnabled || cacheRing == null) {

			canvas.save();

			canvas.translate(0, -pxInFirstVisibleItem);

			for(int i = firstVisibleItemPos; i <= lastVisibleItemPos; i++) {
				final int drawnHeight = fc.items[i].setWidthAndDraw(canvas, width);
				canvas.translate(0, drawnHeight);
			}

			canvas.restore();

		} else {
			if(!cacheRing.draw(canvas, height, -pxInFirstCacheBlock)) invalidate = true;
		}

		if(brieflyDisableCache) brieflyDisableCache();
		else if(invalidate) invalidate();
	}

	public void rrInvalidate(RRView child) {
		// TODO detect if child is on screen/in cache
		brieflyDisableCache();
		//postInvalidate();
	}

	public void rrRequestLayout(RRView child) {
		// TODO detect if child is on screen/in cache
		brieflyDisableCache();
		//postInvalidate();
	}

	public RRClickHandler getClickHandler(int x, int y) {

		if(Math.abs(velocity) > minVelocity) {
			velocity = 0;
			return null;  // TODO only if faster than certain value
		}

		final RRListViewFlattenedContents fc = flattenedContents;
		int totalHeight = -pxInFirstVisibleItem;
		int lastHeight = totalHeight;

		for(int i = firstVisibleItemPos; i <= lastVisibleItemPos; i++) {
			totalHeight += fc.items[i].getOuterHeight();
			if(totalHeight > y) return fc.items[i].getClickHandler(x, y - lastHeight);
			lastHeight = totalHeight;
		}

		return null;
	}

	public RRHSwipeHandler getHSwipeHandler(int x, int y) {

		final RRListViewFlattenedContents fc = flattenedContents;
		int totalHeight = -pxInFirstVisibleItem;
		int lastHeight = totalHeight;

		for(int i = firstVisibleItemPos; i <= lastVisibleItemPos; i++) {
			totalHeight += fc.items[i].getOuterHeight();
			if(totalHeight > y) return fc.items[i].getHSwipeHandler(x, y - lastHeight);
			lastHeight = totalHeight;
		}

		return null;
	}

	public RRVSwipeHandler getVSwipeHandler(int x, int y) {
		return this;
	}

	public void onVSwipeBegin(long timestamp) {
	}

	public void onVSwipeDelta(long timestamp, float dy) {
		scrollBy(-dy);
		invalidate();
	}

	public void onVSwipeEnd(long timestamp, float yVelocity) {
		velocity = -yVelocity;
		invalidate();
	}

	public RRListViewItem getItemAt(final int yCoord) {

		final RRListViewFlattenedContents fc = flattenedContents;
		int totalHeight = -pxInFirstVisibleItem;

		for(int i = firstVisibleItemPos; i <= lastVisibleItemPos; i++) {
			totalHeight += fc.items[i].getOuterHeight();
			if(totalHeight > yCoord) return fc.items[i];
		}

		return null;
	}
}
