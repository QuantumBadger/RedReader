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
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.InterruptableThread;
import org.quantumbadger.redreader.common.RRSchedulerManager;
import org.quantumbadger.redreader.common.UnexpectedInternalStateException;
import org.quantumbadger.redreader.ui.frag.RRFragmentContext;
import org.quantumbadger.redreader.ui.frag.RRViewWrapper;
import org.quantumbadger.redreader.ui.views.RRViewParent;

import java.util.concurrent.atomic.AtomicInteger;

public final class RRListView extends RRViewWrapper implements RRViewParent {

	private final RRListViewContents contents = new RRListViewContents(this);
	private volatile RRListViewFlattenedContents flattenedContents;

	private volatile int width, height;

	private int firstVisibleItemPos = 0, lastVisibleItemPos = -1, pxInFirstVisibleItem = 0;

	private volatile boolean isPaused = true, isMeasured = false;

	private RRListViewCacheBlockRing cacheRing;
	private int pxInFirstCacheBlock = 0;

	private final RRSchedulerManager.RRSingleTaskScheduler cacheEnableTimer;
	private final Runnable cacheEnableRunnable = new Runnable() {
		public void run() {
			enableCache();
		}
	};

	private volatile CacheThread cacheThread = null;

	public RRListView(RRFragmentContext context) {
		super(context.activity);
		cacheEnableTimer = context.scheduler.obtain();
		setWillNotDraw(false);
	}

	public synchronized void clearCacheRing() {

		brieflyDisableCache();

		if(cacheRing != null) {
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

		if(cacheRing == null) {
			cacheRing = new RRListViewCacheBlockRing(width, height / 2, 5);
		}

		pxInFirstCacheBlock = 0;
		cacheRing.assign(flattenedContents, firstVisibleItemPos, pxInFirstVisibleItem);

		if(cacheThread == null) {
			cacheThread = new CacheThread(cacheRing);
			cacheThread.start();
		}

		postInvalidate();
	}

	private synchronized void brieflyDisableCache() {
		disableCache();
		cacheEnableTimer.setSchedule(cacheEnableRunnable, 250);
	}

	private synchronized void disableCache() {

		cacheEnableTimer.cancel();

		if(cacheThread != null) {
			cacheThread.stop();
			cacheThread = null;
		}

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

	public synchronized void scrollBy(int px) {
		pxInFirstVisibleItem += px;
		pxInFirstCacheBlock += px;
		recalculateLastVisibleItem();
	}

	public synchronized void recalculateLastVisibleItem() {

		if(!isMeasured) return;

		final RRListViewItem[] items = flattenedContents.items;

		while(pxInFirstVisibleItem < 0 && firstVisibleItemPos > 0) {
			pxInFirstVisibleItem += items[--firstVisibleItemPos].setWidth(width);
		}

		while(pxInFirstVisibleItem >= items[firstVisibleItemPos].setWidth(width)
				&& firstVisibleItemPos < flattenedContents.itemCount - 1) {
			pxInFirstVisibleItem -= items[firstVisibleItemPos++].getOuterHeight();
		}

		if(cacheThread != null) {
			while(pxInFirstCacheBlock < 0) {
				pxInFirstCacheBlock += cacheRing.blockHeight;
				cacheThread.requestMoveBackward();
			}

			while(pxInFirstCacheBlock >= cacheRing.blockHeight) {
				pxInFirstCacheBlock -= cacheRing.blockHeight;
				cacheThread.requestMoveForward();
			}
		}

		final int width = this.width;

		int pos = items[firstVisibleItemPos].setWidth(width) - pxInFirstVisibleItem;
		int lastVisibleItemPos = firstVisibleItemPos;

		while(pos <= height && lastVisibleItemPos < flattenedContents.itemCount - 1) {
			lastVisibleItemPos++;
			pos += items[lastVisibleItemPos].setWidth(width);
		}

		this.lastVisibleItemPos = lastVisibleItemPos;
	}

	private int lastYPos = -1;

	@Override
	protected void onTouchEvent(TouchEvent e) {

		if(e.pointerCount != 1 || e.isFollowingPointerUp) {
			// TODO disallow tap and hover events - allow only swipes
		}

		// TODO don't allow use of mean pos when horizontal swiping, tapping, etc
		// (although tapping can only be done with one finger anyway, with !followingPointerUp)
		final float meanYPos = e.pointerCount > 1 ? (float)General.mean(e.yPos) : e.yPos[0];

		switch(e.type) {

			case MOVE:
				scrollBy(Math.round(lastYPos - meanYPos));
				invalidate();

			case BEGIN:
				lastYPos = Math.round(meanYPos);
				break;

			case CANCEL:
			case FINISH:
				break;
		}
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

	@Override
	protected void onDraw(Canvas canvas) {

		if(flattenedContents == null) return;

		final RRListViewFlattenedContents fc = flattenedContents;

		if(cacheThread == null) {

			canvas.save();

			canvas.translate(0, -pxInFirstVisibleItem);

			for(int i = firstVisibleItemPos; i <= lastVisibleItemPos; i++) {
				fc.items[i].draw(canvas, width);
				canvas.translate(0, fc.items[i].getOuterHeight());
			}

			canvas.restore();

		} else {
			if(!cacheRing.draw(canvas, height, -pxInFirstCacheBlock)) invalidate();
		}
	}

	public void rrInvalidate() {
		brieflyDisableCache();
		postInvalidate();
	}

	public void rrRequestLayout() {
		// TODO completely flush and rebuild cache manager
	}

	private final class CacheThread extends InterruptableThread {

		private final RRListViewCacheBlockRing localCacheRing;
		private final AtomicInteger ringAdvancesNeeded = new AtomicInteger(0);

		private CacheThread(RRListViewCacheBlockRing cacheRing) {
			super("CacheThread");
			this.localCacheRing = cacheRing;
		}

		public void requestMoveForward() {
			synchronized(ringAdvancesNeeded) {
				ringAdvancesNeeded.incrementAndGet();
				ringAdvancesNeeded.notifyAll();
			}
		}

		public void requestMoveBackward() {
			synchronized(ringAdvancesNeeded) {
				ringAdvancesNeeded.decrementAndGet();
				ringAdvancesNeeded.notifyAll();
			}
		}

		@Override
		public void run() throws InterruptedException {

			while(cacheThread == this) {

				int value = 0;

				synchronized(ringAdvancesNeeded) {

					while(cacheThread == this && (value = ringAdvancesNeeded.get()) == 0) {
						ringAdvancesNeeded.wait();
					}
				}

				if(value == 0) return;

				if(value > 0) {
					localCacheRing.moveForward();
					ringAdvancesNeeded.decrementAndGet();
				} else {
					localCacheRing.moveBackward();
					ringAdvancesNeeded.incrementAndGet();
				}
			}
		}
	}
}
