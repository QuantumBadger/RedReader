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
import android.view.MotionEvent;
import android.view.View;
import org.quantumbadger.redreader.common.RRSchedulerManager;
import org.quantumbadger.redreader.ui.frag.RRFragmentContext;
import org.quantumbadger.redreader.ui.views.RRViewParent;

import java.util.concurrent.atomic.AtomicInteger;

public final class RRListView extends View implements RRViewParent {

	private final RRListViewContents contents = new RRListViewContents(this);
	private volatile RRListViewFlattenedContents flattenedContents;

	private volatile int width, height;

	private int firstVisibleItemPos = 0, lastVisibleItemPos = -1;
	private int pxInFirstVisibleItem = 0;

	private int oldWidth = -1;

	private volatile boolean isPaused = true;

	private volatile RRListViewCacheBlockRing cacheRing;
	private volatile int pxInFirstCacheBlock = 0;

	private final RRSchedulerManager.RRSingleTaskScheduler cacheEnableTimer;
	private volatile boolean cacheEnabled = false;

	private final Runnable cacheEnableRunnable = new Runnable() {
		public void run() {
			enableCache();
		}
	};

	private final AtomicInteger ringAdvancesNeeded = new AtomicInteger(0);

	private CacheThread cacheThread;

	private boolean isMeasured = false, enableCacheAfterMeasuring = true;

	public RRListView(RRFragmentContext context) {
		super(context.activity);
		cacheEnableTimer = context.scheduler.obtain();
		setWillNotDraw(false);
	}

	public synchronized void clearCacheRing() {
		if(cacheRing != null) {
			// TODO recycle
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

		if(!isMeasured || cacheEnabled) return;
		cacheEnableTimer.cancel();

		if(cacheRing == null) {
			cacheRing = new RRListViewCacheBlockRing(width, height / 2, 4);
		}

		cacheRing.assign(flattenedContents, firstVisibleItemPos, pxInFirstVisibleItem);

		pxInFirstCacheBlock = 0;
		cacheEnabled = true;

		cacheThread = new CacheThread();
		cacheThread.start();

		postInvalidate();
	}

	private synchronized void brieflyDisableCache() {
		// TODO replace timer with thread, destroy only in pause()
		if(!isPaused) {
			disableCache();
			cacheEnableTimer.setSchedule(cacheEnableRunnable, 250);
		}
	}

	private synchronized void disableCache() {
		cacheEnabled = false;
		cacheEnableTimer.cancel();

		if(cacheThread != null) cacheThread.interrupt();
		// NOTE do not delete the cache blocks

		postInvalidate();
	}

	@Override
	protected synchronized void onMeasure(int widthMeasureSpec, int heightMeasureSpec) {

		brieflyDisableCache();
		clearCacheRing();

		width = MeasureSpec.getSize(widthMeasureSpec);
		height = MeasureSpec.getSize(heightMeasureSpec);

		setMeasuredDimension(width, height);

		isMeasured = true;

		if(width != oldWidth) {
			recalculateLastVisibleItem();
			oldWidth = width;
		}

		if(enableCacheAfterMeasuring) {
			enableCache();
			enableCacheAfterMeasuring = false;
		}
	}

	public synchronized void scrollBy(int px) {
		pxInFirstVisibleItem += px;
		pxInFirstCacheBlock += px;
		recalculateLastVisibleItem();
	}

	public synchronized void recalculateLastVisibleItem() {

		if(!isMeasured) return;

		final RRListViewItem[] items = flattenedContents.items;

		while(pxInFirstVisibleItem < 0) {

			if(firstVisibleItemPos == 0) {
				pxInFirstVisibleItem = 0;
			} else {
				firstVisibleItemPos--;
				pxInFirstVisibleItem += items[firstVisibleItemPos].setWidth(width);
			}
		}

		while(firstVisibleItemPos < flattenedContents.itemCount - 1 && pxInFirstVisibleItem >= items[firstVisibleItemPos].setWidth(width)) {
			pxInFirstVisibleItem -= items[firstVisibleItemPos].getOuterHeight();
			firstVisibleItemPos++;
		}

		while(pxInFirstCacheBlock < 0) {
			pxInFirstCacheBlock += height / 2;

			synchronized(ringAdvancesNeeded) {
				ringAdvancesNeeded.decrementAndGet();
				ringAdvancesNeeded.notifyAll();
			}
		}

		while(pxInFirstCacheBlock >= height / 2) {
			pxInFirstCacheBlock -= height / 2;

			synchronized(ringAdvancesNeeded) {
				ringAdvancesNeeded.incrementAndGet();
				ringAdvancesNeeded.notifyAll();
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

	private int downId = -1;
	private int lastYPos = -1;

	@Override
	public boolean onTouchEvent(final MotionEvent ev) {

		final int action = ev.getAction() & MotionEvent.ACTION_MASK;

		if(action == MotionEvent.ACTION_DOWN) {

			lastYPos = Math.round(ev.getY());
			downId = ev.getPointerId(0);
			return true;

		} else if(action == MotionEvent.ACTION_UP
				|| action == MotionEvent.ACTION_OUTSIDE
				|| action == MotionEvent.ACTION_CANCEL
				|| action == MotionEvent.ACTION_POINTER_UP) {

			if(ev.getPointerId(ev.getActionIndex()) != downId) return false;
			downId = -1;

			return false;

		} else if(action == MotionEvent.ACTION_MOVE) {

			if(ev.getPointerId(ev.getActionIndex()) != downId) return false;

			final int yDelta = Math.round(ev.getY() - lastYPos);
			lastYPos = Math.round(ev.getY());

			scrollBy(-yDelta);
			invalidate();

			return true;

		} else return false;
	}

	public synchronized void resume() {
		isPaused = false;
		brieflyDisableCache();
		enableCacheAfterMeasuring = true;
	}

	public synchronized void pause() {
		isPaused = true;
		cacheEnableTimer.cancel();
		disableCache();
	}

	@Override
	protected void onDraw(Canvas canvas) {

		if(flattenedContents == null) return;

		final RRListViewFlattenedContents fc = flattenedContents;

		canvas.save();

		if(!cacheEnabled) {

			canvas.translate(0, -pxInFirstVisibleItem);

			for(int i = firstVisibleItemPos; i <= lastVisibleItemPos; i++) {
				fc.items[i].draw(canvas, width);
				canvas.translate(0, fc.items[i].getOuterHeight());
			}
		} else {
			canvas.translate(0, -pxInFirstCacheBlock);
			if(!cacheRing.draw(canvas)) invalidate();
		}

		canvas.restore();
	}

	public void rrInvalidate() {
		brieflyDisableCache();
		postInvalidate();
	}

	public void rrRequestLayout() {
		// TODO completely flush and rebuild cache manager
	}

	private final class CacheThread extends Thread {

		private final RRListViewCacheBlockRing localCacheRing = cacheRing;

		@Override
		public void run() {
			while(cacheEnabled && cacheRing == localCacheRing) {

				final boolean forward;

				synchronized(ringAdvancesNeeded) {

					int value = 0;
					while(cacheEnabled && cacheRing == localCacheRing && (value = ringAdvancesNeeded.get()) == 0) {
						try {
							ringAdvancesNeeded.wait();
						} catch(InterruptedException e) {
							return;
						}
					}

					if(value == 0) return;

					if(value > 0) {
						forward = true;
						ringAdvancesNeeded.decrementAndGet();
					} else {
						forward = false;
						ringAdvancesNeeded.incrementAndGet();
					}
				}

				if(forward) {
					localCacheRing.moveForward();
				} else {
					localCacheRing.moveBackward();
				}
			}
		}
	}
}
