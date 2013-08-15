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
import org.quantumbadger.redreader.ui.RRFragmentContext;
import org.quantumbadger.redreader.ui.views.RRViewParent;

import java.util.Timer;
import java.util.TimerTask;

public final class RRListView extends View implements RRViewParent {

	private final RRListViewContents contents = new RRListViewContents(this);
	private volatile RRListViewFlattenedContents flattenedContents;

	private volatile int width, height;

	private int firstVisibleItemPos = 0, lastVisibleItemPos = -1;
	private int pxInFirstVisibleItem = 0;

	private int oldWidth = -1;

	private volatile boolean isPaused = true;

	private Timer cacheEnableTimer;
	private volatile boolean cacheEnabled = false;
	private final TimerTask cacheEnableTask = new TimerTask() {
		public void run() {
			enableCache();
		}
	};

	public RRListView(RRFragmentContext context) {
		super(context.activity);
		setWillNotDraw(false);
	}

	public void onChildAppended() {

		flattenedContents = contents.getFlattenedContents();
		if(flattenedContents.itemCount - 2 == lastVisibleItemPos) {
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
		flattenedContents = contents.getFlattenedContents();
		recalculateLastVisibleItem();
		postInvalidate();
	}

	public RRListViewContents getContents() {
		return contents;
	}

	private void enableCache() {
		cacheEnabled = true;
	}

	private void brieflyDisableCache() {
		// TODO replace timer with thread, destroy only in pause()
		cacheEnableTimer.cancel();
		cacheEnableTimer.schedule(cacheEnableTask, 250);
		disableCache();
	}

	private void disableCache() {
		cacheEnabled = false;
		// NOTE do not delete the cache blocks
	}

	@Override
	protected void onMeasure(int widthMeasureSpec, int heightMeasureSpec) {

		brieflyDisableCache();

		// TODO delete all cache blocks, create a new set

		width = MeasureSpec.getSize(widthMeasureSpec);
		height = MeasureSpec.getSize(heightMeasureSpec);

		setMeasuredDimension(width, height);

		if(width != oldWidth) {
			recalculateLastVisibleItem();
			oldWidth = width;
		}
	}

	public void scrollBy(int px) {
		pxInFirstVisibleItem += px;
		recalculateLastVisibleItem();
	}

	public void recalculateLastVisibleItem() {

		final RRListViewItem[] items = flattenedContents.items;

		while(pxInFirstVisibleItem < 0) {

			if(firstVisibleItemPos == 0) {
				pxInFirstVisibleItem = 0;
			} else {
				firstVisibleItemPos--;
				pxInFirstVisibleItem += items[firstVisibleItemPos].getHeight();
			}
		}

		while(pxInFirstVisibleItem >= items[firstVisibleItemPos].getHeight()) {
			pxInFirstVisibleItem -= items[firstVisibleItemPos].getHeight();
			firstVisibleItemPos++;
		}

		final int width = this.width;

		int pos = (int) (items[firstVisibleItemPos].setWidth(width) - pxInFirstVisibleItem);
		int lastVisibleItemPos = firstVisibleItemPos;

		while(pos <= height && lastVisibleItemPos < flattenedContents.itemCount - 1) {
			lastVisibleItemPos++;
			pos += items[lastVisibleItemPos].setWidth(width);
		}

		this.lastVisibleItemPos = lastVisibleItemPos;

		// TODO cacheManager.update(fc, firstVisibleItemPos, lastVisibleItemPos, 10, thread);
	}

	private int downId = -1;
	private int lastYPos = -1;

	@Override
	public boolean onTouchEvent(MotionEvent ev) {

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

	public void resume() {
		cacheEnableTimer = new Timer("Cache Enable Timer");
		isPaused = false;
	}

	public void pause() {
		cacheEnableTimer.cancel();
		cacheEnableTimer = null;
		isPaused = true;
	}

	@Override
	protected void onDraw(Canvas canvas) {

		if(flattenedContents == null) return;

		final RRListViewFlattenedContents fc = flattenedContents;

		if(!cacheEnabled) {

			canvas.translate(0, -pxInFirstVisibleItem);

			for(int i = firstVisibleItemPos; i <= lastVisibleItemPos; i++) {
				fc.items[i].draw(canvas, width);
				canvas.translate(0, fc.items[i].getHeight());
			}
		} else {
			// TODO
		}
	}

	public void rrInvalidate() {
		brieflyDisableCache();
		postInvalidate();
	}

	public void rrRequestLayout() {
		// TODO completely flush and rebuild cache manager
	}

	private final class CacheThread extends Thread {



		@Override
		public void run() {
			while(cacheEnabled) {

			}
		}
	}
}
