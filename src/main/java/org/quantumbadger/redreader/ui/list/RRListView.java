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

import java.util.LinkedList;

public final class RRListView extends View {

	private final RRListViewContents contents = new RRListViewContents(this);
	private volatile RRListViewContents.RRListViewFlattenedContents flattenedContents;

	private final RRListViewCacheManager cacheManager = new RRListViewCacheManager();

	private volatile int width, height;

	private int firstVisibleItemPos = 0, lastVisibleItemPos = -1;
	private float positionInFirstVisibleItem = 0;

	private int oldWidth = -1;

	private volatile RenderThread thread;
	private volatile boolean isPaused = true;

	public RRListView(RRFragmentContext context) {
		super(context.activity);
		setWillNotDraw(false);
	}

	public void onChildAppended() {

		flattenedContents = contents.getFlattenedContents();
		if(flattenedContents.itemCount - 2 == lastVisibleItemPos) {
			recalculateLastVisibleItem();
			postInvalidate();
		}
	}

	public void onChildInserted() {
		// TODO invalidate
		throw new UnsupportedOperationException();
	}

	public void onChildrenRecomputed() {
		// TODO invalidate. If previous top child is now invisible, go to the next one visible one after it in the list
		flattenedContents = contents.getFlattenedContents();
		recalculateLastVisibleItem();
		postInvalidate();
	}

	public RRListViewContents getContents() {
		return contents;
	}

	@Override
	protected void onMeasure(int widthMeasureSpec, int heightMeasureSpec) {
		width = MeasureSpec.getSize(widthMeasureSpec);
		height = MeasureSpec.getSize(heightMeasureSpec);

		setMeasuredDimension(width, height);

		if(width != oldWidth) {
			recalculateLastVisibleItem();
			oldWidth = width;
		}
	}

	public void scrollBy(float px) {
		scrollByInner(px);
		recalculateLastVisibleItem();
	}

	private void scrollByInner(float px) {

		if(firstVisibleItemPos < 0) {
			firstVisibleItemPos = 0;
			positionInFirstVisibleItem = 0;
			return;
		}

		// TODO height may not have been calculated?
		// TODO replace recursion with iteration
		// TODO items as local variable

		if(px >= 0) {

			final float pxRemainingInTopItem = flattenedContents.items[firstVisibleItemPos].height * (1 - positionInFirstVisibleItem);

			if(px < pxRemainingInTopItem) {
				positionInFirstVisibleItem += px / flattenedContents.items[firstVisibleItemPos].height;

			} else {
				firstVisibleItemPos++;
				positionInFirstVisibleItem = 0;
				scrollByInner(px - pxRemainingInTopItem);
			}

		} else {

			final float pxRemainingInTopItem = flattenedContents.items[firstVisibleItemPos].height * positionInFirstVisibleItem;

			if(-px < pxRemainingInTopItem) {
				positionInFirstVisibleItem += px / flattenedContents.items[firstVisibleItemPos].height;

			} else {
				firstVisibleItemPos--;
				positionInFirstVisibleItem = 1f;
				scrollByInner(px + pxRemainingInTopItem);
			}
		}
	}

	public void recalculateLastVisibleItem() {

		final int width = this.width;

		final RRListViewContents.RRListViewFlattenedContents fc = flattenedContents;
		int pos = (int) (fc.items[firstVisibleItemPos].measureHeight(width) * (1 - positionInFirstVisibleItem));
		int lastVisibleItemPos = firstVisibleItemPos;

		while(pos <= height && lastVisibleItemPos < fc.itemCount - 1) {
			lastVisibleItemPos++;
			pos += fc.items[lastVisibleItemPos].measureHeight(width);
		}

		this.lastVisibleItemPos = lastVisibleItemPos;

		cacheManager.update(fc, firstVisibleItemPos, lastVisibleItemPos, 10, thread);
	}

	private int downId = -1;
	private float lastYPos = -1;

	@Override
	public boolean onTouchEvent(MotionEvent ev) {

		final int action = ev.getAction() & MotionEvent.ACTION_MASK;

		if(action == MotionEvent.ACTION_DOWN) {

			lastYPos = ev.getY();
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

			final float yDelta = ev.getY() - lastYPos;
			lastYPos = ev.getY();

			scrollBy(-yDelta);
			invalidate();

			return true;

		} else return false;
	}

	public void resume() {
		isPaused = false;
		thread = new RenderThread(); // TODO this is unsafe - two threads may run at once
		thread.start();
	}

	public void pause() {
		isPaused = true;
		thread.interrupt();
	}

	@Override
	protected void onDraw(Canvas canvas) {

		if(flattenedContents == null) return;

		final RRListViewContents.RRListViewFlattenedContents fc = flattenedContents;

		canvas.translate(0, (int)(fc.items[firstVisibleItemPos].height * -positionInFirstVisibleItem)); // TODO height may not have been calculated here...

		for(int i = firstVisibleItemPos; i <= lastVisibleItemPos; i++) {
			fc.items[i].draw(canvas, width);
			canvas.translate(0, fc.items[i].height);
		}
	}

	// TODO low priority
	protected final class RenderThread extends Thread {

		// TODO optimise
		private final LinkedList<RRListViewItem> toRender = new LinkedList<RRListViewItem>();

		public void add(RRListViewItem item) {
			synchronized(toRender) {
				toRender.add(item);
				toRender.notify();
			}
		}

		@Override
		public void run() {

			synchronized(toRender) {
				while(!isPaused) {

					if(toRender.isEmpty()) {
						try {
							toRender.wait();
						} catch(InterruptedException e) {}
					}

					if(!toRender.isEmpty()) {
						toRender.removeFirst().doCacheRender(width, false);
					}
				}
			}
		}
	}
}
