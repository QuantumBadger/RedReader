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

import java.util.concurrent.atomic.AtomicInteger;

public abstract class RRListViewItem {

	private static final AtomicInteger maxItemId = new AtomicInteger(0);
	public final int globalItemId = maxItemId.incrementAndGet();

	protected int width = -1;
	public int height = -1;

	private RRListView parent;

	private float xVel = 0, xPos = 0; // TODO account for dpi, fps

	protected synchronized final int setWidth(int width) {

		if(width == this.width) return height;

		this.width = width;
		height = onMeasureHeight(width);

		// TODO invalidate

		return height;
	}

	protected abstract int onMeasureHeight(int width);

	public final void setParent(RRListView parent) {
		this.parent = parent;
	}

	public final void draw(Canvas c, int width) {
		if(width != this.width) setWidth(width);
		onRender(c);
	}

	protected abstract void onRender(Canvas c);

	@Override
	public final int hashCode() {
		return globalItemId;
	}

	public abstract boolean isVisible();

	public final void invalidate() {

		// TODO notify parent that this view is invalidated
		// TODO have separate "relayout" method
		// TODO extend RRView
	}
}
