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
import org.quantumbadger.redreader.ui.views.RRView;

import java.util.concurrent.atomic.AtomicInteger;

public abstract class RRListViewItem extends RRView {

	private static final AtomicInteger maxItemId = new AtomicInteger(0);
	public final int globalItemId = maxItemId.incrementAndGet();

	private float xVel = 0, xPos = 0; // TODO account for dpi, fps

	protected abstract void onRender(Canvas c);

	@Override
	public final int hashCode() {
		return globalItemId;
	}

	public abstract boolean isVisible();
}
