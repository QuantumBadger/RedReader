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

package org.quantumbadger.redreader.common;

import android.view.MotionEvent;

public class MutableFloatPoint2D {

	public float x;
	public float y;

	public void reset() {
		x = 0;
		y = 0;
	}

	public void set(final MotionEvent event, final int pointerIndex) {
		x = event.getX(pointerIndex);
		y = event.getY(pointerIndex);
	}

	public void set(final MutableFloatPoint2D other) {
		x = other.x;
		y = other.y;
	}

	public void set(final float x, final float y) {
		this.x = x;
		this.y = y;
	}

	public void add(final MutableFloatPoint2D rhs, final MutableFloatPoint2D result) {
		result.x = x + rhs.x;
		result.y = y + rhs.y;
	}

	public void sub(final MutableFloatPoint2D rhs, final MutableFloatPoint2D result) {
		result.x = x - rhs.x;
		result.y = y - rhs.y;
	}

	public void add(final MutableFloatPoint2D rhs) {
		add(rhs, this);
	}

	public void sub(final MutableFloatPoint2D rhs) {
		sub(rhs, this);
	}

	public void scale(final double factor) {
		x *= factor;
		y *= factor;
	}

	public double euclideanDistanceTo(final MutableFloatPoint2D other) {
		final float xDistance = x - other.x;
		final float yDistance = y - other.y;
		return Math.sqrt(xDistance * xDistance + yDistance * yDistance);
	}

	public float distanceSquared() {
		return x * x + y * y;
	}
}
