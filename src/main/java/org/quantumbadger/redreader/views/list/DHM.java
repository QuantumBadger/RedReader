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

package org.quantumbadger.redreader.views.list;

import java.util.ArrayList;

public class DHM {

	private final float startPos, startVelocity, accCoefficient, stepLengthSeconds;

	private final ArrayList<Float> results = new ArrayList<>();

	public DHM(final float startPos, final float startVelocity, final float accCoefficient, final float stepLengthSeconds) {
		this.startPos = startPos;
		this.startVelocity = startVelocity;
		this.accCoefficient = accCoefficient;
		this.stepLengthSeconds = stepLengthSeconds;
	}

	// Returns number of steps
	public int calculate(final float xThreshold, final float vThreshold) {

		float x = startPos;
		float v = startVelocity;

		int step = 0;

		while((Math.abs(x) >= xThreshold  || Math.abs(v) >= vThreshold) && step < 1000) {
			v -= stepLengthSeconds * (x * accCoefficient);
			v *= 0.87; // TODO parameterise
			x += v * stepLengthSeconds;
			results.add(x);
			step++;
		}

		return step;
	}

	public float getPositionAt(final int step) {
		if(step >= results.size()) return 0;
		return results.get(step);
	}
}
