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

package org.saiditnet.redreader.views;

public abstract class RRDHMAnimation extends RRAnimation {

	private final LiveDHM mDHM;

	public RRDHMAnimation(final LiveDHM.Params params) {
		mDHM = new LiveDHM(params);
	}

	@Override
	protected boolean handleFrame(final long nanosSinceAnimationStart) {

		final long microsSinceAnimationStart = nanosSinceAnimationStart / 1000;
		final long stepLengthMicros = (long)(mDHM.getParams().stepLengthSeconds * 1000.0 * 1000.0);

		final int desiredStepNumber = (int)((microsSinceAnimationStart + (stepLengthMicros / 2)) / stepLengthMicros);

		while(mDHM.getCurrentStep() < desiredStepNumber) {

			mDHM.calculateStep();

			if(mDHM.isEndThresholdReached()) {
				onEndPosition(mDHM.getParams().endPosition);
				return false;
			}
		}

		onUpdatedPosition(mDHM.getCurrentPosition());
		return true;
	}

	public final float getCurrentVelocity() {
		return mDHM.getCurrentVelocity();
	}

	protected abstract void onUpdatedPosition(float position);

	protected abstract void onEndPosition(float endPosition);
}
