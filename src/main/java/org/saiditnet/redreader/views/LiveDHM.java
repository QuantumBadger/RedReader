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

public class LiveDHM {

	public static class Params {

		public float startPosition = 0;
		public float endPosition = 0;

		public float startVelocity = 0;

		public float accelerationCoefficient = 30;
		public float velocityDamping = 0.87f;

		public float stepLengthSeconds = 1f / 60f;

		public float thresholdPositionDifference = 0.49f;
		public float thresholdVelocity = 15;
		public int thresholdMaxSteps = 1000;
	}

	private final Params mParams;

	private int mStep = 0;

	private float mPosition;
	private float mVelocity;

	public LiveDHM(final Params params) {
		mParams = params;
		mPosition = params.startPosition;
		mVelocity = params.startVelocity;
	}

	public void calculateStep() {
		mVelocity -= mParams.stepLengthSeconds * ((mPosition - mParams.endPosition) * mParams.accelerationCoefficient);
		mVelocity *= mParams.velocityDamping;
		mPosition += mVelocity * mParams.stepLengthSeconds;
		mStep++;
	}

	public int getCurrentStep() {
		return mStep;
	}

	public float getCurrentPosition() {
		return mPosition;
	}

	public float getCurrentVelocity() {
		return mVelocity;
	}

	public Params getParams() {
		return mParams;
	}

	public boolean isEndThresholdReached() {

		if(mStep >= mParams.thresholdMaxSteps) {
			return true;
		}

		if(Math.abs(mPosition) > mParams.thresholdPositionDifference) {
			return false;
		}

		if(Math.abs(mVelocity) > mParams.thresholdVelocity) {
			return false;
		}

		return true;
	}
}
