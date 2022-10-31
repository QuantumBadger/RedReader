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

package org.saiditnet.redreader.views.imageview;

import org.saiditnet.redreader.common.MutableFloatPoint2D;

public class ImageViewScaleAnimation {

	private final float mStepSize;
	private final float mTargetScale;
	private final CoordinateHelper mCoordinateHelper;
	private final MutableFloatPoint2D mScreenCoord = new MutableFloatPoint2D();

	public ImageViewScaleAnimation(float targetScale, CoordinateHelper coordinateHelper, int stepCount, MutableFloatPoint2D screenCoord) {

		mTargetScale = targetScale;
		mCoordinateHelper = coordinateHelper;
		mStepSize = (float)Math.pow((targetScale / coordinateHelper.getScale()), (1.0 / (double)stepCount));
		mScreenCoord.set(screenCoord);
	}

	public boolean onStep() {

		mCoordinateHelper.scaleAboutScreenPoint(mScreenCoord, mStepSize);

		if(mStepSize > 1) {
			if(mTargetScale <= mCoordinateHelper.getScale()) {
				mCoordinateHelper.setScaleAboutScreenPoint(mScreenCoord, mTargetScale);
				return false;
			}

		} else {
			if(mTargetScale >= mCoordinateHelper.getScale()) {
				mCoordinateHelper.setScaleAboutScreenPoint(mScreenCoord, mTargetScale);
				return false;
			}
		}

		return true;
	}
}
