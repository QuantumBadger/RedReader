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

public class BoundsHelper {

	private final int mResolutionX, mResolutionY;
	private final int mImageResolutionX, mImageResolutionY;
	private final CoordinateHelper mCoordinateHelper;

	private final float mMinScale;

	public BoundsHelper(
			int resolutionX, int resolutionY,
			int imageResolutionX, int imageResolutionY,
			CoordinateHelper coordinateHelper) {

		mResolutionX = resolutionX;
		mResolutionY = resolutionY;
		mImageResolutionX = imageResolutionX;
		mImageResolutionY = imageResolutionY;
		mCoordinateHelper = coordinateHelper;

		mMinScale = Math.min(
				(float) mResolutionX / (float) mImageResolutionX,
				(float) mResolutionY / (float) mImageResolutionY
		);
	}

	public void applyMinScale() {
		mCoordinateHelper.setScale(mMinScale);
	}

	public boolean isMinScale() {
		return mCoordinateHelper.getScale() - 0.000001f <= mMinScale;
	}

	public void applyBounds() {

		if(mCoordinateHelper.getScale() < mMinScale) {
			applyMinScale();
		}

		final float scale = mCoordinateHelper.getScale();
		final MutableFloatPoint2D posOffset = mCoordinateHelper.getPositionOffset();

		final float scaledImageWidth = (float)mImageResolutionX * scale;
		final float scaledImageHeight = (float)mImageResolutionY * scale;

		if(scaledImageWidth <= mResolutionX) {
			posOffset.x = (mResolutionX - scaledImageWidth) / 2;

		} else if(posOffset.x > 0) {
			posOffset.x = 0;
		} else if(posOffset.x < mResolutionX - scaledImageWidth) {
			posOffset.x = mResolutionX - scaledImageWidth;
		}

		if(scaledImageHeight <= mResolutionY) {
			posOffset.y = (mResolutionY - scaledImageHeight) / 2;

		} else if(posOffset.y > 0) {
			posOffset.y = 0;
		} else if(posOffset.y < mResolutionY - scaledImageHeight) {
			posOffset.y = mResolutionY - scaledImageHeight;
		}
	}

	public float getMinScale() {
		return mMinScale;
	}
}
