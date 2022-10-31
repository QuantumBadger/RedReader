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

import android.graphics.Bitmap;

public class MultiScaleTileManager {

	public static final int MAX_SAMPLE_SIZE = 32;

	private final ImageViewTileLoader[] mTileLoaders;

	private int mDesiredScaleIndex = -1;

	private final Object mLock = new Object();

	public static int scaleIndexToSampleSize(int scaleIndex) {
		return 1 << scaleIndex;
	}

	public static int sampleSizeToScaleIndex(int sampleSize) {
		return Integer.numberOfTrailingZeros(sampleSize);
	}

	public MultiScaleTileManager(
			final ImageTileSource imageTileSource,
			final ImageViewTileLoaderThread thread,
			final int x,
			final int y,
			final ImageViewTileLoader.Listener listener) {

		mTileLoaders = new ImageViewTileLoader[sampleSizeToScaleIndex(MAX_SAMPLE_SIZE) + 1];

		for(int s = 0; s < mTileLoaders.length; s++) {
			mTileLoaders[s] = new ImageViewTileLoader(imageTileSource, thread, x, y, scaleIndexToSampleSize(s), listener, mLock);
		}
	}

	public Bitmap getAtDesiredScale() {
		return mTileLoaders[mDesiredScaleIndex].get();
	}

	public void markAsWanted(int desiredScaleIndex) {

		if(desiredScaleIndex == mDesiredScaleIndex) {
			return;
		}

		mDesiredScaleIndex = desiredScaleIndex;

		synchronized(mLock) {

			mTileLoaders[desiredScaleIndex].markAsWanted();

			for(int s = 0; s < mTileLoaders.length; s++) {
				if(s != desiredScaleIndex) {
					mTileLoaders[s].markAsUnwanted();
				}
			}
		}
	}

	public void markAsUnwanted() {

		if(mDesiredScaleIndex == -1) {
			return;
		}

		mDesiredScaleIndex = -1;

		synchronized(mLock) {
			for(int s = 0; s < mTileLoaders.length; s++) {
				mTileLoaders[s].markAsUnwanted();
			}
		}
	}
}
