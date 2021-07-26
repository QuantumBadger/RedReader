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

package org.quantumbadger.redreader.views.imageview;

import android.graphics.Bitmap;
import android.graphics.Matrix;
import org.quantumbadger.redreader.common.General;

public class ImageTileSourceWholeBitmap implements ImageTileSource {

	private final Bitmap mBitmap;

	private final int mWidth;
	private final int mHeight;

	private static final int TILE_SIZE = 512;

	public ImageTileSourceWholeBitmap(final Bitmap bitmap) {
		mBitmap = bitmap;
		mWidth = bitmap.getWidth();
		mHeight = bitmap.getHeight();
	}

	@Override
	public int getWidth() {
		return mWidth;
	}

	@Override
	public int getHeight() {
		return mHeight;
	}

	@Override
	public int getTileSize() {
		return TILE_SIZE;
	}

	@Override
	public int getHTileCount() {
		return General.divideCeil(getWidth(), TILE_SIZE);
	}

	@Override
	public int getVTileCount() {
		return General.divideCeil(getHeight(), TILE_SIZE);
	}

	@Override
	public Bitmap getTile(final int sampleSize, final int tileX, final int tileY) {

		if(sampleSize == 1 && TILE_SIZE >= mWidth && TILE_SIZE >= mHeight) {
			return mBitmap;
		}

		final int tileStartX = tileX * TILE_SIZE;
		final int tileStartY = tileY * TILE_SIZE;
		final int tileEndX = Math.min(mWidth, (tileX + 1) * TILE_SIZE);
		final int tileEndY = Math.min(mHeight, (tileY + 1) * TILE_SIZE);

		final int inputTileWidthPx = tileEndX - tileStartX;
		final int inputTileHeightPx = tileEndY - tileStartY;

		if(sampleSize == 1) {
			return Bitmap.createBitmap(
					mBitmap,
					tileStartX,
					tileStartY,
					inputTileWidthPx,
					inputTileHeightPx);
		}

		final Matrix scaleMatrix = new Matrix();
		scaleMatrix.setScale(1.0f / sampleSize, 1.0f / sampleSize);

		return Bitmap.createBitmap(
				mBitmap,
				tileStartX,
				tileStartY,
				inputTileWidthPx,
				inputTileHeightPx,
				scaleMatrix,
				true);
	}

	@Override
	public void dispose() {
		// Nothing to do here
	}
}
