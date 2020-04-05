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
import android.graphics.BitmapFactory;
import android.util.Log;
import org.quantumbadger.redreader.common.General;

import java.io.IOException;

public class ImageTileSourceWholeBitmap implements ImageTileSource {

	private final byte[] mData;
	private Bitmap mBitmap = null;

	private final int mWidth, mHeight;

	private static final int TILE_SIZE = 512;

	public ImageTileSourceWholeBitmap(final byte[] data) throws IOException {

		mData = data;

		BitmapFactory.Options opts = new BitmapFactory.Options();
		opts.inJustDecodeBounds = true;

		final Bitmap bitmap = BitmapFactory.decodeByteArray(data, 0, data.length);
		mWidth = bitmap.getWidth();
		mHeight = bitmap.getHeight();
		bitmap.recycle();
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
	public synchronized Bitmap getTile(final int sampleSize, final int tileX, final int tileY) {

		if(mBitmap == null) {
			Log.i("ITSWholeBitmap", "Loading bitmap.");
			mBitmap = BitmapFactory.decodeByteArray(mData, 0, mData.length);
		}

		final int tileStartX = tileX * TILE_SIZE;
		final int tileStartY = tileY * TILE_SIZE;
		final int tileEndX = Math.min(mWidth, (tileX + 1) * TILE_SIZE);
		final int tileEndY = Math.min(mHeight, (tileY + 1) * TILE_SIZE);

		final int inputTileWidthPx = tileEndX - tileStartX;
		final int inputTileHeightPx = tileEndY - tileStartY;

		final int outputTileWidth = inputTileWidthPx / sampleSize;
		final int outputTileHeight = inputTileHeightPx / sampleSize;

		// TODO unnecessary extra step
		final Bitmap region = Bitmap.createBitmap(
				mBitmap,
				tileStartX,
				tileStartY,
				inputTileWidthPx,
				inputTileHeightPx);

		return Bitmap.createScaledBitmap(region, outputTileWidth, outputTileHeight, true);
	}

	@Override
	public synchronized void dispose() {

		if(mBitmap != null && !mBitmap.isRecycled()) {
			mBitmap.recycle();
		}

		mBitmap = null;
	}
}
