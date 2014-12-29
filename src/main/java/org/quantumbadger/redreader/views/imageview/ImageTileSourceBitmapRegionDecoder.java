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

import android.annotation.TargetApi;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.BitmapRegionDecoder;
import android.graphics.Rect;
import android.os.Build;
import org.quantumbadger.redreader.common.General;

import java.io.IOException;

@TargetApi(Build.VERSION_CODES.GINGERBREAD_MR1)
public class ImageTileSourceBitmapRegionDecoder implements ImageTileSource {

	private final BitmapRegionDecoder mBitmapRegionDecoder;
	private final int mWidth, mHeight;

	private static final int TILE_SIZE = 256;

	public ImageTileSourceBitmapRegionDecoder(final byte[] data) throws IOException {
		mBitmapRegionDecoder = BitmapRegionDecoder.newInstance(data, 0, data.length, false);
		mWidth = mBitmapRegionDecoder.getWidth();
		mHeight = mBitmapRegionDecoder.getHeight();
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
		return General.divideCeil(mWidth, TILE_SIZE);
	}

	@Override
	public int getVTileCount() {
		return General.divideCeil(mHeight, TILE_SIZE);
	}

	@Override
	public Bitmap getTile(final int sampleSize, final int tileX, final int tileY) {

		BitmapFactory.Options options = new BitmapFactory.Options();
		options.inSampleSize = sampleSize;

		return mBitmapRegionDecoder.decodeRegion(new Rect(
				tileX * TILE_SIZE,
				tileY * TILE_SIZE,
				(tileX + 1) * TILE_SIZE,
				(tileY + 1) * TILE_SIZE), options);
	}

	@Override
	public void dispose() {}
}
