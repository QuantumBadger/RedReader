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
import android.graphics.Canvas;
import android.graphics.Rect;
import org.quantumbadger.redreader.common.General;

import java.io.IOException;

public class ImageTileSourceWholeBitmap implements ImageTileSource {

	private Bitmap mBitmap;

	private static final int TILE_SIZE = 512;

	public ImageTileSourceWholeBitmap(final byte[] data) throws IOException {
		mBitmap = BitmapFactory.decodeByteArray(data, 0, data.length);
	}

	@Override
	public int getWidth() {
		return mBitmap.getWidth();
	}

	@Override
	public int getHeight() {
		return mBitmap.getHeight();
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

		final int tileStartX = tileX * TILE_SIZE;
		final int tileStartY = tileY * TILE_SIZE;
		final int tileEndX = (tileX + 1) * TILE_SIZE;
		final int tileEndY = (tileY + 1) * TILE_SIZE;

		final int outputTileSize = TILE_SIZE / sampleSize;

		if(tileEndX <= getWidth() && tileEndY <= getHeight()) {

			final Bitmap region = Bitmap.createBitmap(
					mBitmap,
					tileStartX,
					tileStartY,
					tileEndX - tileStartX,
					tileEndY - tileStartY);

			return Bitmap.createScaledBitmap(region, outputTileSize, outputTileSize, true);

		} else {

			final Bitmap tile = Bitmap.createBitmap(outputTileSize, outputTileSize, Bitmap.Config.ARGB_8888);
			final Canvas canvas = new Canvas(tile);

			final int tileLimitedEndX = Math.min(tileEndX, getWidth());
			final int tileLimitedEndY = Math.min(tileEndY, getHeight());

			final Rect srcRect = new Rect(
					tileStartX,
					tileStartY,
					tileLimitedEndX,
					tileLimitedEndY
			);

			final int srcTileWidth = tileLimitedEndX - tileStartX;
			final int srcTileHeight = tileLimitedEndY - tileStartY;

			final Rect dstRect = new Rect(
					0,
					0,
					srcTileWidth / sampleSize,
					srcTileHeight / sampleSize
			);

			canvas.drawBitmap(mBitmap, srcRect, dstRect, null);

			return tile;
		}
	}
}
