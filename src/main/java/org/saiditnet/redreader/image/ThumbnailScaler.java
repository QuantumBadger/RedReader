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

package org.saiditnet.redreader.image;

import android.graphics.Bitmap;

public final class ThumbnailScaler {

	private static final float maxHeightWidthRatio = 3.0f;

	private static Bitmap scaleAndCrop(final Bitmap src, final int w, final int h, final int newWidth) {

		final float scaleFactor = (float)newWidth / (float)w;
		final Bitmap scaled = Bitmap.createScaledBitmap(src, Math.round(scaleFactor * src.getWidth()), Math.round(scaleFactor * src.getHeight()), true);

		final Bitmap result = Bitmap.createBitmap(scaled, 0, 0, newWidth, Math.round((float)h * scaleFactor));

		if(result != scaled) scaled.recycle();

		return result;
	}

	public static Bitmap scale(final Bitmap image, final int width) {

		final float heightWidthRatio = (float)image.getHeight() / (float)image.getWidth();

		if(heightWidthRatio >= 1.0f && heightWidthRatio <= maxHeightWidthRatio) {

			// Use as-is.
			return Bitmap.createScaledBitmap(image, width, Math.round(heightWidthRatio * width), true);

		} else if(heightWidthRatio < 1.0f) {

			// Wide image. Crop horizontally.
			return scaleAndCrop(image, image.getHeight(), image.getHeight(), width);

		} else {

			// Tall image.
			return scaleAndCrop(image, image.getWidth(), Math.round(image.getWidth() * maxHeightWidthRatio), width);
		}
	}

	public static Bitmap scaleNoCrop(final Bitmap image, final int desiredSquareSizePx) {

		final int currentSquareSizePx = Math.max(image.getWidth(), image.getHeight());

		final float scale = (float)desiredSquareSizePx / (float)currentSquareSizePx;

		return Bitmap.createScaledBitmap(image, Math.round(scale * image.getWidth()), Math.round(scale * image.getHeight()), true);
	}
}
