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

package org.quantumbadger.redreader.image;

import android.graphics.Bitmap;
import android.graphics.BitmapFactory;

import androidx.annotation.NonNull;

import org.quantumbadger.redreader.common.datastream.SeekableInputStream;

import java.io.IOException;

public final class ScaledBitmapDecoder {

	private ScaledBitmapDecoder() {}

	/**
	 * Decodes the supplied image, scaled down so that it fits within the given bounds. The
	 * aspect ratio is preserved, and the image is never scaled up.
	 *
	 * <p>The scaling is done in two stages, to avoid the visible artifacts which result from
	 * reducing an image by a large factor in a single step.
	 *
	 * <p>First, the image is decoded using {@link BitmapFactory.Options#inSampleSize}. This
	 * is cheap, and avoids ever holding the image in memory at its full size, but it only
	 * supports powers of two, and (other than for JPEGs, where the decoder can scale during
	 * the DCT) it simply discards pixels. We therefore use the largest power of two which
	 * still leaves the image at least as large as the target size.
	 *
	 * <p>Second, the result is scaled to the exact target size using a bilinear filter. As
	 * the first stage guarantees this is a reduction of less than a factor of two, every
	 * source pixel falls within the filter's sample window, so no detail is discarded.
	 */
	@NonNull
	public static Bitmap decodeToFitWithin(
			@NonNull final SeekableInputStream input,
			final int maxWidthPx,
			final int maxHeightPx) throws IOException {

		if(maxWidthPx < 1 || maxHeightPx < 1) {
			throw new IllegalArgumentException("Invalid bounds " + maxWidthPx
					+ "x"
					+ maxHeightPx);
		}

		final BitmapFactory.Options boundsOptions = new BitmapFactory.Options();
		boundsOptions.inJustDecodeBounds = true;

		input.seek(0);
		input.mark(0);
		BitmapFactory.decodeStream(input, null, boundsOptions);

		final int srcWidth = boundsOptions.outWidth;
		final int srcHeight = boundsOptions.outHeight;

		if(srcWidth < 1 || srcHeight < 1) {
			throw new IOException("Failed to read image bounds");
		}

		// The factor by which the image must be scaled in order to fit within the bounds
		final double scale = Math.min(
				1.0,
				Math.min(
						(double)maxWidthPx / (double)srcWidth,
						(double)maxHeightPx / (double)srcHeight));

		int sampleSize = 1;

		while(scale * (double)(sampleSize * 2) <= 1.0) {
			sampleSize *= 2;
		}

		final BitmapFactory.Options decodeOptions = new BitmapFactory.Options();
		decodeOptions.inSampleSize = sampleSize;

		input.seek(0);
		input.mark(0);

		final Bitmap decoded = BitmapFactory.decodeStream(input, null, decodeOptions);

		if(decoded == null) {
			throw new IOException("Failed to decode bitmap");
		}

		final int targetWidth = Math.max(1, (int)Math.round((double)srcWidth * scale));
		final int targetHeight = Math.max(1, (int)Math.round((double)srcHeight * scale));

		if(decoded.getWidth() == targetWidth && decoded.getHeight() == targetHeight) {
			return decoded;
		}

		final Bitmap scaled = Bitmap.createScaledBitmap(
				decoded,
				targetWidth,
				targetHeight,
				true);

		if(scaled != decoded) {
			decoded.recycle();
		}

		return scaled;
	}
}
