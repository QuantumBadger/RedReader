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

package org.quantumbadger.redreader.test.general;

import android.graphics.Bitmap;
import android.graphics.Color;

import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.quantumbadger.redreader.common.datastream.MemoryDataStream;
import org.quantumbadger.redreader.common.datastream.SeekableInputStream;
import org.quantumbadger.redreader.image.ScaledBitmapDecoder;
import org.robolectric.RobolectricTestRunner;

import java.io.ByteArrayOutputStream;
import java.io.IOException;

@RunWith(RobolectricTestRunner.class)
public class ScaledBitmapDecoderTest {

	private static SeekableInputStream createPng(
			final int width,
			final int height,
			final int color) {

		final Bitmap bitmap = Bitmap.createBitmap(width, height, Bitmap.Config.ARGB_8888);
		bitmap.eraseColor(color);

		final ByteArrayOutputStream output = new ByteArrayOutputStream();
		Assert.assertTrue(bitmap.compress(Bitmap.CompressFormat.PNG, 100, output));
		bitmap.recycle();

		return new MemoryDataStream(output.toByteArray()).getInputStream();
	}

	private static Bitmap decode(
			final int srcWidth,
			final int srcHeight,
			final int maxWidth,
			final int maxHeight) throws IOException {

		return ScaledBitmapDecoder.decodeToFitWithin(
				createPng(srcWidth, srcHeight, Color.RED),
				maxWidth,
				maxHeight);
	}

	private static void assertSize(
			final int expectedWidth,
			final int expectedHeight,
			final Bitmap actual) {

		Assert.assertEquals(
				expectedWidth + "x" + expectedHeight,
				actual.getWidth() + "x" + actual.getHeight());
	}

	@Test
	public void testScaleDownToExactSize() throws IOException {
		// The largest power of two reduction lands on 125x125, which is then filtered
		// down to the exact size requested
		assertSize(100, 100, decode(1000, 1000, 100, 100));
	}

	@Test
	public void testAspectRatioPreserved() throws IOException {
		assertSize(100, 50, decode(1000, 500, 100, 100));
		assertSize(50, 100, decode(500, 1000, 100, 100));
	}

	@Test
	public void testExactPowerOfTwoReduction() throws IOException {
		// No filtering pass should be needed here
		assertSize(512, 512, decode(1024, 1024, 512, 512));
	}

	@Test
	public void testTallImageIsLimitedByHeight() throws IOException {
		// A tall image displayed in a box two thirds of the screen height: it's the
		// height, not the width, which determines the size
		assertSize(576, 1600, decode(1080, 3000, 1080, 1600));
	}

	@Test
	public void testImagesAreNeverScaledUp() throws IOException {
		assertSize(50, 50, decode(50, 50, 1000, 1000));
		assertSize(1080, 810, decode(1080, 810, 1080, 1600));
	}

	@Test
	public void testContentIsPreserved() throws IOException {

		final Bitmap result = ScaledBitmapDecoder.decodeToFitWithin(
				createPng(800, 800, Color.GREEN),
				200,
				200);

		assertSize(200, 200, result);
		Assert.assertEquals(Color.GREEN, result.getPixel(100, 100));
	}

	@Test(expected = IllegalArgumentException.class)
	public void testInvalidBoundsRejected() throws IOException {
		decode(100, 100, 0, 100);
	}
}
