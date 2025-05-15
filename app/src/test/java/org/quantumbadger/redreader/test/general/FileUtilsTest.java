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

import org.junit.Test;
import org.quantumbadger.redreader.common.FileUtils;
import org.quantumbadger.redreader.common.Optional;

import static org.junit.Assert.assertEquals;

public class FileUtilsTest {

	@Test
	public void testGetExtensionFromPath() {

		assertEquals(
				Optional.of("jpg"),
				FileUtils.getExtensionFromPath("image.jpg"));

		assertEquals(
				Optional.of("mp4"),
				FileUtils.getExtensionFromPath("path/segments/image.other.mp4"));

		assertEquals(
				Optional.of("jpg"),
				FileUtils.getExtensionFromPath("other.image.jpg"));

		assertEquals(
				Optional.empty(),
				FileUtils.getExtensionFromPath("image"));

		assertEquals(
				Optional.empty(),
				FileUtils.getExtensionFromPath("path/segments.test/image"));

		assertEquals(
				Optional.of("bmp"),
				FileUtils.getExtensionFromPath("path/segments.test/image.bmp"));

		assertEquals(
				Optional.empty(),
				FileUtils.getExtensionFromPath("path/segments.test/image."));

		assertEquals(
				Optional.empty(),
				FileUtils.getExtensionFromPath("path/segments.test/.image"));

		assertEquals(
				Optional.empty(),
				FileUtils.getExtensionFromPath("path/segments.test/.image."));

		assertEquals(
				Optional.of("jpg"),
				FileUtils.getExtensionFromPath("path/segments.test/.image.jpg"));

	}
}
