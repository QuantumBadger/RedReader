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

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;

import org.junit.Test;
import org.quantumbadger.redreader.cache.CacheDownload;
import org.quantumbadger.redreader.common.Constants;

public class CacheDownloadBufferSizeTest {

	private static final int DEFAULT = 64 * 1024;

	@Test
	public void usesReportedLengthExactly() {
		assertEquals(8_187_319, CacheDownload.chooseInitialBufferSize(
				8_187_319L,
				Constants.FileType.IMAGE));
		assertEquals(1, CacheDownload.chooseInitialBufferSize(
				1L,
				Constants.FileType.COMMENT_LIST));
	}

	@Test
	public void reportedLengthTakesPriorityOverTypeDefault() {
		assertEquals(1234, CacheDownload.chooseInitialBufferSize(
				1234L,
				Constants.FileType.COMMENT_LIST));
	}

	@Test
	public void unknownLengthUsesPerTypeDefault() {
		assertEquals(128 * 1024, CacheDownload.chooseInitialBufferSize(
				null,
				Constants.FileType.COMMENT_LIST));
		assertEquals(256 * 1024, CacheDownload.chooseInitialBufferSize(
				null,
				Constants.FileType.POST_LIST));
		assertEquals(256 * 1024, CacheDownload.chooseInitialBufferSize(
				null,
				Constants.FileType.INLINE_IMAGE_PREVIEW));
		assertEquals(2 * 1024 * 1024, CacheDownload.chooseInitialBufferSize(
				null,
				Constants.FileType.IMAGE));
	}

	@Test
	public void unknownLengthFallsBackToGlobalDefaultForOtherTypes() {
		assertEquals(DEFAULT, CacheDownload.chooseInitialBufferSize(
				null,
				Constants.FileType.THUMBNAIL));
		assertEquals(DEFAULT, CacheDownload.chooseInitialBufferSize(
				null,
				Constants.FileType.SUBREDDIT_ABOUT));
		assertEquals(DEFAULT, CacheDownload.chooseInitialBufferSize(
				null,
				Constants.FileType.NOCACHE));
	}

	@Test
	public void invalidLengthIsTreatedAsUnknown() {
		assertEquals(128 * 1024, CacheDownload.chooseInitialBufferSize(
				0L,
				Constants.FileType.COMMENT_LIST));
		assertEquals(128 * 1024, CacheDownload.chooseInitialBufferSize(
				-1L,
				Constants.FileType.COMMENT_LIST));
		assertEquals(2 * 1024 * 1024, CacheDownload.chooseInitialBufferSize(
				Long.MAX_VALUE,
				Constants.FileType.IMAGE));
		assertEquals(DEFAULT, CacheDownload.chooseInitialBufferSize(
				(256L * 1024 * 1024) + 1,
				Constants.FileType.THUMBNAIL));
	}

	@Test
	public void acceptsLengthAtUpperBound() {
		assertEquals(256 * 1024 * 1024, CacheDownload.chooseInitialBufferSize(
				256L * 1024 * 1024,
				Constants.FileType.IMAGE));
	}

	@Test
	public void commentListsWithUnknownLengthJumpTo600kOnFirstResize() {
		assertArrayEquals(
				new int[] {600 * 1024},
				CacheDownload.chooseGrowthPlan(null, Constants.FileType.COMMENT_LIST));
		assertArrayEquals(
				new int[] {600 * 1024},
				CacheDownload.chooseGrowthPlan(0L, Constants.FileType.COMMENT_LIST));
	}

	@Test
	public void knownLengthHasNoGrowthPlan() {
		assertArrayEquals(
				new int[0],
				CacheDownload.chooseGrowthPlan(400_000L, Constants.FileType.COMMENT_LIST));
	}

	@Test
	public void otherTypesHaveNoGrowthPlan() {
		assertArrayEquals(
				new int[0],
				CacheDownload.chooseGrowthPlan(null, Constants.FileType.POST_LIST));
		assertArrayEquals(
				new int[0],
				CacheDownload.chooseGrowthPlan(null, Constants.FileType.IMAGE));
		assertArrayEquals(
				new int[0],
				CacheDownload.chooseGrowthPlan(null, Constants.FileType.THUMBNAIL));
	}
}
