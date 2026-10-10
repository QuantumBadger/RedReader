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

import static org.junit.Assert.assertEquals;

import org.junit.Test;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.datastream.MemoryDataStream;

import java.io.IOException;

public class MemoryDataStreamGrowthPlanTest {

	private static void write(final MemoryDataStream stream, final int count) {
		final byte[] data = new byte[count];
		for(int i = 0; i < count; i++) {
			data[i] = (byte)('a' + (i % 26));
		}
		stream.writeBytes(data, 0, count);
	}

	@Test
	public void noPlanDoublesAsBefore() {
		final MemoryDataStream stream = new MemoryDataStream(4);
		write(stream, 5);
		assertEquals(8, stream.getCapacity());
	}

	@Test
	public void firstResizeJumpsToPlannedSize() {
		final MemoryDataStream stream = new MemoryDataStream(4, new int[] {64});
		write(stream, 5);
		assertEquals(64, stream.getCapacity());
	}

	@Test
	public void afterPlanIsExhaustedDoublingResumes() {
		final MemoryDataStream stream = new MemoryDataStream(4, new int[] {64});
		write(stream, 5);
		write(stream, 60);
		assertEquals(128, stream.getCapacity());
	}

	@Test
	public void plannedSizeTooSmallForWriteIsSkipped() {
		// A single 100 byte write into a 4 byte buffer with a 16 byte plan: the plan is
		// not enough, so the usual rule for oversize writes applies (1.5x the required)
		final MemoryDataStream stream = new MemoryDataStream(4, new int[] {16});
		write(stream, 100);
		assertEquals(150, stream.getCapacity());
	}

	@Test
	public void plannedSizeNotLargerThanCurrentIsSkipped() {
		final MemoryDataStream stream = new MemoryDataStream(32, new int[] {16, 256});
		write(stream, 40);
		assertEquals(256, stream.getCapacity());
	}

	@Test
	public void multiStepPlanIsFollowedInOrder() {
		final MemoryDataStream stream = new MemoryDataStream(4, new int[] {16, 64, 256});
		write(stream, 10);
		assertEquals(16, stream.getCapacity());
		write(stream, 10);
		assertEquals(64, stream.getCapacity());
		write(stream, 100);
		assertEquals(256, stream.getCapacity());
		write(stream, 200);
		assertEquals(512, stream.getCapacity());
	}

	@Test
	public void dataSurvivesPlannedResizes() throws IOException {
		final MemoryDataStream stream = new MemoryDataStream(2, new int[] {8});
		stream.writeBytes(new byte[] {'H', 'e', 'l'}, 0, 3);
		stream.writeBytes(new byte[] {'l', 'o', '!', '!', '!', '!', '!'}, 0, 7);
		stream.setComplete();
		assertEquals(
				"Hello!!!!!",
				General.readWholeStreamAsUTF8(stream.getInputStream()));
	}
}
