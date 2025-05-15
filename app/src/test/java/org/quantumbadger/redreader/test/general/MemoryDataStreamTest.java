package org.quantumbadger.redreader.test.general;

import org.junit.Assert;
import org.junit.Test;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.datastream.MemoryDataStream;

import java.io.IOException;

public class MemoryDataStreamTest {

	private void assertBytesStartsWith(final byte[] actual, final char... expected) {

		for(int i = 0; i < expected.length; i++) {
			Assert.assertEquals(expected[i], actual[i]);
		}
	}

	@Test
	public void test() throws IOException {

		final MemoryDataStream stream = new MemoryDataStream();

		Assert.assertEquals(0, stream.size());

		stream.writeBytes(new byte[] {'H', 'i'}, 0, 2);

		final byte[] buf = new byte[10];
		Assert.assertEquals(
				2,
				stream.blockingRead(0, buf, 0, buf.length));

		Assert.assertEquals('H', buf[0]);
		Assert.assertEquals('i', buf[1]);

		assertBytesStartsWith(buf, 'H', 'i');

		stream.writeBytes(new byte[] {'A'}, 0, 1);
		stream.writeBytes(new byte[] {'Z'}, 0, 1);

		Assert.assertEquals(
				4,
				stream.blockingRead(0, buf, 0, buf.length));

		assertBytesStartsWith(buf, 'H', 'i', 'A', 'Z');

		Assert.assertEquals(
				3,
				stream.blockingRead(1, buf, 0, buf.length));

		assertBytesStartsWith(buf, 'i', 'A', 'Z');

		stream.setComplete();

		Assert.assertEquals(
				"HiAZ",
				General.readWholeStreamAsUTF8(stream.getInputStream()));
	}
}
