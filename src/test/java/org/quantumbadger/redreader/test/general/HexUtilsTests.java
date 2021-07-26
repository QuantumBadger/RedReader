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

import org.junit.Assert;
import org.junit.Test;
import org.quantumbadger.redreader.common.HexUtils;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.Locale;

public class HexUtilsTests {

	@Test
	public void hexTestChars() throws IOException {

		for(int i = 0; i < 16; i++) {
			Assert.assertEquals(i, HexUtils.fromHex(String.format(Locale.US, "%X", i).charAt(0)));
		}

		for(int i = 0; i < 16; i++) {
			Assert.assertEquals(i, HexUtils.fromHex(String.format(Locale.US, "%x", i).charAt(0)));
		}
	}

	@Test
	public void hexTest1() throws IOException {

		final byte[] msg = "Hello World".getBytes(StandardCharsets.UTF_8);

		final String hexMsg = HexUtils.toHex(msg);

		Assert.assertEquals("48656C6C6F20576F726C64", hexMsg);

		Assert.assertArrayEquals(msg, HexUtils.fromHex(hexMsg));
		Assert.assertArrayEquals(msg, HexUtils.fromHex(hexMsg.toLowerCase()));
	}

	@Test
	public void hexTest2() {
		Assert.assertThrows(IOException.class, () -> HexUtils.fromHex("123"));
	}

	@Test
	public void hexTest3() {
		Assert.assertThrows(IOException.class, () -> HexUtils.fromHex("48656C6C6F20576F726CR4"));
	}
}
