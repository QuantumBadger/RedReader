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
import org.quantumbadger.redreader.common.StringUtils;

import java.util.Locale;

import static org.junit.Assert.assertEquals;

public class GeneralTest {

	@Test
	public void testAsciiUppercase() {

		for(char c = 0; c < 128; c++) {
			{
				final String str = "This is a test" + new String(new char[]{c});
				assertEquals(str.toUpperCase(Locale.ENGLISH), StringUtils.asciiUppercase(str));
			}

			{
				final String str = "" + c + c + c + c + c + "A" + c + "A";
				assertEquals(str.toUpperCase(Locale.ENGLISH), StringUtils.asciiUppercase(str));
			}
		}
	}
}
