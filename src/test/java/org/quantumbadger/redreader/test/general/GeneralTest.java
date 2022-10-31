package org.saiditnet.redreader.test.general;

import org.junit.Test;
import org.saiditnet.redreader.common.General;

import java.util.Locale;

import static org.junit.Assert.assertEquals;

public class GeneralTest {

	@Test
	public void testAsciiUppercase() {

		for(char c = 0; c < 128; c++) {
			{
				final String str = "This is a test" + new String(new char[]{c});
				assertEquals(str.toUpperCase(Locale.ENGLISH), General.asciiUppercase(str));
			}

			{
				String str = "" + c + c + c + c + c + "A" + c + "A";
				assertEquals(str.toUpperCase(Locale.ENGLISH), General.asciiUppercase(str));
			}
		}
	}
}
