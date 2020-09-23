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
import org.quantumbadger.redreader.common.Optional;
import org.quantumbadger.redreader.common.StringUtils;

import java.util.Arrays;
import java.util.Collections;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class StringUtilsTest {

	@Test
	public void testRemovePrefix() {

		assertEquals(Optional.empty(), StringUtils.removePrefix("abc", "def"));
		assertEquals(Optional.of("def"), StringUtils.removePrefix("abcdef", "abc"));
		assertEquals(Optional.of("abcdef"), StringUtils.removePrefix("abcdef", ""));
		assertEquals(Optional.of(""), StringUtils.removePrefix("123", "123"));
	}

	@Test
	public void testAsciiUppercase() {

		assertEquals("ABC123", StringUtils.asciiUppercase("abc123"));
		assertEquals("åBC123", StringUtils.asciiUppercase("åbc123"));
		assertEquals("", StringUtils.asciiUppercase(""));
	}

	@Test
	public void testAsciiLowercase() {

		assertEquals("abc123", StringUtils.asciiLowercase("ABC123"));
		assertEquals("Åbc123", StringUtils.asciiLowercase("ÅBC123"));
		assertEquals("", StringUtils.asciiLowercase(""));
	}

	@Test
	public void testJoin() {

		assertEquals("helloworld", StringUtils.join(
				Arrays.asList("hello", "world"),
				""));

		assertEquals("hello,world", StringUtils.join(
				Arrays.asList("hello", "world"),
				","));

		assertEquals("hello,world, abc ", StringUtils.join(
				Arrays.asList("hello", "world", " abc "),
				","));

		assertEquals("hello", StringUtils.join(
				Collections.singletonList("hello"),
				","));

		assertEquals("hello", StringUtils.join(
				Collections.singletonList("hello"),
				""));

		assertEquals("", StringUtils.join(
				Collections.singletonList(""),
				""));

		assertEquals("abcdefabcdef", StringUtils.join(
				Arrays.asList("", "", ""),
				"abcdef"));
	}

	@Test
	public void testIsEmpty() {

		assertTrue(StringUtils.isEmpty(null));
		assertTrue(StringUtils.isEmpty(""));
		assertFalse(StringUtils.isEmpty(" "));
		assertFalse(StringUtils.isEmpty("\t"));
		assertFalse(StringUtils.isEmpty("\n"));
		assertFalse(StringUtils.isEmpty("\r"));
		assertFalse(StringUtils.isEmpty("a"));
	}
}
