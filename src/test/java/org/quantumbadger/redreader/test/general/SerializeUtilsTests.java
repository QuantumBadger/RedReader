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

import androidx.annotation.NonNull;
import org.junit.Assert;
import org.junit.Test;
import org.quantumbadger.redreader.common.SerializeUtils;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class SerializeUtilsTests {

	private static final class DataHandler {

		@NonNull private final ByteArrayOutputStream mOutput
				= new ByteArrayOutputStream();

		@NonNull
		public DataOutputStream getOutput() {
			return new DataOutputStream(mOutput);
		}

		@NonNull
		public DataInputStream getInput() {
			return new DataInputStream(new ByteArrayInputStream(mOutput.toByteArray()));
		}
	}

	@Test
	public void testNull() throws SerializeUtils.UnhandledTypeException, IOException {

		final DataHandler dataHandler = new DataHandler();

		SerializeUtils.serialize(dataHandler.getOutput(), null);

		Assert.assertNull(SerializeUtils.deserialize(dataHandler.getInput()));
	}

	@Test
	public void testByte() throws SerializeUtils.UnhandledTypeException, IOException {

		final DataHandler dataHandler = new DataHandler();
		SerializeUtils.serialize(dataHandler.getOutput(), (byte)123);

		final Object result = SerializeUtils.deserialize(dataHandler.getInput());

		Assert.assertTrue(result instanceof Byte);
		Assert.assertEquals((byte)123, result);
	}

	@Test
	public void testChar() throws SerializeUtils.UnhandledTypeException, IOException {

		final DataHandler dataHandler = new DataHandler();
		SerializeUtils.serialize(dataHandler.getOutput(), (char)123);

		final Object result = SerializeUtils.deserialize(dataHandler.getInput());

		Assert.assertTrue(result instanceof Character);
		Assert.assertEquals((char)123, result);
	}

	@Test
	public void testShort() throws SerializeUtils.UnhandledTypeException, IOException {

		final DataHandler dataHandler = new DataHandler();
		SerializeUtils.serialize(dataHandler.getOutput(), (short)123);

		final Object result = SerializeUtils.deserialize(dataHandler.getInput());

		Assert.assertTrue(result instanceof Short);
		Assert.assertEquals((short)123, result);
	}

	@Test
	public void testInt() throws SerializeUtils.UnhandledTypeException, IOException {

		final DataHandler dataHandler = new DataHandler();
		SerializeUtils.serialize(dataHandler.getOutput(), 123);

		final Object result = SerializeUtils.deserialize(dataHandler.getInput());

		Assert.assertTrue(result instanceof Integer);
		Assert.assertEquals(123, result);
	}

	@Test
	public void testLong() throws SerializeUtils.UnhandledTypeException, IOException {

		final DataHandler dataHandler = new DataHandler();
		SerializeUtils.serialize(dataHandler.getOutput(), (long)123);

		final Object result = SerializeUtils.deserialize(dataHandler.getInput());

		Assert.assertTrue(result instanceof Long);
		Assert.assertEquals((long)123, result);
	}

	@Test
	public void testFloat() throws SerializeUtils.UnhandledTypeException, IOException {

		final DataHandler dataHandler = new DataHandler();
		SerializeUtils.serialize(dataHandler.getOutput(), 0.25f);

		final Object result = SerializeUtils.deserialize(dataHandler.getInput());

		Assert.assertTrue(result instanceof Float);
		Assert.assertEquals(0.25f, result);
	}

	@Test
	public void testDouble() throws SerializeUtils.UnhandledTypeException, IOException {

		final DataHandler dataHandler = new DataHandler();
		SerializeUtils.serialize(dataHandler.getOutput(), 0.25);

		final Object result = SerializeUtils.deserialize(dataHandler.getInput());

		Assert.assertTrue(result instanceof Double);
		Assert.assertEquals(0.25, result);
	}

	@Test
	public void testTrue() throws SerializeUtils.UnhandledTypeException, IOException {

		final DataHandler dataHandler = new DataHandler();
		SerializeUtils.serialize(dataHandler.getOutput(), true);

		final Object result = SerializeUtils.deserialize(dataHandler.getInput());

		Assert.assertTrue(result instanceof Boolean);
		Assert.assertEquals(true, result);
	}

	@Test
	public void testFalse() throws SerializeUtils.UnhandledTypeException, IOException {

		final DataHandler dataHandler = new DataHandler();
		SerializeUtils.serialize(dataHandler.getOutput(), false);

		final Object result = SerializeUtils.deserialize(dataHandler.getInput());

		Assert.assertTrue(result instanceof Boolean);
		Assert.assertEquals(false, result);
	}

	@Test
	public void testString() throws SerializeUtils.UnhandledTypeException, IOException {

		final DataHandler dataHandler = new DataHandler();
		SerializeUtils.serialize(dataHandler.getOutput(), "Hello world");

		final Object result = SerializeUtils.deserialize(dataHandler.getInput());

		Assert.assertTrue(result instanceof String);
		Assert.assertEquals("Hello world", result);
	}

	@Test
	public void testSet() throws SerializeUtils.UnhandledTypeException, IOException {

		final DataHandler dataHandler = new DataHandler();

		final Set<Object> input = new HashSet<>();
		input.add(12345);
		input.add("String value");
		input.add(null);

		SerializeUtils.serialize(dataHandler.getOutput(), input);

		//noinspection unchecked
		final Set<Object> result
				= (Set<Object>)SerializeUtils.deserialize(dataHandler.getInput());

		Assert.assertNotNull(result);
		Assert.assertEquals(input, result);
		Assert.assertEquals(input.size(), result.size());

		Assert.assertTrue(result.contains(12345));
		Assert.assertTrue(result.contains("String value"));
		Assert.assertTrue(result.contains(null));
	}

	@Test
	public void testEmptySet() throws SerializeUtils.UnhandledTypeException, IOException {

		final DataHandler dataHandler = new DataHandler();

		final Set<Object> input = new HashSet<>();

		SerializeUtils.serialize(dataHandler.getOutput(), input);

		//noinspection unchecked
		final Set<Object> result
				= (Set<Object>)SerializeUtils.deserialize(dataHandler.getInput());

		Assert.assertNotNull(result);
		Assert.assertEquals(input, result);
		Assert.assertEquals(input.size(), result.size());
	}

	@Test
	public void testList() throws SerializeUtils.UnhandledTypeException, IOException {

		final DataHandler dataHandler = new DataHandler();

		final List<Object> input = new ArrayList<>();
		input.add(12345);
		input.add("String value");
		input.add(null);

		SerializeUtils.serialize(dataHandler.getOutput(), input);

		//noinspection unchecked
		final List<Object> result
				= (List<Object>)SerializeUtils.deserialize(dataHandler.getInput());

		Assert.assertNotNull(result);
		Assert.assertEquals(input, result);
		Assert.assertEquals(input.size(), result.size());

		Assert.assertEquals(12345, result.get(0));
		Assert.assertEquals("String value", result.get(1));
		Assert.assertNull(result.get(2));
	}

	@Test
	public void testEmptyList() throws SerializeUtils.UnhandledTypeException, IOException {

		final DataHandler dataHandler = new DataHandler();

		final List<Object> input = new ArrayList<>();

		SerializeUtils.serialize(dataHandler.getOutput(), input);

		//noinspection unchecked
		final List<Object> result
				= (List<Object>)SerializeUtils.deserialize(dataHandler.getInput());

		Assert.assertNotNull(result);
		Assert.assertEquals(input, result);
		Assert.assertEquals(input.size(), result.size());
	}

	@Test
	public void testMap() throws SerializeUtils.UnhandledTypeException, IOException {

		final DataHandler dataHandler = new DataHandler();

		final Map<Object, Object> input = new HashMap<>();
		input.put("first", 12345);
		input.put("second", "String value");
		input.put(543, null);
		input.put((byte)98, 0.25);

		SerializeUtils.serialize(dataHandler.getOutput(), input);

		//noinspection unchecked
		final Map<Object, Object> result
				= (Map<Object, Object>)SerializeUtils.deserialize(dataHandler.getInput());

		Assert.assertNotNull(result);
		Assert.assertEquals(input, result);
		Assert.assertEquals(input.size(), result.size());

		Assert.assertEquals(12345, result.get("first"));
		Assert.assertEquals("String value", result.get("second"));
		Assert.assertNull(result.get(543));
		Assert.assertEquals(0.25, result.get((byte)98));
	}

	@Test
	public void testEmptyMap() throws SerializeUtils.UnhandledTypeException, IOException {

		final DataHandler dataHandler = new DataHandler();

		final Map<Object, Object> input = new HashMap<>();

		SerializeUtils.serialize(dataHandler.getOutput(), input);

		//noinspection unchecked
		final Map<Object, Object> result
				= (Map<Object, Object>)SerializeUtils.deserialize(dataHandler.getInput());

		Assert.assertNotNull(result);
		Assert.assertEquals(input, result);
		Assert.assertEquals(input.size(), result.size());
	}

	@Test
	public void testMapContainingList() throws SerializeUtils.UnhandledTypeException, IOException {

		final DataHandler dataHandler = new DataHandler();

		final Map<Object, Object> input = new HashMap<>();
		input.put("first", Collections.singletonList(0.25));

		SerializeUtils.serialize(dataHandler.getOutput(), input);

		//noinspection unchecked
		final Map<Object, Object> result
				= (Map<Object, Object>)SerializeUtils.deserialize(dataHandler.getInput());

		Assert.assertNotNull(result);
		Assert.assertEquals(input, result);
	}
}
