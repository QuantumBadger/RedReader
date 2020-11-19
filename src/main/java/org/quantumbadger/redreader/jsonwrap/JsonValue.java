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

package org.quantumbadger.redreader.jsonwrap;

import androidx.annotation.IntDef;
import androidx.annotation.Nullable;
import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonToken;

import java.io.IOException;
import java.io.InputStream;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.reflect.InvocationTargetException;


/**
 * Contains a literal, object, or array value, and is responsible for parsing the incoming JSON
 * stream.
 *
 * <p>
 * <b>To parse a JSON stream, call the JsonValue constructor with a JsonParser
 * as the argument, then call the "build" method (with the same JsonParser as an argument) in
 * another thread.</b>
 * </p>
 */
public final class JsonValue {

	public static final int TYPE_OBJECT = 0;
	public static final int TYPE_ARRAY = 1;
	public static final int TYPE_NULL = 2;
	public static final int TYPE_BOOLEAN = 3;
	public static final int TYPE_STRING = 4;
	public static final int TYPE_FLOAT = 5;
	public static final int TYPE_INTEGER = 6;

	@IntDef({
			TYPE_OBJECT, TYPE_ARRAY, TYPE_NULL, TYPE_BOOLEAN, TYPE_STRING, TYPE_FLOAT,
			TYPE_INTEGER})
	@Retention(RetentionPolicy.SOURCE)
	public @interface Type {
	}

	private final @Type int type;
	private final Object value;

	public JsonValue(final JsonParser jp) throws IOException {
		this(jp, jp.nextToken());
	}

	public JsonValue(final InputStream source) throws IOException {
		this(new JsonFactory().createParser(source));
	}

	public JsonValue(final byte[] source) throws IOException {
		this(new JsonFactory().createParser(source));
	}

	protected JsonValue(final JsonParser parser, final JsonToken firstToken) throws IOException {

		switch(firstToken) {

			case START_OBJECT:
				type = TYPE_OBJECT;
				value = new JsonBufferedObject(parser);
				break;

			case START_ARRAY:
				type = TYPE_ARRAY;
				value = new JsonBufferedArray(parser);
				break;

			case VALUE_FALSE:
				type = TYPE_BOOLEAN;
				value = false;
				break;

			case VALUE_TRUE:
				type = TYPE_BOOLEAN;
				value = true;
				break;

			case VALUE_NULL:
				type = TYPE_NULL;
				value = null;
				break;

			case VALUE_STRING:
				type = TYPE_STRING;
				value = parser.getValueAsString();
				break;

			case VALUE_NUMBER_FLOAT:

				//noinspection FloatingPointEquality,UnnecessaryExplicitNumericCast
				if(parser.getValueAsDouble() == (double)parser.getValueAsLong()) {
					type = TYPE_INTEGER;
					value = parser.getValueAsLong();
				} else {
					type = TYPE_FLOAT;
					value = parser.getValueAsDouble();
				}

				break;

			case VALUE_NUMBER_INT:
				type = TYPE_INTEGER;
				value = parser.getValueAsLong();
				break;

			default:
				throw new JsonParseException(
						parser,
						"Expecting an object, literal, or array",
						parser.getCurrentLocation());
		}
	}

	/**
	 * @return The type of value this JsonValue contains.
	 */
	public @Type
	int getType() {
		return type;
	}

	/**
	 * @return True if the type of this value is NULL.
	 */
	public boolean isNull() {
		return type == TYPE_NULL;
	}

	/**
	 * @return If this JsonValue contains a JSON object, then this method returns that object. The
	 * type of value this JsonValue contains can be checked with the getType() method.
	 */
	@Nullable
	public JsonBufferedObject asObject() {

		if(value instanceof JsonBufferedObject) {
			return (JsonBufferedObject)value;
		} else {
			return null;
		}
	}

	@Nullable
	public <E> E asObject(final Class<E> clazz) throws
			InstantiationException,
			IllegalAccessException,
			NoSuchMethodException,
			InvocationTargetException {

		if(value instanceof JsonBufferedObject) {
			return ((JsonBufferedObject)value).asObject(clazz);
		} else {
			return null;
		}
	}

	/**
	 * @return If this JsonValue contains a JSON array, then this method returns that array. The
	 * type of value this JsonValue contains can be checked with the getType() method.
	 */
	@Nullable
	public JsonBufferedArray asArray() {

		if(value instanceof JsonBufferedArray) {
			return (JsonBufferedArray)value;
		} else {
			return null;
		}
	}

	/**
	 * @return If this JsonValue contains a boolean, then this method returns that boolean. The type
	 * of value this JsonValue contains can be checked with the getType() method.
	 */
	@Nullable
	public Boolean asBoolean() {

		if(value instanceof Boolean) {
			return (Boolean)value;
		} else {
			return null;
		}
	}

	/**
	 * @return If this JsonValue contains a string, then this method returns that string. The type
	 * of value this JsonValue contains can be checked with the getType() method.
	 */
	@Nullable
	public String asString() {

		if(value instanceof String) {
			return (String)value;

		} else if(value != null) {
			return value.toString();

		} else {
			return null;
		}
	}

	/**
	 * @return If this JsonValue contains a double, then this method returns that double. The type
	 * of value this JsonValue contains can be checked with the getType() method.
	 */
	@Nullable
	public Double asDouble() {

		if(value instanceof Double) {
			return (Double)value;

		} else if(value instanceof String) {
			return Double.parseDouble((String)value);

		} else if(value instanceof Long) {
			return ((Long)value).doubleValue();

		} else {
			return null;
		}
	}

	/**
	 * @return If this JsonValue contains an integer, then this method returns that integer. The
	 * type of value this JsonValue contains can be checked with the getType() method.
	 */
	@Nullable
	public Long asLong() {

		if(value instanceof Long) {
			return (Long)value;

		} else if(value instanceof String) {
			return Long.parseLong((String)value);

		} else if(value instanceof Double) {
			return ((Double)value).longValue();

		} else {
			return null;
		}
	}

	@Override
	public String toString() {
		final StringBuilder sb = new StringBuilder();
		prettyPrint(0, sb);
		return sb.toString();
	}

	protected void prettyPrint(final int indent, final StringBuilder sb) {

		switch(type) {
			case TYPE_BOOLEAN:
				sb.append(asBoolean());
				break;
			case TYPE_FLOAT:
				sb.append(asDouble());
				break;
			case TYPE_INTEGER:
				sb.append(asLong());
				break;
			case TYPE_NULL:
				sb.append("null");
				break;
			case TYPE_STRING:
				sb.append("\"")
						.append(asString().replace("\\", "\\\\").replace("\"", "\\\""))
						.append("\"");
				break;
			case TYPE_ARRAY:
			case TYPE_OBJECT:
				((JsonBuffered)value).prettyPrint(indent, sb);
				break;
		}
	}
}
