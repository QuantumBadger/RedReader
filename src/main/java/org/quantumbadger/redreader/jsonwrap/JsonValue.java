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

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.core.JsonParser;
import org.quantumbadger.redreader.common.Optional;

import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.InvocationTargetException;


public abstract class JsonValue {

	@NonNull
	public static JsonValue parse(final InputStream source) throws IOException {
		return parse(new JsonFactory().createParser(source));
	}

	@NonNull
	public static JsonValue parse(final JsonParser parser) throws IOException {

		if(parser.currentToken() == null) {
			parser.nextToken();
		}

		if(parser.currentToken() == null) {
			throw new IOException("Invalid input: no JSON tokens available");
		}

		switch(parser.currentToken()) {

			case START_OBJECT:
				return new JsonObject(parser);

			case START_ARRAY:
				return new JsonArray(parser);

			case VALUE_FALSE:
				parser.nextToken();
				return JsonBoolean.FALSE;

			case VALUE_TRUE:
				parser.nextToken();
				return JsonBoolean.TRUE;

			case VALUE_NULL:
				parser.nextToken();
				return JsonNull.INSTANCE;

			case VALUE_STRING: {
				final JsonString result = new JsonString(parser.getValueAsString());
				parser.nextToken();
				return result;
			}

			case VALUE_NUMBER_FLOAT: {
				final JsonDouble result = new JsonDouble(parser.getValueAsDouble());
				parser.nextToken();
				return result;
			}

			case VALUE_NUMBER_INT: {
				final JsonLong result = new JsonLong(parser.getValueAsLong());
				parser.nextToken();
				return result;
			}

			default:
				throw new JsonParseException(
						parser,
						"Expecting an object, literal, or array, got: " + parser.currentToken(),
						parser.getCurrentLocation());
		}
	}

	@Nullable
	public JsonObject asObject() {
		// Default implementation
		return null;
	}

	@Nullable
	public <E extends JsonObject.JsonDeserializable> E asObject(final Class<E> clazz) throws
			InstantiationException,
			IllegalAccessException,
			NoSuchMethodException,
			InvocationTargetException {

		// Default implementation
		return null;
	}

	@Nullable
	public JsonArray asArray() {
		// Default implementation
		return null;
	}

	@Nullable
	public Boolean asBoolean() {
		// Default implementation
		return null;
	}

	@Nullable
	public String asString() {
		// Default implementation
		return null;
	}

	@Nullable
	public Double asDouble() {
		// Default implementation
		return null;
	}

	@Nullable
	public Long asLong() {
		// Default implementation
		return null;
	}

	@Override
	public String toString() {
		final StringBuilder sb = new StringBuilder();
		prettyPrint(0, sb);
		return sb.toString();
	}

	protected abstract void prettyPrint(int indent, StringBuilder sb);

	@NonNull
	public final Optional<JsonValue> getAtPath(final Object... keys) {
		return getAtPathInternal(0, keys);
	}

	@NonNull
	public final Optional<JsonObject> getObjectAtPath(final Object... keys) {

		final Optional<JsonValue> result = getAtPath(keys);

		if(result.isEmpty()) {
			return Optional.empty();
		}

		return Optional.ofNullable(result.get().asObject());
	}

	@NonNull
	public final Optional<JsonArray> getArrayAtPath(final Object... keys) {

		final Optional<JsonValue> result = getAtPath(keys);

		if(result.isEmpty()) {
			return Optional.empty();
		}

		return Optional.ofNullable(result.get().asArray());
	}

	@NonNull
	public final Optional<String> getStringAtPath(final Object... keys) {

		final Optional<JsonValue> result = getAtPath(keys);

		if(result.isEmpty()) {
			return Optional.empty();
		}

		return Optional.ofNullable(result.get().asString());
	}

	@NonNull
	protected Optional<JsonValue> getAtPathInternal(final int offset, final Object... keys) {

		// Default implementation

		if(offset == keys.length) {
			return Optional.of(this);
		}

		return Optional.empty();
	}
}
