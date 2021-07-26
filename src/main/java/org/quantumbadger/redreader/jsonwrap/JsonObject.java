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
import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonToken;
import org.quantumbadger.redreader.common.Optional;

import java.io.IOException;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Modifier;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;


public final class JsonObject extends JsonValue
		implements Iterable<Map.Entry<String, JsonValue>> {

	public interface JsonDeserializable {}

	private final HashMap<String, JsonValue> properties = new HashMap<>();

	protected JsonObject(final JsonParser parser) throws IOException {

		if(parser.currentToken() != JsonToken.START_OBJECT) {
			throw new JsonParseException(
					parser,
					"Expecting object start, got " + parser.currentToken(),
					parser.getCurrentLocation());
		}

		parser.nextToken();

		JsonToken jt;

		while((jt = parser.currentToken()) != JsonToken.END_OBJECT) {

			if(jt != JsonToken.FIELD_NAME) {
				throw new JsonParseException(parser, "Expecting field name, got " + jt.name(),
						parser.getCurrentLocation());
			}

			final String fieldName = parser.getCurrentName();

			parser.nextToken();
			final JsonValue value = JsonValue.parse(parser);

			properties.put(fieldName, value);
		}

		parser.nextToken();
	}

	public boolean isEmpty() {
		return properties.isEmpty();
	}

	@NonNull
	@Override
	public JsonObject asObject() {
		return this;
	}

	@NonNull
	@Override
	public <E extends JsonDeserializable> E asObject(final Class<E> clazz) throws
			InstantiationException,
			IllegalAccessException,
			NoSuchMethodException,
			InvocationTargetException {

		final E obj = clazz.getConstructor().newInstance();
		populateObject(obj);
		return obj;
	}

	@Nullable
	public JsonValue get(final String name) {
		return properties.get(name);
	}

	@Nullable
	public String getString(@NonNull final String id) {

		final JsonValue value = get(id);

		if(value == null) {
			return null;
		}

		return value.asString();
	}

	@Nullable
	public Long getLong(@NonNull final String id) {

		final JsonValue value = get(id);

		if(value == null) {
			return null;
		}

		return value.asLong();
	}

	@Nullable
	public Double getDouble(@NonNull final String id) {

		final JsonValue value = get(id);

		if(value == null) {
			return null;
		}

		return value.asDouble();
	}

	@Nullable
	public Boolean getBoolean(@NonNull final String id) {

		final JsonValue value = get(id);

		if(value == null) {
			return null;
		}

		return value.asBoolean();
	}

	@Nullable
	public JsonObject getObject(@NonNull final String id) {

		final JsonValue value = get(id);

		if(value == null) {
			return null;
		}

		return value.asObject();
	}

	@Nullable
	public <E extends JsonDeserializable> E getObject(
			@NonNull final String id,
			final Class<E> clazz) throws
					InstantiationException,
					IllegalAccessException,
					NoSuchMethodException,
					InvocationTargetException {

		final JsonValue value = get(id);

		if(value == null) {
			return null;
		}

		return value.asObject(clazz);
	}

	@Nullable
	public JsonArray getArray(@NonNull final String id) {

		final JsonValue value = get(id);

		if(value == null) {
			return null;
		}

		return value.asArray();
	}

	@Override
	protected void prettyPrint(final int indent, final StringBuilder sb) {

		sb.append('{');

		final Set<String> propertyKeySet = properties.keySet();
		final String[] fieldNames = propertyKeySet.toArray(new String[0]);

		for(int prop = 0; prop < fieldNames.length; prop++) {
			if(prop != 0) {
				sb.append(',');
			}
			sb.append('\n');
			for(int i = 0; i < indent + 1; i++) {
				sb.append("   ");
			}
			sb.append("\"")
					.append(fieldNames[prop].replace("\\", "\\\\").replace("\"", "\\\""))
					.append("\": ");
			properties.get(fieldNames[prop]).prettyPrint(indent + 1, sb);
		}

		sb.append('\n');
		for(int i = 0; i < indent; i++) {
			sb.append("   ");
		}
		sb.append('}');
	}

	public void populateObject(final Object o) throws
			IllegalArgumentException,
			InstantiationException,
			NoSuchMethodException,
			InvocationTargetException {

		final Field[] objectFields = o.getClass().getFields();

		try {

			for(final Field objectField : objectFields) {

				if((objectField.getModifiers() & Modifier.TRANSIENT) != 0) {
					continue;
				}

				final JsonValue val;

				if(properties.containsKey(objectField.getName())) {
					val = properties.get(objectField.getName());

				} else if(objectField.getName().startsWith("_json_")) {
					val = properties.get(objectField.getName()
							.substring("_json_".length()));
				} else {
					val = null;
				}

				if(val == null) {
					continue;
				}

				objectField.setAccessible(true);

				final Class<?> fieldType = objectField.getType();

				if(fieldType == Long.class || fieldType == Long.TYPE) {
					objectField.set(o, val.asLong());

				} else if(fieldType == Double.class || fieldType == Double.TYPE) {
					objectField.set(o, val.asDouble());

				} else if(fieldType == Integer.class || fieldType == Integer.TYPE) {
					objectField.set(o, val.asLong() == null ? null : val.asLong().intValue());

				} else if(fieldType == Float.class || fieldType == Float.TYPE) {
					objectField.set(o, val.asDouble() == null ? null : val.asDouble().floatValue());

				} else if(fieldType == Boolean.class || fieldType == Boolean.TYPE) {
					objectField.set(o, val.asBoolean());

				} else if(fieldType == String.class) {
					objectField.set(o, val.asString());

				} else if(fieldType == JsonArray.class) {
					objectField.set(o, val.asArray());

				} else if(fieldType == JsonObject.class) {
					objectField.set(o, val.asObject());

				} else if(fieldType == JsonValue.class) {
					objectField.set(o, val);

				} else if(JsonDeserializable.class.isAssignableFrom(fieldType)) {
					//noinspection unchecked
					objectField.set(o, val.asObject(
							(Class<? extends JsonDeserializable>)fieldType));

				} else {
					throw new RuntimeException("Cannot handle field type "
							+ fieldType.getCanonicalName());
				}
			}

		} catch(final IllegalAccessException e) {
			throw new RuntimeException(e);
		}
	}

	@Override
	public Iterator<Map.Entry<String, JsonValue>> iterator() {
		return properties.entrySet().iterator();
	}

	@NonNull
	@Override
	protected Optional<JsonValue> getAtPathInternal(final int offset, final Object... keys) {

		if(offset == keys.length) {
			return Optional.of(this);
		}

		final JsonValue next = properties.get(keys[offset].toString());

		if(next == null) {
			return Optional.empty();
		}

		return next.getAtPathInternal(offset + 1, keys);
	}
}
