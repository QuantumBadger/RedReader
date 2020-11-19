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

import java.io.IOException;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Modifier;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;


public final class JsonBufferedObject extends JsonBuffered
		implements Iterable<Map.Entry<String, JsonValue>> {

	private final HashMap<String, JsonValue> properties = new HashMap<>();

	public JsonBufferedObject(final JsonParser jp) throws IOException {

		JsonToken jt;

		while((jt = jp.nextToken()) != JsonToken.END_OBJECT) {

			if(jt != JsonToken.FIELD_NAME) {
				throw new JsonParseException(jp, "Expecting field name, got " + jt.name(),
						jp.getCurrentLocation());
			}

			final String fieldName = jp.getCurrentName();
			final JsonValue value = new JsonValue(jp);

			properties.put(fieldName, value);
		}
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
	public JsonBufferedObject getObject(@NonNull final String id) {

		final JsonValue value = get(id);

		if(value == null) {
			return null;
		}

		return value.asObject();
	}

	@Nullable
	public <E> E getObject(@NonNull final String id, final Class<E> clazz) throws
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
	public JsonBufferedArray getArray(@NonNull final String id) {

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
		final String[] fieldNames =
				propertyKeySet.toArray(new String[0]);

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

	public <E> E asObject(final Class<E> clazz) throws
			InstantiationException,
			IllegalAccessException,
			NoSuchMethodException,
			InvocationTargetException {
		final E obj = clazz.getConstructor().newInstance();
		populateObject(obj);
		return obj;
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
					objectField.set(o, val.isNull() ? null : val.asLong().intValue());

				} else if(fieldType == Float.class || fieldType == Float.TYPE) {
					objectField.set(o, val.isNull() ? null : val.asDouble().floatValue());

				} else if(fieldType == Boolean.class || fieldType == Boolean.TYPE) {
					objectField.set(o, val.asBoolean());

				} else if(fieldType == String.class) {
					objectField.set(o, val.asString());

				} else if(fieldType == JsonBufferedArray.class) {
					objectField.set(o, val.asArray());

				} else if(fieldType == JsonBufferedObject.class) {
					objectField.set(o, val.asObject());

				} else if(fieldType == JsonValue.class) {
					objectField.set(o, val);

				} else if(fieldType == Object.class) {

					final Object result;

					switch(val.getType()) {
						case JsonValue.TYPE_BOOLEAN:
							result = val.asBoolean();
							break;
						case JsonValue.TYPE_INTEGER:
							result = val.asLong();
							break;
						case JsonValue.TYPE_STRING:
							result = val.asString();
							break;
						case JsonValue.TYPE_FLOAT:
							result = val.asDouble();
							break;
						case JsonValue.TYPE_NULL:
							result = null;
							break;

						case JsonValue.TYPE_OBJECT:
						case JsonValue.TYPE_ARRAY:
						default:
							result = val;
							break;
					}

					objectField.set(o, result);

				} else {
					objectField.set(o, val.asObject(fieldType));
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
}
