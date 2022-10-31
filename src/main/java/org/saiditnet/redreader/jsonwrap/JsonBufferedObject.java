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

package org.saiditnet.redreader.jsonwrap;

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


/**
 * A JSON object, which may be partially or fully received.
 */
public final class JsonBufferedObject extends JsonBuffered implements Iterable<Map.Entry<String, JsonValue>> {

	private final HashMap<String, JsonValue> properties = new HashMap<>();

	@Override
	protected void buildBuffered(final JsonParser jp) throws IOException {

		JsonToken jt;

		while((jt = jp.nextToken()) != JsonToken.END_OBJECT) {

			if(jt != JsonToken.FIELD_NAME)
				throw new JsonParseException("Expecting field name, got " + jt.name(),
						jp.getCurrentLocation());

			final String fieldName = jp.getCurrentName();
			final JsonValue value = new JsonValue(jp);

			synchronized(this) {
				properties.put(fieldName, value);
				notifyAll();
			}

			value.buildInThisThread();
		}
	}

	/**
	 * This method will block until either: the specified field is received, the
	 * object fails to parse, or the object is fully received and there is no
	 * such field.
	 *
	 * @param name
	 *			The name of the field
	 * @return The value contained in the specified field
	 * @throws InterruptedException
	 * @throws java.io.IOException
	 */
	public JsonValue get(final String name) throws InterruptedException, IOException {

		synchronized(this) {

			while(getStatus() == STATUS_LOADING && !properties.containsKey(name)) {
				wait();
			}

			if(getStatus() != STATUS_FAILED || properties.containsKey(name)) {
				return properties.get(name);
			}

			if(getStatus() == STATUS_FAILED) {
				throwFailReasonException();
			}

			return null;
		}
	}

	/**
	 * This method will block until either: the specified field is received, the
	 * object fails to parse, or the object is fully received and there is no
	 * such field.
	 *
	 * @param name
	 *			The name of the field
	 * @return The value contained in the specified field
	 * @throws InterruptedException
	 * @throws java.io.IOException
	 */
	public String getString(final String name) throws InterruptedException, IOException {
		final JsonValue jsonValue = get(name);
		return jsonValue == null ? null : jsonValue.asString();
	}

	/**
	 * This method will block until either: the specified field is received, the
	 * object fails to parse, or the object is fully received and there is no
	 * such field.
	 *
	 * @param name
	 *			The name of the field
	 * @return The value contained in the specified field
	 * @throws InterruptedException
	 * @throws java.io.IOException
	 */
	public Long getLong(final String name) throws InterruptedException, IOException {
		return get(name).asLong();
	}

	/**
	 * This method will block until either: the specified field is received, the
	 * object fails to parse, or the object is fully received and there is no
	 * such field.
	 *
	 * @param name
	 *			The name of the field
	 * @return The value contained in the specified field
	 * @throws InterruptedException
	 * @throws java.io.IOException
	 */
	public Double getDouble(final String name) throws InterruptedException, IOException {
		return get(name).asDouble();
	}

	/**
	 * This method will block until either: the specified field is received, the
	 * object fails to parse, or the object is fully received and there is no
	 * such field.
	 *
	 * @param name
	 *			The name of the field
	 * @return The value contained in the specified field
	 * @throws InterruptedException
	 * @throws java.io.IOException
	 */
	public Boolean getBoolean(final String name) throws InterruptedException, IOException {
		return get(name).asBoolean();
	}

	/**
	 * This method will block until either: the specified field is received, the
	 * object fails to parse, or the object is fully received and there is no
	 * such field.
	 *
	 * @param name
	 *			The name of the field
	 * @return The value contained in the specified field
	 * @throws InterruptedException
	 * @throws java.io.IOException
	 */
	public JsonBufferedObject getObject(final String name) throws InterruptedException, IOException {
		return get(name).asObject();
	}

	public <E> E getObject(final String name, final Class<E> clazz) throws InterruptedException, IOException, InstantiationException, IllegalAccessException, NoSuchMethodException, InvocationTargetException {
		return get(name).asObject(clazz);
	}

	/**
	 * This method will block until either: the specified field is received, the
	 * object fails to parse, or the object is fully received and there is no
	 * such field.
	 *
	 * @param name
	 *			The name of the field
	 * @return The value contained in the specified field
	 * @throws InterruptedException
	 * @throws java.io.IOException
	 */
	public JsonBufferedArray getArray(final String name) throws InterruptedException, IOException {
		return get(name).asArray();
	}

	@Override
	protected void prettyPrint(final int indent, final StringBuilder sb) throws InterruptedException, IOException {

		if(join() != STATUS_LOADED) {
			throwFailReasonException();
		}

		sb.append('{');

		final Set<String> propertyKeySet = properties.keySet();
		final String[] fieldNames = propertyKeySet.toArray(new String[propertyKeySet.size()]);

		for(int prop = 0; prop < fieldNames.length; prop++) {
			if(prop != 0) sb.append(',');
			sb.append('\n');
			for(int i = 0; i < indent + 1; i++) sb.append("   ");
			sb.append("\"").append(fieldNames[prop].replace("\\", "\\\\").replace("\"", "\\\"")).append("\": ");
			properties.get(fieldNames[prop]).prettyPrint(indent + 1, sb);
		}

		sb.append('\n');
		for(int i = 0; i < indent; i++) sb.append("   ");
		sb.append('}');
	}

	public <E> E asObject(final Class<E> clazz) throws InstantiationException, IllegalAccessException, InterruptedException, IOException, NoSuchMethodException, InvocationTargetException {
		final E obj = clazz.getConstructor().newInstance();
		populateObject(obj);
		return obj;
	}

	public void populateObject(final Object o) throws InterruptedException, IOException, IllegalArgumentException, InstantiationException, NoSuchMethodException, InvocationTargetException {

		if(join() != STATUS_LOADED) {
			throwFailReasonException();
		}

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
					val = properties.get(objectField.getName().substring("_json_".length()));
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
						case JsonValue.TYPE_BOOLEAN: result = val.asBoolean(); break;
						case JsonValue.TYPE_INTEGER: result = val.asLong(); break;
						case JsonValue.TYPE_STRING: result = val.asString(); break;
						case JsonValue.TYPE_FLOAT: result = val.asDouble(); break;
						default: result = val;
					}

					objectField.set(o, result);

				} else {
					objectField.set(o, val.asObject(fieldType));
				}
			}

		} catch(IllegalAccessException e) {
			throw new RuntimeException(e);
		}
	}

	public Iterator<Map.Entry<String, JsonValue>> iterator() {
		try {
			join();
		} catch (InterruptedException e) {
			throw new RuntimeException(e);
		}
		return properties.entrySet().iterator();
	}
}
