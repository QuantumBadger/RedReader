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
import org.quantumbadger.redreader.common.Consumer;
import org.quantumbadger.redreader.common.Optional;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Iterator;


public final class JsonArray extends JsonValue implements Iterable<JsonValue> {

	private final ArrayList<JsonValue> mContents = new ArrayList<>(16);

	protected JsonArray(final JsonParser parser) throws IOException {

		if(parser.currentToken() != JsonToken.START_ARRAY) {
			throw new JsonParseException(
					parser,
					"Expecting array start, got " + parser.currentToken(),
					parser.getCurrentLocation());
		}

		parser.nextToken();

		while(parser.currentToken() != JsonToken.END_ARRAY) {
			mContents.add(JsonValue.parse(parser));
		}

		parser.nextToken();
	}

	@NonNull
	@Override
	public JsonArray asArray() {
		return this;
	}

	@NonNull
	public JsonValue get(final int id) {
		return mContents.get(id);
	}

	@Nullable
	public String getString(final int id) {
		return get(id).asString();
	}

	@Nullable
	public Long getLong(final int id) {
		return get(id).asLong();
	}

	@Nullable
	public Double getDouble(final int id) {
		return get(id).asDouble();
	}

	@Nullable
	public Boolean getBoolean(final int id) {
		return get(id).asBoolean();
	}

	@Nullable
	public JsonObject getObject(final int id) {
		return get(id).asObject();
	}

	@Nullable
	public <E extends JsonObject.JsonDeserializable> E getObject(
					final int id,
					final Class<E> clazz) throws
			InstantiationException,
			IllegalAccessException,
			NoSuchMethodException,
			InvocationTargetException {

		return get(id).asObject(clazz);
	}

	@Nullable
	public JsonArray getArray(final int id) {
		return get(id).asArray();
	}

	@Override
	public Iterator<JsonValue> iterator() {
		return mContents.iterator();
	}

	@Override
	protected void prettyPrint(final int indent, final StringBuilder sb) {

		sb.append('[');

		for(int item = 0; item < mContents.size(); item++) {
			if(item != 0) {
				sb.append(',');
			}
			sb.append('\n');
			for(int i = 0; i < indent + 1; i++) {
				sb.append("   ");
			}
			mContents.get(item).prettyPrint(indent + 1, sb);
		}

		sb.append('\n');
		for(int i = 0; i < indent; i++) {
			sb.append("   ");
		}
		sb.append(']');
	}

	public int size() {
		return mContents.size();
	}

	public void forEachObject(final Consumer<JsonObject> consumer) {

		for(final JsonValue value : mContents) {
			consumer.consume(value.asObject());
		}
	}

	@NonNull
	@Override
	protected Optional<JsonValue> getAtPathInternal(final int offset, final Object... keys) {

		if(offset == keys.length) {
			return Optional.of(this);
		}

		if(!(keys[offset] instanceof Integer)) {
			return Optional.empty();
		}

		final int key = (Integer)keys[offset];

		if(key < 0 || key >= mContents.size()) {
			return Optional.empty();
		}

		final JsonValue next = mContents.get(key);

		if(next == null) {
			return Optional.empty();
		}

		return next.getAtPathInternal(offset + 1, keys);
	}
}
