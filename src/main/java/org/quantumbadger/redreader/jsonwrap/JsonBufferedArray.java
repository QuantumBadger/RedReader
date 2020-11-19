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
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonToken;
import org.quantumbadger.redreader.common.Consumer;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Iterator;


public final class JsonBufferedArray extends JsonBuffered implements Iterable<JsonValue> {

	private final ArrayList<JsonValue> mContents = new ArrayList<>(16);

	public JsonBufferedArray(final JsonParser jp) throws IOException {

		JsonToken jt;
		while((jt = jp.nextToken()) != JsonToken.END_ARRAY) {
			mContents.add(new JsonValue(jp, jt));
		}
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
	public JsonBufferedObject getObject(final int id) {
		return get(id).asObject();
	}

	@Nullable
	public <E> E getObject(final int id, final Class<E> clazz) throws
			InstantiationException,
			IllegalAccessException,
			NoSuchMethodException,
			InvocationTargetException {

		return get(id).asObject(clazz);
	}

	@Nullable
	public JsonBufferedArray getArray(final int id) {
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

	public void forEachObject(final Consumer<JsonBufferedObject> consumer) {

		for(final JsonValue value : mContents) {
			consumer.consume(value.asObject());
		}
	}
}
