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

package org.saiditnet.redreader.io;

import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteOpenHelper;
import android.util.Log;
import org.saiditnet.redreader.common.UnexpectedInternalStateException;

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedList;
import java.util.Locale;

public class RawObjectDB<K, E extends WritableObject<K>> extends SQLiteOpenHelper {

	private final Class<E> clazz;

	private final Field[] fields;
	private final String[] fieldNames;

	private static final String TABLE_NAME = "objects",
			FIELD_ID = "RawObjectDB_id",
			FIELD_TIMESTAMP = "RawObjectDB_timestamp";

	private static <E> int getDbVersion(Class<E> clazz) {
		for(final Field field : clazz.getDeclaredFields()) {
			if(field.isAnnotationPresent(WritableObject.WritableObjectVersion.class)) {
				field.setAccessible(true);
				try {
					return field.getInt(null);
				} catch(IllegalAccessException e) {
					throw new RuntimeException(e);
				}
			}
		}
		throw new UnexpectedInternalStateException("Writable object has no DB version");
	}

	public RawObjectDB(final Context context, final String dbFilename, final Class<E> clazz) {

		super(context.getApplicationContext(), dbFilename, null, getDbVersion(clazz));
		this.clazz = clazz;

		final LinkedList<Field> fields = new LinkedList<>();
		for(final Field field : clazz.getDeclaredFields()) {
			if((field.getModifiers() & Modifier.TRANSIENT) == 0
					&& !field.isAnnotationPresent(WritableObject.WritableObjectKey.class)
					&& !field.isAnnotationPresent(WritableObject.WritableObjectTimestamp.class)
					&& field.isAnnotationPresent(WritableObject.WritableField.class)) {

				field.setAccessible(true);
				fields.add(field);
			}
		}

		this.fields = fields.toArray(new Field[fields.size()]);

		fieldNames = new String[this.fields.length + 2];
		for(int i = 0; i < this.fields.length; i++) fieldNames[i] = this.fields[i].getName();
		fieldNames[this.fields.length] = FIELD_ID;
		fieldNames[this.fields.length + 1] = FIELD_TIMESTAMP;
	}

	private String getFieldTypeString(Class<?> fieldType) {

		if(fieldType == Integer.class
				|| fieldType == Long.class
				|| fieldType == Integer.TYPE
				|| fieldType == Long.TYPE) {
			return " INTEGER";

		} else if(fieldType == Boolean.class
				|| fieldType == Boolean.TYPE) {
			return " INTEGER";

		} else {
			return " TEXT";
		}
	}

	@Override
	public void onCreate(final SQLiteDatabase db) {

		final StringBuilder query = new StringBuilder("CREATE TABLE ");
		query.append(TABLE_NAME);
		query.append('(');
		query.append(FIELD_ID);
		query.append(" TEXT PRIMARY KEY ON CONFLICT REPLACE,");
		query.append(FIELD_TIMESTAMP);
		query.append(" INTEGER");

		for(final Field field : fields) {
			query.append(',');
			query.append(field.getName());
			query.append(getFieldTypeString(field.getType()));
		}

		query.append(')');

		Log.i("RawObjectDB query string", query.toString());

		db.execSQL(query.toString());
	}

	@Override
	public void onUpgrade(final SQLiteDatabase db, final int oldVersion, final int newVersion) {
		// TODO detect version from static field/WritableObject method, delete all data, start again
	}

	public synchronized Collection<E> getAll() {

		final SQLiteDatabase db = getReadableDatabase();

		try {

			final Cursor cursor = db.query(TABLE_NAME, fieldNames, null, null, null, null, null);

			try {

				final LinkedList<E> result = new LinkedList<>();
				while(cursor.moveToNext()) result.add(readFromCursor(cursor));
				return result;

			} catch(InstantiationException e) {
				throw new RuntimeException(e);

			} catch(IllegalAccessException e) {
				throw new RuntimeException(e);

			} catch(InvocationTargetException e) {
				throw new RuntimeException(e);

			} finally { cursor.close(); }
		} finally { db.close(); }
	}

	public synchronized E getById(final K id) {
		final ArrayList<E> queryResult = getByField(FIELD_ID, id.toString());
		if(queryResult.size() != 1) return null;
		else return queryResult.get(0);
	}

	public synchronized ArrayList<E> getByField(final String field, final String value) {

		final SQLiteDatabase db = getReadableDatabase();

		try {

			final Cursor cursor = db.query(TABLE_NAME, fieldNames, String.format(Locale.US, "%s=?", field),
					new String[] {value}, null, null, null);

			try {
				final ArrayList<E> result = new ArrayList<>(cursor.getCount());
				while(cursor.moveToNext()) result.add(readFromCursor(cursor));
				return result;

			} catch(InstantiationException e) {
				throw new RuntimeException(e);

			} catch(IllegalAccessException e) {
				throw new RuntimeException(e);

			} catch(InvocationTargetException e) {
				throw new RuntimeException(e);

			} finally { cursor.close(); }
		} finally { db.close(); }
	}

	private E readFromCursor(final Cursor cursor)
			throws IllegalAccessException, InstantiationException, InvocationTargetException {

		final E obj;
		try {
			final Constructor<E> constructor = clazz.getConstructor(WritableObject.CreationData.class);
			final String id = cursor.getString(fields.length);
			final long timestamp = cursor.getLong(fields.length + 1);
			obj = constructor.newInstance(new WritableObject.CreationData(id, timestamp));

		} catch(NoSuchMethodException e) {
			throw new RuntimeException(e);
		}

		for(int i = 0; i < fields.length; i++) {

			final Field field = fields[i];
			final Class<?> fieldType = field.getType();

			if(fieldType == String.class) {
				field.set(obj, cursor.isNull(i) ? null : cursor.getString(i));

			} else if(fieldType == Integer.class) {
				field.set(obj, cursor.isNull(i) ? null : cursor.getInt(i));

			} else if(fieldType == Integer.TYPE) {
				field.setInt(obj, cursor.getInt(i));

			} else if(fieldType == Long.class) {
				field.set(obj, cursor.isNull(i) ? null : cursor.getLong(i));

			} else if(fieldType == Long.TYPE) {
				field.setLong(obj, cursor.getLong(i));

			} else if(fieldType == Boolean.class) {
				field.set(obj, cursor.isNull(i) ? null : cursor.getInt(i) != 0);

			} else if(fieldType == Boolean.TYPE) {
				field.setBoolean(obj, cursor.getInt(i) != 0);

			} else if(fieldType == WritableHashSet.class) {
				field.set(obj, cursor.isNull(i) ? null : WritableHashSet.unserializeWithMetadata(cursor.getString(i)));

			} else {
				throw new UnexpectedInternalStateException("Invalid readFromCursor field type "
						+ fieldType.getClass().getCanonicalName());
			}
		}

		return obj;
	}

	public synchronized void put(E object) {

		final SQLiteDatabase db = getWritableDatabase();

		try {
			final ContentValues values = new ContentValues(fields.length + 1);
			final long result = db.insertOrThrow(TABLE_NAME, null, toContentValues(object, values));

			if(result < 0) throw new RuntimeException("Database write failed");

		} catch(IllegalAccessException e) {
			throw new RuntimeException(e);

		} finally { db.close(); }
	}

	public synchronized void putAll(final Collection<E> objects) {

		final SQLiteDatabase db = getWritableDatabase();

		try {

			final ContentValues values = new ContentValues(fields.length + 1);

			for(final E object : objects) {
				final long result = db.insertOrThrow(TABLE_NAME, null, toContentValues(object, values));
				if(result < 0) throw new RuntimeException("Bulk database write failed");
			}

		} catch(IllegalAccessException e) {
			throw new RuntimeException(e);

		} finally { db.close(); }
	}

	private ContentValues toContentValues(final E obj, final ContentValues result) throws IllegalAccessException {

		result.put(FIELD_ID, obj.getKey().toString());
		result.put(FIELD_TIMESTAMP, obj.getTimestamp());

		for(int i = 0; i < fields.length; i++) {

			final Field field = fields[i];
			final Class<?> fieldType = field.getType();

			if(fieldType == String.class) {
				result.put(fieldNames[i], (String) field.get(obj));

			} else if(fieldType == Integer.class) {
				result.put(fieldNames[i], (Integer) field.get(obj));

			} else if(fieldType == Integer.TYPE) {
				result.put(fieldNames[i], field.getInt(obj));

			} else if(fieldType == Long.class) {
				result.put(fieldNames[i], (Long) field.get(obj));

			} else if (fieldType == Long.TYPE) {
				result.put(fieldNames[i], field.getLong(obj));

			} else if(fieldType == Boolean.class) {
				final Boolean val = (Boolean) field.get(obj);
				result.put(fieldNames[i], val == null ? null : (val ? 1 : 0));

			} else if(fieldType == Boolean.TYPE) {
				result.put(fieldNames[i], field.getBoolean(obj) ? 1 : 0);

			} else if(fieldType == WritableHashSet.class) {
				result.put(fieldNames[i], ((WritableHashSet)field.get(obj)).serializeWithMetadata());

			} else {
				throw new UnexpectedInternalStateException();
			}
		}

		return result;
	}

}
