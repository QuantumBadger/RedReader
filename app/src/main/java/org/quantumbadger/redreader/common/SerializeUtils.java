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

package org.quantumbadger.redreader.common;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import com.github.luben.zstd.Zstd;
import com.github.luben.zstd.ZstdInputStream;

import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

public final class SerializeUtils {

	private static final int COMPRESSED_FILE_VERSION = 1;

	@NonNull private static final byte[] COMPRESSED_FILE_USER_HEADER
			= "RedReader compressed data\r\n".getBytes(General.CHARSET_UTF8);

	private enum DataType {
		NULL(0),
		BYTE(1),
		CHAR(2),
		SHORT(3),
		INT(4),
		LONG(5),
		FLOAT(6),
		DOUBLE(7),
		SET(8),
		LIST(9),
		MAP(10),
		STRING(11),
		BOOLEAN(12);

		final byte constant;

		DataType(final int constant) {
			this.constant = (byte)constant;
		}

		@NonNull
		public static DataType fromConstant(final byte value) throws UnhandledTypeException {

			if(value < 0 || value >= values().length) {
				throw new UnhandledTypeException("Unknown type constant " + (int)value);
			}

			return values()[value];
		}
	}

	public static class UnhandledTypeException extends Exception {
		public UnhandledTypeException(final String message) {
			super(message);
		}
	}

	private SerializeUtils() {}

	private static boolean isInvalidHashKey(@Nullable final Object value) {

		if(value == null) {
			return false;
		}

		return !(value instanceof String
				|| value instanceof Byte
				|| value instanceof Character
				|| value instanceof Short
				|| value instanceof Integer
				|| value instanceof Long
				|| value instanceof Boolean);
	}

	public static void serialize(
			@NonNull final DataOutputStream destination,
			@Nullable final Object value) throws IOException, UnhandledTypeException {

		if(value == null) {
			destination.writeByte(DataType.NULL.constant);

		} else if(value instanceof Byte) {
			destination.writeByte(DataType.BYTE.constant);
			destination.writeByte((Byte)value);

		} else if(value instanceof Character) {
			destination.writeByte(DataType.CHAR.constant);
			destination.writeChar((Character)value);

		} else if(value instanceof Short) {
			destination.writeByte(DataType.SHORT.constant);
			destination.writeShort((Short)value);

		} else if(value instanceof Integer) {
			destination.writeByte(DataType.INT.constant);
			destination.writeInt((Integer)value);

		} else if(value instanceof Long) {
			destination.writeByte(DataType.LONG.constant);
			destination.writeLong((Long)value);

		} else if(value instanceof Float) {
			destination.writeByte(DataType.FLOAT.constant);
			destination.writeFloat((Float)value);

		} else if(value instanceof Double) {
			destination.writeByte(DataType.DOUBLE.constant);
			destination.writeDouble((Double)value);

		} else if(value instanceof Set) {

			final Set<?> set = (Set<?>)value;

			destination.writeByte(DataType.SET.constant);
			destination.writeInt(set.size());

			for(final Object obj : set) {

				if(isInvalidHashKey(obj)) {
					throw new UnhandledTypeException(
							"Invalid set entry type: " + value.getClass().getCanonicalName());
				}

				serialize(destination, obj);
			}

		} else if(value instanceof List) {

			final List<?> list = (List<?>)value;

			destination.writeByte(DataType.LIST.constant);
			destination.writeInt(list.size());

			for(final Object obj : list) {
				serialize(destination, obj);
			}

		} else if(value instanceof Map) {

			final Map<?, ?> map = (Map<?, ?>)value;

			destination.writeByte(DataType.MAP.constant);
			destination.writeInt(map.size());

			for(final Map.Entry<?, ?> entry : map.entrySet()) {

				if(isInvalidHashKey(entry.getKey())) {
					throw new UnhandledTypeException(
							"Invalid map key type: " + value.getClass().getCanonicalName());
				}

				serialize(destination, entry.getKey());
				serialize(destination, entry.getValue());
			}

		} else if(value instanceof String) {

			destination.writeByte(DataType.STRING.constant);

			final byte[] bytes = ((String)value).getBytes(General.CHARSET_UTF8);
			destination.writeInt(bytes.length);
			destination.write(bytes);

		} else if(value instanceof Boolean) {
			destination.writeByte(DataType.BOOLEAN.constant);
			destination.writeBoolean((Boolean)value);

		} else {
			throw new UnhandledTypeException(
					"Unhandled type: " + value.getClass().getCanonicalName());
		}
	}

	@Nullable
	public static Object deserialize(@NonNull final DataInputStream source)
			throws IOException, UnhandledTypeException {

		final DataType type = DataType.fromConstant(source.readByte());

		switch(type) {

			case NULL:    return null;
			case BYTE:    return source.readByte();
			case CHAR:    return source.readChar();
			case SHORT:   return source.readShort();
			case INT:     return source.readInt();
			case LONG:    return source.readLong();
			case FLOAT:   return source.readFloat();
			case DOUBLE:  return source.readDouble();
			case BOOLEAN: return source.readBoolean();

			case SET: {

				final int count = source.readInt();
				final HashSet<Object> result = new HashSet<>(count);

				for(int i = 0; i < count; i++) {
					result.add(deserialize(source));
				}

				return result;
			}

			case LIST: {

				final int count = source.readInt();
				final ArrayList<Object> result = new ArrayList<>(count);

				for(int i = 0; i < count; i++) {
					result.add(deserialize(source));
				}

				return result;
			}

			case MAP: {

				final int count = source.readInt();
				final HashMap<Object, Object> result = new HashMap<>(count);

				for(int i = 0; i < count; i++) {
					final Object key = deserialize(source);
					final Object value = deserialize(source);
					result.put(key, value);
				}

				return result;
			}

			case STRING: {

				final int byteCount = source.readInt();
				final byte[] bytes = new byte[byteCount];
				source.readFully(bytes);

				return new String(bytes, General.CHARSET_UTF8);
			}
		}

		throw new UnhandledTypeException(
				"Unhandled deserialize type: " + type);
	}

	public static void serializeCompressed(
			@NonNull final DataOutputStream destination,
			@Nullable final Object value) throws IOException, UnhandledTypeException {

		final ByteArrayOutputStream data = new ByteArrayOutputStream();
		serialize(new DataOutputStream(data), value);

		final byte[] uncompressedBytes = data.toByteArray();

		final long maxDestSize = Zstd.compressBound(uncompressedBytes.length);

		if(maxDestSize > Integer.MAX_VALUE) {
			throw new IOException("Max output size is greater than MAX_INT");
		}

		final byte[] compressedBytes = new byte[(int)maxDestSize];

		final int compressedSize = (int)Zstd.compressByteArray(
				compressedBytes,
				0,
				compressedBytes.length,
				uncompressedBytes,
				0,
				uncompressedBytes.length,
				3);

		destination.write(COMPRESSED_FILE_USER_HEADER);
		destination.writeInt(COMPRESSED_FILE_VERSION);

		destination.writeInt(compressedSize);
		destination.write(compressedBytes, 0, compressedSize);
	}

	@Nullable
	public static Object deserializeCompressed(@NonNull final DataInputStream source)
			throws IOException, UnhandledTypeException {

		final byte[] userHeader = new byte[COMPRESSED_FILE_USER_HEADER.length];
		source.readFully(userHeader);

		if(!Arrays.equals(userHeader, COMPRESSED_FILE_USER_HEADER)) {
			throw new IOException("Invalid user header");
		}

		final int version = source.readInt();
		final int compressedBytesLength = source.readInt();

		if(version != COMPRESSED_FILE_VERSION) {
			throw new IOException("Unsupported version " + version);
		}

		final byte[] compressedData = new byte[compressedBytesLength];
		source.readFully(compressedData);

		return deserialize(new DataInputStream(new BufferedInputStream(
				new ZstdInputStream(new ByteArrayInputStream(compressedData)))));
	}
}
