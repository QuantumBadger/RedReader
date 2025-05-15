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

package org.quantumbadger.redreader.receivers.announcements;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

public final class Payload {

	private final HashMap<String, String> mStrings = new HashMap<>();
	private final HashMap<String, Long> mLongs = new HashMap<>();
	private final HashMap<String, Boolean> mBooleans = new HashMap<>();

	private static final byte HEADER_EOF = 0;
	private static final byte HEADER_ENTRY_STRING = 1;
	private static final byte HEADER_ENTRY_LONG = 2;
	private static final byte HEADER_ENTRY_BOOLEAN = 3;

	public void setString(@NonNull final String key, @NonNull final String value) {
		mStrings.put(key, value);
	}

	public void setLong(@NonNull final String key, final long value) {
		mLongs.put(key, value);
	}

	public void setBoolean(@NonNull final String key, final boolean value) {
		mBooleans.put(key, value);
	}

	@Nullable
	public String getString(@NonNull final String key) {
		return mStrings.get(key);
	}

	@Nullable
	public Long getLong(@NonNull final String key) {
		return mLongs.get(key);
	}

	@Nullable
	public Boolean getBoolean(@NonNull final String key) {
		return mBooleans.get(key);
	}

	@NonNull
	public byte[] toBytes() {

		final ByteArrayOutputStream result = new ByteArrayOutputStream();
		final DataOutputStream dos = new DataOutputStream(result);

		try {
			for(final Map.Entry<String, String> entry : mStrings.entrySet()) {

				if(entry.getValue() == null) {
					continue;
				}
				dos.writeByte(HEADER_ENTRY_STRING);
				dos.writeUTF(entry.getKey());
				dos.writeUTF(entry.getValue());
			}

			for(final Map.Entry<String, Long> entry : mLongs.entrySet()) {

				if(entry.getValue() == null) {
					continue;
				}

				dos.writeByte(HEADER_ENTRY_LONG);
				dos.writeUTF(entry.getKey());
				dos.writeLong(entry.getValue());
			}

			for(final Map.Entry<String, Boolean> entry : mBooleans.entrySet()) {

				if(entry.getValue() == null) {
					continue;
				}

				dos.writeByte(HEADER_ENTRY_BOOLEAN);
				dos.writeUTF(entry.getKey());
				dos.writeBoolean(entry.getValue());
			}

			dos.writeByte(HEADER_EOF);

			dos.flush();
			dos.close();

		} catch(final IOException e) {
			throw new RuntimeException(e);
		}

		return result.toByteArray();
	}

	@NonNull
	public static Payload fromBytes(@NonNull final byte[] data) throws IOException {

		try(DataInputStream dis = new DataInputStream(new ByteArrayInputStream(data))) {

			final Payload result = new Payload();

			while(true) {

				final byte header = dis.readByte();

				switch(header) {
					case HEADER_EOF:
						return result;

					case HEADER_ENTRY_STRING:
						result.setString(dis.readUTF(), dis.readUTF());
						break;

					case HEADER_ENTRY_LONG:
						result.setLong(dis.readUTF(), dis.readLong());
						break;

					case HEADER_ENTRY_BOOLEAN:
						result.setBoolean(dis.readUTF(), dis.readBoolean());
						break;

					default:
						throw new IOException("Unknown entry header " + header);
				}
			}
		}
	}
}
