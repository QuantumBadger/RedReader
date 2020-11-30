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

package org.quantumbadger.redreader.common.datastream;

import androidx.annotation.NonNull;

import java.io.IOException;

public class MemoryDataStreamInputStream extends SeekableInputStream {

	@NonNull private final MemoryDataStream mStream;
	private int mPosition;

	public MemoryDataStreamInputStream(@NonNull final MemoryDataStream stream) {
		mStream = stream;
		mPosition = 0;
	}

	@Override
	public int read() throws IOException {

		final int result = mStream.blockingReadOneByte(mPosition);

		if(result >= 0) {
			mPosition++;
		}

		return result;
	}

	@Override
	public int read(final byte[] buf) throws IOException {
		return read(buf, 0, buf.length);
	}

	@Override
	public int read(final byte[] buf, final int off, final int len) throws IOException {

		final int bytesRead = mStream.blockingRead(mPosition, buf, off, len);

		if(bytesRead > 0) {
			mPosition += bytesRead;
		}

		return bytesRead;
	}

	@Override
	public long getPosition() {
		return mPosition;
	}

	@Override
	public void seek(final long position) throws IOException {

		if(position < 0) {
			throw new IOException("Attempted to seek before zero");
		}

		mPosition = (int)position;
	}

	@Override
	public long skip(final long offset) {
		final int bytesToSkip = (int)Math.min(offset, Math.max(0, mStream.size() - mPosition));
		mPosition += bytesToSkip;
		return bytesToSkip;
	}

	@Override
	public int available() {
		return mStream.size();
	}

	@Override
	public void close() {
		// Nothing to do here
	}

	@Override
	public void readRemainingAsBytes(@NonNull final ByteArrayCallback callback) throws IOException {
		mStream.getUnderlyingByteArrayWhenComplete((buf, offset, length)
				-> callback.onByteArray(buf, offset + mPosition, length - mPosition));
	}
}
