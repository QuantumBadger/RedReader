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
import org.quantumbadger.redreader.common.General;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.RandomAccessFile;

public class SeekableFileInputStream extends SeekableInputStream {

	@NonNull private final RandomAccessFile mFile;
	private long mPosition;

	public SeekableFileInputStream(@NonNull final File file) throws FileNotFoundException {
		mFile = new RandomAccessFile(file, "r");
	}

	@Override
	public long getPosition() {
		return mPosition;
	}

	@Override
	public void seek(final long position) throws IOException {
		mFile.seek(position);
		mPosition = position;
	}

	@Override
	public void readRemainingAsBytes(@NonNull final ByteArrayCallback callback) throws IOException {
		final byte[] result = General.readWholeStream(this);
		callback.onByteArray(result, 0, result.length);
	}

	@Override
	public int read() throws IOException {

		final int result = mFile.read();

		if(result >= 0) {
			mPosition ++;
		}

		return result;
	}

	@Override
	public int read(final byte[] buf) throws IOException {
		return read(buf, 0, buf.length);
	}

	@Override
	public int read(final byte[] buf, final int off, final int len) throws IOException {

		if(len == 0) {
			throw new IOException("Attempted to read zero bytes");
		}

		final int result = mFile.read(buf, off, len);

		if(result > 0) {
			mPosition += result;
		}

		return result;
	}

	@Override
	public long skip(final long n) throws IOException {
		final long bytesToSkip = Math.min(n, available());
		seek((int)(mPosition + bytesToSkip));
		return bytesToSkip;
	}

	@Override
	public int available() throws IOException {
		return (int)(mFile.length() - mPosition);
	}

	@Override
	public void close() throws IOException {
		mFile.close();
	}
}
