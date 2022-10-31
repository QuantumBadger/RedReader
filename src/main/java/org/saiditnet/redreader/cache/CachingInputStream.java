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

package org.saiditnet.redreader.cache;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

final class CachingInputStream extends InputStream {

	private final InputStream in;
	private final OutputStream out;

	private long bytesRead = 0;
	private final BytesReadListener listener;

	private boolean stillRunning = true;

	public CachingInputStream(final InputStream in, final OutputStream out, final BytesReadListener listener) {
		this.in = in;
		this.out = out;
		this.listener = listener;
	}

	public interface BytesReadListener {
		void onBytesRead(long total);
	}

	private void notifyOnBytesRead() {
		if(listener != null) listener.onBytesRead(bytesRead);
	}

	@Override
	public void close() throws IOException {

		if(stillRunning) {
			in.close();
			throw new RuntimeException("Closing CachingInputStream before the input stream has ended");
		}
	}

	@Override
	public int read() throws IOException {

		final int byteRead = in.read();

		if(byteRead >= 0) {
			out.write(byteRead);
			bytesRead++;
			notifyOnBytesRead();

		} else {
			terminate();
		}

		return byteRead;
	}

	@Override
	public int read(final byte[] buffer, final int offset, final int length) throws IOException {

		final int result = in.read(buffer, offset, length);

		if(result > 0) {
			out.write(buffer, offset, result);
			bytesRead += result;
			notifyOnBytesRead();

		} else {
			terminate();
		}

		return result;
	}

	private void terminate() throws IOException {

		if(stillRunning) {
			stillRunning = false;
			out.flush();
			out.close();
			in.close();
		}
	}
}
