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
import androidx.annotation.Nullable;

import java.io.IOException;
import java.util.Arrays;

public final class MemoryDataStream {

	private final Object mLock = new Object();

	@NonNull private byte[] mData;
	private int mSize;

	@Nullable private IOException mFailed;
	private boolean mComplete;

	public MemoryDataStream() {
		this(64 * 1024);
	}

	public MemoryDataStream(final int initialCapacity) {

		if(initialCapacity < 1) {
			throw new RuntimeException("Initial capacity must be at least 1");
		}

		mData = new byte[initialCapacity];
		mSize = 0;
	}

	public MemoryDataStream(final byte[] data) {
		mData = data;
		mSize = data.length;
		mComplete = true;
	}

	private void ensureCapacity(final int desiredCapacity) {

		if(desiredCapacity <= mData.length) {
			return;
		}

		if(desiredCapacity > (mData.length * 2)) {
			realloc(desiredCapacity + (desiredCapacity / 2));

		} else {
			realloc(mData.length * 2);
		}
	}

	private void realloc(final int newCapacity) {

		if(newCapacity < mSize) {
			throw new RuntimeException("Cannot shrink array");
		}

		mData = Arrays.copyOf(mData, newCapacity);
	}

	public int size() {
		synchronized(mLock) {
			return mSize;
		}
	}

	public void writeBytes(@NonNull final byte[] data, final int offset, final int length) {

		synchronized(mLock) {
			ensureCapacity(mSize + length);
			System.arraycopy(data, offset, mData, mSize, length);
			mSize += length;
			mLock.notifyAll();
		}
	}

	public void setComplete() {
		synchronized(mLock) {
			mComplete = true;
			mLock.notifyAll();
		}
	}

	public void setFailed(@NonNull final IOException e) {
		synchronized(mLock) {
			mFailed = e;
			mLock.notifyAll();
		}
	}

	private boolean notReadyForRead(final int startingPosition) {
		return !mComplete && mFailed == null && mSize <= startingPosition;
	}

	public int blockingReadOneByte(final int position) throws IOException {

		synchronized(mLock) {

			while(notReadyForRead(position)) {
				try {
					mLock.wait();
				} catch(final InterruptedException e) {
					throw new RuntimeException(e);
				}
			}

			if(mFailed != null) {
				throw mFailed;
			}

			if(mSize > position) {
				return mData[position];
			}

			if(mComplete) {
				return -1;
			}

			throw new IOException("Internal error: ready conditions not true");
		}
	}

	public int blockingRead(
			final int startingPosition,
			@NonNull final byte[] output,
			final int offset,
			final int maxLength) throws IOException {

		if(maxLength == 0) {
			throw new RuntimeException("Attempted to read zero bytes");
		}

		synchronized(mLock) {

			while(notReadyForRead(startingPosition)) {
				try {
					mLock.wait();
				} catch(final InterruptedException e) {
					throw new RuntimeException(e);
				}
			}

			if(mFailed != null) {
				throw mFailed;
			}

			if(mSize > startingPosition) {
				final int bytesToRead = Math.min(maxLength, mSize - startingPosition);
				System.arraycopy(mData, startingPosition, output, offset, bytesToRead);
				return bytesToRead;
			}

			if(mComplete) {
				return -1;
			}

			throw new IOException("Internal error: ready conditions not true");
		}
	}

	@NonNull
	public MemoryDataStreamInputStream getInputStream() {
		return new MemoryDataStreamInputStream(this);
	}

	public void getUnderlyingByteArrayWhenComplete(
			@NonNull final ByteArrayCallback callback) throws IOException {

		synchronized(mLock) {

			while(!mComplete && mFailed == null) {
				try {
					mLock.wait();
				} catch(final InterruptedException e) {
					throw new RuntimeException(e);
				}
			}

			if(mFailed != null) {
				throw mFailed;
			}
		}

		callback.onByteArray(mData, 0, mSize);
	}
}
