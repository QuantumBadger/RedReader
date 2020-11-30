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

package org.quantumbadger.redreader.views.video;

import android.net.Uri;
import androidx.annotation.Nullable;
import com.google.android.exoplayer2.C;
import com.google.android.exoplayer2.upstream.BaseDataSource;
import com.google.android.exoplayer2.upstream.DataSpec;
import org.quantumbadger.redreader.common.GenericFactory;
import org.quantumbadger.redreader.common.datastream.SeekableInputStream;

import java.io.IOException;
import java.util.Objects;

public class ExoPlayerSeekableInputStreamDataSource extends BaseDataSource {

	public static final Uri URI = Uri.parse("redreader://video");

	private final GenericFactory<SeekableInputStream, IOException> mStreamFactory;

	@Nullable private SeekableInputStream mCurrentStream;

	protected ExoPlayerSeekableInputStreamDataSource(
			final boolean isNetwork,
			final GenericFactory<SeekableInputStream, IOException> streamFactory) {

		super(isNetwork);
		mStreamFactory = streamFactory;
	}

	@Override
	public long open(final DataSpec dataSpec) throws IOException {

		if(mCurrentStream != null) {
			throw new IOException("Already open!");
		}

		transferInitializing(dataSpec);

		mCurrentStream = mStreamFactory.create();
		mCurrentStream.seek(dataSpec.position);

		transferStarted(dataSpec);

		return C.LENGTH_UNSET;
	}

	@Override
	public int read(
			final byte[] buffer,
			final int offset,
			final int readLength) throws IOException {

		if(readLength == 0) {
			return 0;
		}

		final int result = Objects.requireNonNull(mCurrentStream).read(buffer, offset, readLength);

		if(result < 0) {
			return C.RESULT_END_OF_INPUT;
		}

		bytesTransferred(result);
		return result;
	}

	@Nullable
	@Override
	public Uri getUri() {
		return URI;
	}

	@Override
	public void close() throws IOException {

		if(mCurrentStream != null) {
			mCurrentStream.close();
			mCurrentStream = null;
			transferEnded();
		}
	}
}
