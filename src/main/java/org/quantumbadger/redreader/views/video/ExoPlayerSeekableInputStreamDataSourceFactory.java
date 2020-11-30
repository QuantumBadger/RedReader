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

import androidx.annotation.NonNull;
import com.google.android.exoplayer2.upstream.DataSource;
import org.quantumbadger.redreader.common.GenericFactory;
import org.quantumbadger.redreader.common.datastream.SeekableInputStream;

import java.io.IOException;

public class ExoPlayerSeekableInputStreamDataSourceFactory implements DataSource.Factory {

	private final boolean mIsNetwork;
	@NonNull private final GenericFactory<SeekableInputStream, IOException> mStreamFactory;

	public ExoPlayerSeekableInputStreamDataSourceFactory(
			final boolean isNetwork,
			@NonNull final GenericFactory<SeekableInputStream, IOException> streamFactory) {

		mIsNetwork = isNetwork;
		mStreamFactory = streamFactory;
	}

	@Override
	public DataSource createDataSource() {
		return new ExoPlayerSeekableInputStreamDataSource(mIsNetwork, mStreamFactory);
	}
}
