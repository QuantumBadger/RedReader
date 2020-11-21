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

import java.io.IOException;
import java.io.InputStream;

public abstract class SeekableInputStream extends InputStream {

	private int mMark;

	public abstract int getPosition();

	public abstract void seek(int position) throws IOException;

	@Override
	public final void mark(final int readlimit) {
		mMark = getPosition();
	}

	@Override
	public final void reset() throws IOException {
		seek(mMark);
	}

	@Override
	public final boolean markSupported() {
		return true;
	}
}
