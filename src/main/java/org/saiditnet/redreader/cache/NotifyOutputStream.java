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

import java.io.FilterOutputStream;
import java.io.IOException;
import java.io.OutputStream;

public class NotifyOutputStream extends FilterOutputStream {

	private final Listener listener;

	public NotifyOutputStream(final OutputStream out, final Listener listener) {
		super(out);
		this.listener = listener;
	}

	@Override
	public void close() throws IOException {
		super.close();
		listener.onClose();
	}

	public interface Listener {
		void onClose() throws IOException;
	}
}
