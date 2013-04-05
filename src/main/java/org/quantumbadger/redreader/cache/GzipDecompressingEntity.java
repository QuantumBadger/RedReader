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

package org.quantumbadger.redreader.cache;

import org.apache.http.HttpEntity;
import org.apache.http.entity.HttpEntityWrapper;

import java.io.IOException;
import java.io.InputStream;
import java.util.zip.GZIPInputStream;

/**
 * Wrapping entity that decompresses {@link #getContent content}.
 *
 * @since 4.0
 */
class GzipDecompressingEntity extends HttpEntityWrapper {

	public GzipDecompressingEntity(final HttpEntity entity) {
		super(entity);
	}

	public InputStream getContent()
			throws IOException, IllegalStateException {

		// the wrapped entity's getContent() decides about repeatability
		final InputStream wrappedin = wrappedEntity.getContent();

		return new GZIPInputStream(wrappedin);
	}

	public long getContentLength() {
		// length of ungzipped content not known in advance
		return -1;
	}

} // class GzipDecompressingEntity