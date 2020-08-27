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

package org.quantumbadger.redreader.common;

import androidx.annotation.Nullable;

public class RRError {

	@Nullable public final String title, message;
	@Nullable public final Throwable t;
	@Nullable public final Integer httpStatus;
	@Nullable public final String url;

	public RRError(@Nullable final String title, @Nullable final String message) {
		this(title, message, null, null, null);
	}

	public RRError(
			@Nullable final String title,
			@Nullable final String message,
			@Nullable final Throwable t) {
		this(title, message, t, null, null);
	}

	public RRError(
			@Nullable final String title,
			@Nullable final String message,
			@Nullable final Throwable t,
			@Nullable final Integer httpStatus,
			@Nullable final String url) {

		this.title = title;
		this.message = message;
		this.t = t;
		this.httpStatus = httpStatus;
		this.url = url;
	}
}
