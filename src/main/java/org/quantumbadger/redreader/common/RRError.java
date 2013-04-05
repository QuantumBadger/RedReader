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

import org.apache.http.StatusLine;

public class RRError {

	public final String title, message;
	public final Throwable t;
	public final StatusLine httpStatus;

	public RRError(String title, String message) {
		this(title, message, null, null);
	}

	public RRError(String title, String message, Throwable t) {
		this(title, message, t, null);
	}

	public RRError(String title, String message, Throwable t, StatusLine httpStatus) {

		this.title = title;
		this.message = message;
		this.t = t;
		this.httpStatus = httpStatus;
	}
}
