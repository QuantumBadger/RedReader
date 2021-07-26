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

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import org.quantumbadger.redreader.http.FailedRequestBody;

public class RRError {

	@Nullable public final String title;
	@Nullable public final String message;
	public final boolean reportable;
	@Nullable public final Throwable t;
	@Nullable public final Integer httpStatus;
	@Nullable public final String url;
	@Nullable public final String debuggingContext;
	@Nullable public final String response;

	public RRError(
			@Nullable final String title,
			@Nullable final String message,
			final boolean reportable) {
		this(title, message, reportable, null);
	}

	public RRError(
			@Nullable final String title,
			@Nullable final String message,
			final boolean reportable,
			@Nullable final Throwable t) {
		this(title, message, reportable, t, null, null, null);
	}

	public RRError(
			@Nullable final String title,
			@Nullable final String message,
			final boolean reportable,
			@Nullable final Throwable t,
			@Nullable final Integer httpStatus,
			@Nullable final String url,
			@Nullable final String debuggingContext) {

		this(title, message, reportable, t, httpStatus, url, debuggingContext, Optional.empty());
	}

	public RRError(
			@Nullable final String title,
			@Nullable final String message,
			final boolean reportable,
			@Nullable final Throwable t,
			@Nullable final Integer httpStatus,
			@Nullable final String url,
			@Nullable final String debuggingContext,
			@NonNull final Optional<FailedRequestBody> response) {

		this.title = title;
		this.message = message;
		this.reportable = reportable;
		this.t = t;
		this.httpStatus = httpStatus;
		this.url = url;
		this.debuggingContext = debuggingContext;
		this.response = response.map(FailedRequestBody::toString).orElseNull();
	}
}
