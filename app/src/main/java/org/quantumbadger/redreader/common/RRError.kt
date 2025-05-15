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

package org.quantumbadger.redreader.common

import androidx.annotation.StringRes
import androidx.compose.runtime.Immutable
import org.quantumbadger.redreader.R
import org.quantumbadger.redreader.http.FailedRequestBody

@Immutable
data class RRError @JvmOverloads constructor(
	@JvmField val title: String? = null,
	@JvmField val message: String? = null,
	@JvmField val reportable: Boolean = true,
	@JvmField val t: Throwable? = null,
	@JvmField val httpStatus: Int? = null,
	@JvmField val url: UriString? = null,
	@JvmField val debuggingContext: String? = null,
	@JvmField val responseString: String? = null,
	@JvmField val resolution: Resolution? = null
) {
	enum class Resolution(@StringRes val buttonText: Int) {
		ACCEPT_REDDIT_TERMS(buttonText = R.string.reddit_terms_error_resolution_button),
		ACCOUNTS_LIST(buttonText = R.string.options_accounts),
		RETRY(buttonText = R.string.error_resolution_button_retry)
	}

	override fun toString() = "$title: $message (http: $httpStatus, thrown: $t)"

	companion object {
		@JvmStatic
		fun createLegacy(
			title: String? = null,
			message: String? = null,
			reportable: Boolean = true,
			t: Throwable? = null,
			httpStatus: Int? = null,
			url: UriString? = null,
			debuggingContext: String? = null,
			response: Optional<FailedRequestBody> = Optional.empty(),
			resolution: Resolution? = null
		) = RRError(
			title = title,
			message = message,
			reportable = reportable,
			t = t,
			httpStatus = httpStatus,
			url = url,
			debuggingContext = debuggingContext,
			responseString = response.map(FailedRequestBody::toString).orElseNull(),
			resolution = resolution
		)

		fun create(
			title: String? = null,
			message: String? = null,
			reportable: Boolean = true,
			t: Throwable? = null,
			httpStatus: Int? = null,
			url: UriString? = null,
			debuggingContext: String? = null,
			response: FailedRequestBody? = null,
			resolution: Resolution? = null
		) = RRError(
			title = title,
			message = message,
			reportable = reportable,
			t = t,
			httpStatus = httpStatus,
			url = url,
			debuggingContext = debuggingContext,
			responseString = response?.toString(),
			resolution = resolution
		)
	}
}

sealed class Result<out T> {
	data class Ok<out T>(val value: T) : Result<T>()
	data class Err(val error: RRError) : Result<Nothing>()

	inline fun orElse(action: () -> Nothing): T {
		when (this) {
			is Err -> action()
			is Ok -> return value
		}
	}
}
