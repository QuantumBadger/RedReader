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

package org.quantumbadger.redreader.http

import android.content.Context
import org.quantumbadger.redreader.cache.CacheRequest.RequestFailureType
import org.quantumbadger.redreader.common.Result
import org.quantumbadger.redreader.common.UriString
import org.quantumbadger.redreader.http.body.HTTPRequestBody
import org.quantumbadger.redreader.http.okhttp.OKHTTPBackend
import java.io.InputStream

abstract class HTTPBackend {
    data class RequestDetails(
        val url: UriString,
        val requestBody: HTTPRequestBody?
    )

    interface Request {
        fun executeInThisThread(listener: Listener)
        fun cancel()
        fun addHeader(name: String, value: String)
    }

    interface Listener {
        fun onError(
            failureType: RequestFailureType,
            exception: Throwable?,
            httpStatus: Int?,
            body: FailedRequestBody?
        )

        fun onSuccess(mimetype: String?, bodyBytes: Long?, body: InputStream?)
    }

    abstract fun resolveRedirectUri(
        context: Context,
        url: UriString
    ): Result<UriString>

    abstract fun prepareRequest(context: Context, details: RequestDetails): Request

    abstract fun recreateHttpBackend()

    companion object {
        @JvmStatic
		val backend: HTTPBackend
            get() = OKHTTPBackend.getHttpBackend()
    }
}
