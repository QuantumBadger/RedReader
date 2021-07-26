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

package org.quantumbadger.redreader.http;

import android.content.Context;
import androidx.annotation.NonNull;
import org.quantumbadger.redreader.cache.CacheRequest;
import org.quantumbadger.redreader.common.Optional;
import org.quantumbadger.redreader.http.body.HTTPRequestBody;
import org.quantumbadger.redreader.http.okhttp.OKHTTPBackend;

import java.io.InputStream;
import java.net.URI;

public abstract class HTTPBackend {
	/**
	 * Factory method can read configuration information to choose a backend
	 */
	public static HTTPBackend getBackend() {
		return OKHTTPBackend.getHttpBackend();
	}

	public static class RequestDetails {

		@NonNull private final URI mUrl;
		@NonNull private final Optional<HTTPRequestBody> mRequestBody;

		public RequestDetails(
				@NonNull final URI url,
				@NonNull final Optional<HTTPRequestBody> requestBody) {
			mUrl = url;
			mRequestBody = requestBody;
		}

		@NonNull
		public URI getUrl() {
			return mUrl;
		}

		@NonNull
		public Optional<HTTPRequestBody> getRequestBody() {
			return mRequestBody;
		}

		@NonNull
		@Override
		public String toString() {
			return "RequestDetails("
					+ mUrl
					+ ", "
					+ mRequestBody
					+ ")";
		}
	}

	public interface Request {
		void executeInThisThread(final Listener listener);

		void cancel();

		void addHeader(String name, String value);
	}

	public interface Listener {
		void onError(
				@CacheRequest.RequestFailureType int failureType,
				Throwable exception,
				Integer httpStatus,
				@NonNull Optional<FailedRequestBody> body);

		void onSuccess(String mimetype, Long bodyBytes, InputStream body);
	}

	public abstract Request prepareRequest(Context context, RequestDetails details);

	public abstract void recreateHttpBackend();

}
