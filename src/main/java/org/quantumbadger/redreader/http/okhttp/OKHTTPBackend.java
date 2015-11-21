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

package org.quantumbadger.redreader.http.okhttp;

import android.content.Context;
import com.squareup.okhttp.*;
import org.quantumbadger.redreader.cache.RequestFailureType;
import org.quantumbadger.redreader.http.HTTPBackend;

import java.io.IOException;
import java.io.InputStream;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;

public class OKHTTPBackend implements HTTPBackend {

	private final OkHttpClient mClient;

	public OKHTTPBackend() {

		mClient = new OkHttpClient();

		mClient.setFollowRedirects(true);
		mClient.setFollowSslRedirects(true);

		mClient.setConnectTimeout(15000, TimeUnit.SECONDS);
		mClient.setReadTimeout(10000, TimeUnit.SECONDS);

		mClient.setRetryOnConnectionFailure(true);

		// TODO Is this necessary?
		mClient.setConnectionPool(ConnectionPool.getDefault());
	}

	@Override
	public Request prepareRequest(final Context context, final RequestDetails details) {

		final com.squareup.okhttp.Request.Builder builder = new com.squareup.okhttp.Request.Builder();

		final List<PostField> postFields = details.getPostFields();

		if(postFields != null) {
			builder.post(RequestBody.create(
					MediaType.parse("application/x-www-form-urlencoded"),
					PostField.encodeList(postFields)));

		} else {
			builder.get();
		}

		builder.url(details.getUrl().toString());
		builder.cacheControl(CacheControl.FORCE_NETWORK);

		final AtomicReference<Call> callRef = new AtomicReference<Call>();

		return new Request() {

			public void executeInThisThread(final Listener listener) {

				final Call call = mClient.newCall(builder.build());
				callRef.set(call);

				try {

					final Response response;

					try {
						response = call.execute();
					} catch(IOException e) {
						listener.onError(RequestFailureType.CONNECTION, e, null);
						return;
					}

					final int status = response.code();

					if(status == 200 || status == 202) {

						final ResponseBody body = response.body();
						final InputStream bodyStream;
						final Long bodyBytes;

						if(body != null) {
							bodyStream = body.byteStream();
							bodyBytes = body.contentLength();

						} else {
							// TODO error
							bodyStream = null;
							bodyBytes = null;
						}

						final String contentType = response.header("Content-Type");

						listener.onSuccess(contentType, bodyBytes, bodyStream);

					} else {
						listener.onError(RequestFailureType.REQUEST, null, status);
					}

				} catch(Throwable t) {
					listener.onError(RequestFailureType.CONNECTION, t, null);
				}
			}

			@Override
			public void cancel() {
				final Call call = callRef.getAndSet(null);
				if(call != null) {
					call.cancel();
				}
			}

			@Override
			public void addHeader(final String name, final String value) {
				builder.addHeader(name, value);
			}
		};
	}
}
