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
import android.util.Log;

import okhttp3.CacheControl;
import okhttp3.Call;
import okhttp3.ConnectionPool;
import okhttp3.Cookie;
import okhttp3.CookieJar;
import okhttp3.HttpUrl;
import okhttp3.MediaType;
import okhttp3.OkHttpClient;
import okhttp3.RequestBody;
import okhttp3.Response;
import okhttp3.ResponseBody;
import org.quantumbadger.redreader.cache.CacheRequest;
import org.quantumbadger.redreader.common.Constants;
import org.quantumbadger.redreader.common.TorCommon;
import org.quantumbadger.redreader.http.HTTPBackend;

import java.io.IOException;
import java.io.InputStream;
import java.net.InetSocketAddress;
import java.net.Proxy;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;

public class OKHTTPBackend extends HTTPBackend {

	private final OkHttpClient mClient;
	private static HTTPBackend httpBackend;

	private OKHTTPBackend() {
		final OkHttpClient.Builder builder = new OkHttpClient.Builder();

		// here we set the over18 cookie and return it whenever the url contains search
		// this is necessary to get the reddit API to return NSFW search results
		final List<Cookie> list = new ArrayList<>();
		Cookie.Builder cookieBuilder = new Cookie.Builder();

		cookieBuilder.domain("reddit.com");
		cookieBuilder.name("over18");
		cookieBuilder.value("1");
		cookieBuilder.path("/");

		list.add(cookieBuilder.build());


		final CookieJar cookieJar = new CookieJar() {
			@Override
			public void saveFromResponse(HttpUrl url, List<Cookie> cookies) {
				//LOL we do not care
			}

			@Override
			public List<Cookie> loadForRequest(HttpUrl url) {
				if (url.toString().contains("search"))
					return list;
				else return Collections.emptyList();
			}
		};

		builder.cookieJar(cookieJar);

		if(TorCommon.isTorEnabled()) {
			Proxy tor = new Proxy(Proxy.Type.HTTP, new InetSocketAddress("127.0.0.1", 8118));
			//SOCKS appears to be broken for now, Relevant: https://github.com/square/okhttp/issues/2315
			builder.proxy(tor);
		}

		builder.followRedirects(true);
		builder.followSslRedirects(true);

		builder.connectTimeout(15000, TimeUnit.SECONDS);
		builder.readTimeout(10000, TimeUnit.SECONDS);

		builder.connectionPool(new ConnectionPool(1, 5, TimeUnit.SECONDS));

		builder.retryOnConnectionFailure(true);

		mClient = builder.build();
	}

	public static synchronized HTTPBackend getHttpBackend() {
		if(httpBackend == null) {
			httpBackend = new OKHTTPBackend();
		}
		return httpBackend;
	}

	@Override
	public synchronized void recreateHttpBackend() {
		httpBackend = new OKHTTPBackend();
	}

	@Override
	public Request prepareRequest(final Context context, final RequestDetails details) {

		final okhttp3.Request.Builder builder = new okhttp3.Request.Builder();

		builder.header("User-Agent", Constants.ua(context));

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

		final AtomicReference<Call> callRef = new AtomicReference<>();

		return new Request() {

			public void executeInThisThread(final Listener listener) {

				final Call call = mClient.newCall(builder.build());
				Log.d("OK", "calling: " + call.request().url());
				callRef.set(call);


				final Response response;

				try {
					response = call.execute();
				} catch(IOException e) {
					listener.onError(CacheRequest.REQUEST_FAILURE_CONNECTION, e, null);
					Log.d("OK", "request didn't even connect: " + e.getMessage());
					return;
				} catch (Throwable t) {
					listener.onError(CacheRequest.REQUEST_FAILURE_CONNECTION, t, null);
					Log.d("OK", "request didn't even connect: " + t.getMessage());
					return;
				}

				final int status = response.code();
				Log.d("OK", "request got status: " + status);


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
					listener.onError(CacheRequest.REQUEST_FAILURE_REQUEST, null, status);
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
