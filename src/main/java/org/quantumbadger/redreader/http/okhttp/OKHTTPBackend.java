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
import android.os.Build;
import android.util.Log;
import androidx.annotation.NonNull;
import okhttp3.CacheControl;
import okhttp3.Call;
import okhttp3.ConnectionPool;
import okhttp3.ConnectionSpec;
import okhttp3.Cookie;
import okhttp3.CookieJar;
import okhttp3.HttpUrl;
import okhttp3.MediaType;
import okhttp3.MultipartBody;
import okhttp3.OkHttpClient;
import okhttp3.RequestBody;
import okhttp3.Response;
import okhttp3.ResponseBody;
import okhttp3.TlsVersion;
import org.quantumbadger.redreader.cache.CacheRequest;
import org.quantumbadger.redreader.common.Constants;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.Optional;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.common.TorCommon;
import org.quantumbadger.redreader.common.Void;
import org.quantumbadger.redreader.http.FailedRequestBody;
import org.quantumbadger.redreader.http.HTTPBackend;
import org.quantumbadger.redreader.http.LegacyTLSSocketFactory;
import org.quantumbadger.redreader.http.PostField;
import org.quantumbadger.redreader.http.body.HTTPRequestBody;
import org.quantumbadger.redreader.http.body.HTTPRequestBodyMultipart;
import org.quantumbadger.redreader.http.body.HTTPRequestBodyPostFields;
import org.quantumbadger.redreader.http.body.multipart.Part;
import org.quantumbadger.redreader.http.body.multipart.PartFormData;
import org.quantumbadger.redreader.http.body.multipart.PartFormDataBinary;

import javax.net.ssl.SSLContext;
import java.io.IOException;
import java.io.InputStream;
import java.net.InetSocketAddress;
import java.net.Proxy;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;

public class OKHTTPBackend extends HTTPBackend {

	private static final String TAG = "OKHTTPBackend";

	private final OkHttpClient mClient;
	private static HTTPBackend httpBackend;

	private OKHTTPBackend() {
		final OkHttpClient.Builder builder = new OkHttpClient.Builder();

		// Enable TLS 1.2 on legacy Android devices
		if (Build.VERSION.SDK_INT >= 16 && Build.VERSION.SDK_INT < 21) {
			try {
				final SSLContext sc = SSLContext.getInstance("TLSv1.2");
				sc.init(null, null, null);
				builder.sslSocketFactory(new LegacyTLSSocketFactory(sc.getSocketFactory()));

				final ConnectionSpec cs = new ConnectionSpec.Builder(ConnectionSpec.MODERN_TLS)
						.tlsVersions(TlsVersion.TLS_1_2)
						.build();

				final List<ConnectionSpec> specs = new ArrayList<>();
				specs.add(cs);
				specs.add(ConnectionSpec.COMPATIBLE_TLS);
				specs.add(ConnectionSpec.CLEARTEXT);

				builder.connectionSpecs(specs);

			} catch(final Exception e) {
				// Continue anyway
				Log.e(TAG, "Failed to enable TLS 1.2", e);
			}
		}

		// here we set the over18 cookie if needed, and return it whenever the url contains search
		// this is necessary to get the reddit API to return NSFW search results
		if(PrefsUtility.pref_behaviour_nsfw()) {
			final List<Cookie> list = new ArrayList<>();
			final Cookie.Builder cookieBuilder = new Cookie.Builder();

			cookieBuilder.domain("reddit.com");
			cookieBuilder.name("over18");
			cookieBuilder.value("1");
			cookieBuilder.path("/");

			list.add(cookieBuilder.build());


			final CookieJar cookieJar = new CookieJar() {
				@Override
				public void saveFromResponse(
						@NonNull final HttpUrl url,
						@NonNull final List<Cookie> cookies) {
					//LOL we do not care
				}

				@NonNull
				@Override
				public List<Cookie> loadForRequest(final HttpUrl url) {
					if(url.toString().contains("search")) {
						return list;
					} else {
						return Collections.emptyList();
					}
				}
			};

			builder.cookieJar(cookieJar);
		}

		if(TorCommon.isTorEnabled()) {
			final Proxy tor = new Proxy(
					Proxy.Type.HTTP,
					new InetSocketAddress("127.0.0.1", 8118));
			//SOCKS appears to be broken for now, Relevant: https://github.com/square/okhttp/issues/2315
			builder.proxy(tor);
		}

		builder.followRedirects(true);
		builder.followSslRedirects(true);

		builder.connectTimeout(20, TimeUnit.SECONDS);
		builder.readTimeout(20, TimeUnit.SECONDS);
		builder.writeTimeout(20, TimeUnit.SECONDS);

		builder.connectionPool(new ConnectionPool(
				10,
				30,
				TimeUnit.SECONDS));

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

		final Optional<HTTPRequestBody> requestBody
				= details.getRequestBody();

		if(requestBody.isPresent()) {

			builder.post(requestBody.get().visit(new HTTPRequestBody.Visitor<RequestBody>() {

				@Override
				public RequestBody visitRequestBody(
						@NonNull final HTTPRequestBodyPostFields body) {

					return RequestBody.create(
							MediaType.parse("application/x-www-form-urlencoded"),
							PostField.encodeList(body.getPostFields()));
				}

				@Override
				public RequestBody visitRequestBody(@NonNull final HTTPRequestBodyMultipart body) {

					final MultipartBody.Builder builder = new MultipartBody.Builder()
							.setType(MultipartBody.FORM);

					body.forEachPart(part -> part.visit(new Part.Visitor<Void>() {

						@NonNull
						@Override
						public Void visitPart(@NonNull final PartFormData part) {
							builder.addFormDataPart(part.name, part.value);
							return Void.INSTANCE;
						}

						@NonNull
						@Override
						public Void visitPart(@NonNull final PartFormDataBinary part) {

							builder.addFormDataPart(
									part.name,
									null,
									RequestBody.create(
											MediaType.parse("application/octet-stream"),
											part.value));

							return Void.INSTANCE;
						}
					}));

					return builder.build();
				}
			}));

		} else {
			builder.get();
		}

		builder.url(details.getUrl().toString());
		builder.cacheControl(CacheControl.FORCE_NETWORK);

		final AtomicBoolean cancelled = new AtomicBoolean(false);
		final AtomicReference<Call> callRef = new AtomicReference<>();

		return new Request() {

			@Override
			public void executeInThisThread(final Listener listener) {

				final Call call = mClient.newCall(builder.build());
				callRef.set(call);

				if(cancelled.get()) {
					call.cancel();
					return;
				}

				final Response response;

				try {
					response = call.execute();
				} catch(final Exception e) {
					listener.onError(
							CacheRequest.REQUEST_FAILURE_CONNECTION,
							e,
							null,
							Optional.empty());
					if(General.isSensitiveDebugLoggingEnabled()) {
						Log.i(TAG, "request didn't even connect: " + e.getMessage());
					}
					return;
				}

				try {

					final int status = response.code();
					final ResponseBody body = response.body();

					if(status == 200 || status == 202) {

						@SuppressWarnings("PMD.CloseResource") final InputStream bodyStream;

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

						if(General.isSensitiveDebugLoggingEnabled()) {
							Log.e(TAG, String.format(
									Locale.US,
									"Got HTTP error %d for %s",
									status,
									details));
						}

						Optional<FailedRequestBody> bodyBytes = Optional.empty();

						if(body != null) {
							try {
								bodyBytes = Optional.of(new FailedRequestBody(body.bytes()));
							} catch(final IOException e) {
								// Ignore
							}
						}


						listener.onError(
								CacheRequest.REQUEST_FAILURE_REQUEST,
								null,
								status,
								bodyBytes);
					}
				} finally {
					if(response.body() != null) {
						response.body().close();
					}
				}
			}

			@Override
			public void cancel() {

				cancelled.set(true);

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
