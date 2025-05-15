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

package org.quantumbadger.redreader.http.okhttp

import android.content.Context
import android.util.Log
import okhttp3.CacheControl
import okhttp3.Call
import okhttp3.ConnectionPool
import okhttp3.Cookie
import okhttp3.CookieJar
import okhttp3.HttpUrl
import okhttp3.MediaType.Companion.toMediaType
import okhttp3.MultipartBody
import okhttp3.OkHttpClient
import okhttp3.RequestBody.Companion.toRequestBody
import org.quantumbadger.redreader.cache.CacheRequest
import org.quantumbadger.redreader.common.Constants
import org.quantumbadger.redreader.common.General.getGeneralErrorForFailure
import org.quantumbadger.redreader.common.General.isSensitiveDebugLoggingEnabled
import org.quantumbadger.redreader.common.Optional
import org.quantumbadger.redreader.common.PrefsUtility
import org.quantumbadger.redreader.common.RRError
import org.quantumbadger.redreader.common.Result
import org.quantumbadger.redreader.common.TorCommon
import org.quantumbadger.redreader.common.UriString
import org.quantumbadger.redreader.http.FailedRequestBody
import org.quantumbadger.redreader.http.HTTPBackend
import org.quantumbadger.redreader.http.body.HTTPRequestBody
import org.quantumbadger.redreader.http.body.multipart.Part
import java.io.IOException
import java.io.InputStream
import java.net.InetSocketAddress
import java.net.Proxy
import java.util.Locale
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.atomic.AtomicReference
import okhttp3.Request as OkHTTPRequest

class OKHTTPBackend private constructor() : HTTPBackend() {
	private val mClient: OkHttpClient

	init {
		val builder: OkHttpClient.Builder = OkHttpClient.Builder()

		// Here we set the over18 cookie if needed, and return it whenever the url contains search
		// this is necessary to get the reddit API to return NSFW search results
		if (PrefsUtility.pref_behaviour_nsfw()) {
			val list: MutableList<Cookie> = ArrayList()
			val cookieBuilder: Cookie.Builder = Cookie.Builder()

			cookieBuilder.domain("reddit.com")
			cookieBuilder.name("over18")
			cookieBuilder.value("1")
			cookieBuilder.path("/")

			list.add(cookieBuilder.build())


			val cookieJar: CookieJar = object : CookieJar {
				override fun saveFromResponse(
					url: HttpUrl,
					cookies: List<Cookie>
				) {
					//LOL we do not care
				}

				override fun loadForRequest(url: HttpUrl): List<Cookie> {
					return if (url.toString().contains("search")) {
						list
					} else {
						emptyList()
					}
				}
			}

			builder.cookieJar(cookieJar)
		}

		if (TorCommon.isTorEnabled()) {
			val tor = Proxy(
				Proxy.Type.HTTP,
				InetSocketAddress("127.0.0.1", 8118)
			)
			//SOCKS appears to be broken for now, Relevant: https://github.com/square/okhttp/issues/2315
			builder.proxy(tor)
		}

		builder.followRedirects(true)
		builder.followSslRedirects(true)

		builder.connectTimeout(20, TimeUnit.SECONDS)
		builder.readTimeout(20, TimeUnit.SECONDS)
		builder.writeTimeout(20, TimeUnit.SECONDS)

		builder.connectionPool(
			ConnectionPool(
				10,
				30,
				TimeUnit.SECONDS
			)
		)

		builder.retryOnConnectionFailure(true)

		mClient = builder.build()
	}

	@Synchronized
	override fun recreateHttpBackend() {
		httpBackend = OKHTTPBackend()
	}

	override fun resolveRedirectUri(
		context: Context,
		url: UriString
	): Result<UriString> {
		try {
			val builder: OkHTTPRequest.Builder = OkHTTPRequest.Builder()
			val headRequest: okhttp3.Request = builder.url(url.value).head().build()
			val noRedirectsClient =
				mClient.newBuilder().followRedirects(false).build()
			noRedirectsClient.newCall(headRequest).execute().use { response ->
				if (!response.isRedirect) {
					return Result.Err(
						RRError(
							"Redirect response not received",
							null,
							true,
							null,
							response.code,
							url
						)
					)
				}
				val locationHeader = response.header("Location")
					?: throw RuntimeException("Location header was null")
				return Result.Ok(UriString(locationHeader))
			}
		} catch (e: Exception) {
			return Result.Err(
				getGeneralErrorForFailure(
					context,
					CacheRequest.RequestFailureType.CONNECTION,
					e,
					null,
					url,
					Optional.empty()
				)
			)
		}
	}

	override fun prepareRequest(context: Context, details: RequestDetails): Request {
		val reqBuilder: OkHTTPRequest.Builder = OkHTTPRequest.Builder()

		reqBuilder.header("User-Agent", Constants.ua(context))

		val requestBody = details.requestBody

		if (requestBody != null) {
			reqBuilder.post(
				when (requestBody) {
					is HTTPRequestBody.Multipart -> {
						val builder = MultipartBody.Builder().setType(MultipartBody.FORM)

						requestBody.forEachPart { part: Part ->
							when (part) {
								is Part.FormData -> {
									builder.addFormDataPart(part.name, part.value)
								}

								is Part.FormDataBinary -> {
									builder.addFormDataPart(
										name = part.name,
										filename = null,
										body = part.value.toRequestBody("application/octet-stream".toMediaType())
									)
								}
							}
						}

						builder.build()
					}

					is HTTPRequestBody.PostFields -> requestBody.encodeFields()
						.toRequestBody("application/x-www-form-urlencoded".toMediaType())
				}
			)
		} else {
			reqBuilder.get()
		}

		reqBuilder.url(details.url.value)
		reqBuilder.cacheControl(CacheControl.FORCE_NETWORK)

		val cancelled = AtomicBoolean(false)
		val callRef = AtomicReference<Call?>()

		return object : Request {
			override fun executeInThisThread(listener: Listener) {
				val call = mClient.newCall(reqBuilder.build())
				callRef.set(call)

				if (cancelled.get()) {
					call.cancel()
					return
				}

				val response = try {
					call.execute()
				} catch (e: Exception) {
					listener.onError(
						CacheRequest.RequestFailureType.CONNECTION,
						e,
						null,
						null
					)
					if (isSensitiveDebugLoggingEnabled) {
						Log.i(TAG, "request didn't even connect: " + e.message)
					}
					return
				}

				try {
					val status = response.code
					val body = response.body

					if (status == 200 || status == 202) {

						val bodyStream: InputStream?
						val bodyLength: Long?

						if (body != null) {
							bodyStream = body.byteStream()
							bodyLength = body.contentLength().takeUnless { it < 0 }
						} else {
							bodyStream = null
							bodyLength = null
						}

						val contentType = response.header("Content-Type")

						listener.onSuccess(contentType, bodyLength, bodyStream)
					} else {
						if (isSensitiveDebugLoggingEnabled) {
							Log.e(
								TAG, String.format(
									Locale.US,
									"Got HTTP error %d for %s",
									status,
									details
								)
							)
						}

						var bodyBytes: FailedRequestBody? = null

						if (body != null) {
							try {
								bodyBytes = FailedRequestBody(body.bytes())
							} catch (e: IOException) {
								// Ignore
							}
						}

						listener.onError(
							CacheRequest.RequestFailureType.REQUEST,
							null,
							status,
							bodyBytes
						)
					}
				} catch(e: Exception) {
					listener.onError(
						CacheRequest.RequestFailureType.REQUEST,
						e,
						null,
						null
					)

				} finally {
					response.body?.close()
				}
			}

			override fun cancel() {
				cancelled.set(true)

				val call = callRef.getAndSet(null)
				call?.cancel()
			}

			override fun addHeader(name: String, value: String) {
				reqBuilder.addHeader(name, value)
			}
		}
	}

	companion object {
		private const val TAG = "OKHTTPBackend"

		private var httpBackend: HTTPBackend? = null

		@Synchronized
		fun getHttpBackend(): HTTPBackend {
			if (httpBackend == null) {
				httpBackend = OKHTTPBackend()
			}
			return httpBackend!!
		}
	}
}
