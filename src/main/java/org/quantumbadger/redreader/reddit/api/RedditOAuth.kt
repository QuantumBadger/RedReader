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
package org.quantumbadger.redreader.reddit.api

import android.app.ProgressDialog
import android.content.Context
import android.content.DialogInterface
import android.net.Uri
import android.os.Build
import android.os.SystemClock
import android.util.Base64
import android.util.Log
import android.view.KeyEvent
import androidx.appcompat.app.AppCompatActivity
import com.google.android.material.dialog.MaterialAlertDialogBuilder
import org.quantumbadger.redreader.R
import org.quantumbadger.redreader.account.RedditAccount
import org.quantumbadger.redreader.account.RedditAccountManager
import org.quantumbadger.redreader.cache.CacheRequest.RequestFailureType
import org.quantumbadger.redreader.common.*
import org.quantumbadger.redreader.common.General.closeSafely
import org.quantumbadger.redreader.common.General.readWholeStreamAsUTF8
import org.quantumbadger.redreader.common.General.safeDismissDialog
import org.quantumbadger.redreader.common.General.uriFromString
import org.quantumbadger.redreader.http.FailedRequestBody
import org.quantumbadger.redreader.http.HTTPBackend
import org.quantumbadger.redreader.http.HTTPBackend.RequestDetails
import org.quantumbadger.redreader.http.PostField
import org.quantumbadger.redreader.http.body.HTTPRequestBodyPostFields
import org.quantumbadger.redreader.jsonwrap.JsonValue
import java.io.IOException
import java.io.InputStream
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.atomic.AtomicReference

object RedditOAuth {
	private const val TAG = "RedditOAuth"
    private const val REDIRECT_URI_NEW = "redreader://rr_oauth_redir"
    private const val ALL_SCOPES = ("identity edit flair history "
            + "modconfig modflair modlog modposts modwiki mysubreddits "
            + "privatemessages read report save submit subscribe vote "
            + "wikiedit wikiread account")
    private const val ACCESS_TOKEN_URL = "https://www.reddit.com/api/v1/access_token"

    @JvmStatic
	val promptUri: Uri
        get() {
            val uri = Uri.parse("https://www.reddit.com/api/v1/authorize.compact").buildUpon()
            uri.appendQueryParameter("response_type", "code")
            uri.appendQueryParameter("duration", "permanent")
            uri.appendQueryParameter("state", "Texas")
            uri.appendQueryParameter("redirect_uri", REDIRECT_URI_NEW)
            uri.appendQueryParameter("client_id", appId)
            uri.appendQueryParameter("scope", ALL_SCOPES)
            return uri.build()
        }

	private val appId: String?
		get() = PrefsUtility.pref_reddit_client_id_override() ?: GlobalConfig.appId

	private val cachedAppId = CachedStringHash { appId ?: "null" }

	fun init(context: Context) {
		try {
			val fileContents = context.assets.open("reddit_auth.txt").use {
				readWholeStreamAsUTF8(it)
			}.split("\"")

			if (fileContents.size != 3) {
				throw RuntimeException("Invalid file contents: $fileContents")
			}

			val id = fileContents[1].trim()

			if (id.isEmpty()) {
				throw RuntimeException("No ID provided in reddit_auth.txt")
			}

			GlobalConfig.appId = id

		} catch (e: Exception) {
			Log.i(TAG, "Got exception during init", e)
		}
	}

	private fun checkAccess(context: Context, user: RedditAccount?): RRError? {
		if (appId == null) {
			return RRError(
				title = "Reddit authentication failure",
				message = "If you compiled this copy of RedReader yourself, you must specify the Reddit client ID. Please see CONTRIBUTING.md for more details, or contact the developer.",
				reportable = true
			)
		}

		if (!PrefsUtility.isRedditUserAgreementAccepted()) {
			return RRError(
				title = context.getString(R.string.reddit_terms_error_title),
				message = context.getString(R.string.reddit_terms_error_message),
				resolution = RRError.Resolution.ACCEPT_REDDIT_TERMS,
				reportable = true
			)
		}

		if (user?.run(::needsRelogin) == true) {
			return RRError(
				title = context.getString(R.string.reddit_relogin_error_title),
				message = context.getString(R.string.reddit_relogin_error_message),
				resolution = RRError.Resolution.ACCOUNTS_LIST,
				reportable = true
			)
		}

		return null
	}

	@JvmStatic
	fun needsRelogin(user: RedditAccount) = !user.isAnonymous && user.clientId != cachedAppId.hash

	@JvmStatic
	fun anyNeedRelogin(context: Context) =
		RedditAccountManager.getInstance(context).accounts.any(this::needsRelogin)

    private fun handleRefreshTokenError(
        exception: Throwable?,
        httpStatus: Int?,
        context: Context,
        uri: String
    ): FetchRefreshTokenResult {
        return if (httpStatus != null && httpStatus != 200) {
            FetchRefreshTokenResult(
                FetchRefreshTokenResultStatus.UNKNOWN_ERROR,
                RRError(
                    context.getString(R.string.error_unknown_title),
                    context.getString(R.string.message_cannotlogin),
                    true,
                    null,
                    httpStatus,
                    uri,
                    null
                )
            )
        } else if (exception is IOException) {
            FetchRefreshTokenResult(
                FetchRefreshTokenResultStatus.CONNECTION_ERROR,
                RRError(
                    context.getString(R.string.error_connection_title),
                    context.getString(R.string.error_connection_message),
                    true,
                    exception,
                    null,
                    uri,
                    null
                )
            )
        } else {
            FetchRefreshTokenResult(
                FetchRefreshTokenResultStatus.UNKNOWN_ERROR,
                RRError(
                    context.getString(R.string.error_unknown_title),
                    context.getString(R.string.error_unknown_message),
                    true,
                    exception,
                    null,
                    uri,
                    null
                )
            )
        }
    }

    private fun handleAccessTokenError(
        exception: Throwable?,
        httpStatus: Int?,
        context: Context,
        uri: String
    ): FetchAccessTokenResult {
        return if (httpStatus != null && httpStatus != 200) {
            FetchAccessTokenResult(
                FetchAccessTokenResultStatus.UNKNOWN_ERROR,
                RRError(
                    context.getString(R.string.error_unknown_title),
                    context.getString(R.string.message_cannotlogin),
                    true,
                    null,
                    httpStatus,
                    uri,
                    null
                )
            )
        } else if (exception is IOException) {
            FetchAccessTokenResult(
                FetchAccessTokenResultStatus.CONNECTION_ERROR,
                RRError(
                    context.getString(R.string.error_connection_title),
                    context.getString(R.string.error_connection_message),
                    true,
                    exception,
                    null,
                    uri,
                    null
                )
            )
        } else {
            FetchAccessTokenResult(
                FetchAccessTokenResultStatus.UNKNOWN_ERROR,
                RRError(
                    context.getString(R.string.error_unknown_title),
                    context.getString(R.string.error_unknown_message),
                    true,
                    exception,
                    null,
                    uri,
                    null
                )
            )
        }
    }

    private fun fetchRefreshTokenSynchronous(
        context: Context,
        redirectUri: Uri
    ): FetchRefreshTokenResult {

		checkAccess(context, null)?.apply {
			return FetchRefreshTokenResult(FetchRefreshTokenResultStatus.INVALID_REQUEST, this)
		}

        val error = redirectUri.getQueryParameter("error")
        if (error != null) {
            return if (error == "access_denied") {
                FetchRefreshTokenResult(
                    FetchRefreshTokenResultStatus.USER_REFUSED_PERMISSION,
                    RRError(
                        context.getString(
                            R.string.error_title_login_user_denied_permission
                        ),
                        context.getString(
                            R.string.error_message_login_user_denied_permission
                        ),
                        false
                    )
                )
            } else {
                FetchRefreshTokenResult(
                    FetchRefreshTokenResultStatus.INVALID_REQUEST,
                    RRError(
                        context.getString(
                            R.string.error_title_login_unknown_reddit_error,
                            error
                        ),
                        context.getString(R.string.error_unknown_message),
                        true
                    )
                )
            }
        }
        val code = redirectUri.getQueryParameter("code")
            ?: return FetchRefreshTokenResult(
                FetchRefreshTokenResultStatus.INVALID_RESPONSE,
                RRError(
                    context.getString(R.string.error_unknown_title),
                    context.getString(R.string.error_unknown_message),
                    true
                )
            )
        val uri = ACCESS_TOKEN_URL
        val postFields = ArrayList<PostField>(3)
        postFields.add(PostField("grant_type", "authorization_code"))
        postFields.add(PostField("code", code))
        postFields.add(PostField("redirect_uri", REDIRECT_URI_NEW))
        return try {
            val request = HTTPBackend.getBackend().prepareRequest(
                context,
                RequestDetails(
                    uriFromString(uri)!!,
                    Optional.of(HTTPRequestBodyPostFields(postFields))
                )
            )
            request.addHeader(
                "Authorization",
                "Basic " + Base64.encodeToString(
                    ("$appId:").toByteArray(),
                    Base64.URL_SAFE or Base64.NO_WRAP
                )
            )
            val result = AtomicReference<FetchRefreshTokenResult>()
            request.executeInThisThread(object : HTTPBackend.Listener {
                override fun onError(
                    @RequestFailureType failureType: Int,
                    exception: Throwable?,
                    httpStatus: Int?,
                    body: Optional<FailedRequestBody>
                ) {
                    result.set(
                        handleRefreshTokenError(
                            exception,
                            httpStatus,
                            context,
                            uri
                        )
                    )
                }

                override fun onSuccess(
                    mimetype: String,
                    bodyBytes: Long,
                    body: InputStream
                ) {
                    try {
                        val jsonValue = JsonValue.parse(body)
                        val responseObject = jsonValue.asObject()
                        val refreshToken = RefreshToken(responseObject!!.getString("refresh_token"))
                        val accessToken = AccessToken(
                            responseObject.getString("access_token")
                        )
                        result.set(
                            FetchRefreshTokenResult(
                                refreshToken,
                                accessToken
                            )
                        )
                    } catch (e: IOException) {
                        result.set(
                            FetchRefreshTokenResult(
                                FetchRefreshTokenResultStatus.CONNECTION_ERROR,
                                RRError(
                                    context.getString(R.string.error_connection_title),
                                    context.getString(R.string.error_connection_message),
                                    true,
                                    e,
                                    null,
                                    uri,
                                    null
                                )
                            )
                        )
                    } catch (t: Throwable) {
                        throw RuntimeException(t)
                    } finally {
                        closeSafely(body)
                    }
                }
            })
            result.get()
        } catch (t: Throwable) {
            FetchRefreshTokenResult(
                FetchRefreshTokenResultStatus.UNKNOWN_ERROR,
                RRError(
                    context.getString(R.string.error_unknown_title),
                    context.getString(R.string.error_unknown_message),
                    true,
                    t,
                    null,
                    uri,
                    null
                )
            )
        }
    }

    private fun fetchUserInfoSynchronous(
        context: Context,
        accessToken: AccessToken?
    ): FetchUserInfoResult {
        val uri = Constants.Reddit.getUri(Constants.Reddit.PATH_ME)
        return try {
            val request = HTTPBackend.getBackend()
                .prepareRequest(context, RequestDetails(uri, Optional.empty()))
            request.addHeader("Authorization", "bearer " + accessToken!!.token)
            val result = AtomicReference<FetchUserInfoResult>()
            request.executeInThisThread(object : HTTPBackend.Listener {
                override fun onError(
                    @RequestFailureType failureType: Int,
                    exception: Throwable?,
                    httpStatus: Int?,
                    body: Optional<FailedRequestBody>
                ) {
                    if (httpStatus != null && httpStatus != 200) {
                        result.set(
                            FetchUserInfoResult(
                                FetchUserInfoResultStatus.CONNECTION_ERROR,
                                RRError(
                                    context.getString(R.string.error_unknown_title),
                                    context.getString(R.string.error_unknown_message),
                                    true,
                                    null,
                                    httpStatus,
                                    uri.toString(),
                                    null,
                                    body
                                )
                            )
                        )
                    } else {
                        result.set(
                            FetchUserInfoResult(
                                FetchUserInfoResultStatus.UNKNOWN_ERROR,
                                RRError(
                                    context.getString(R.string.error_unknown_title),
                                    context.getString(R.string.error_unknown_message),
                                    true,
                                    exception,
                                    null,
                                    uri.toString(),
                                    null,
                                    body
                                )
                            )
                        )
                    }
                }

                override fun onSuccess(
                    mimetype: String,
                    bodyBytes: Long,
                    body: InputStream
                ) {
                    try {
                        val jsonValue = JsonValue.parse(body)
                        val responseObject = jsonValue.asObject()
                        val username = responseObject!!.getString("name")
                        if (username.isNullOrEmpty()) {
                            result.set(
                                FetchUserInfoResult(
                                    FetchUserInfoResultStatus.INVALID_RESPONSE,
                                    RRError(
                                        context.getString(R.string.error_unknown_title),
                                        context.getString(R.string.error_unknown_message),
                                        true,
                                        null,
                                        null,
                                        uri.toString(),
                                        null,
                                        Optional.of(FailedRequestBody(jsonValue))
                                    )
                                )
                            )
                            return
                        }
                        result.set(FetchUserInfoResult(username))
                    } catch (e: IOException) {
                        result.set(
                            FetchUserInfoResult(
                                FetchUserInfoResultStatus.CONNECTION_ERROR,
                                RRError(
                                    context.getString(R.string.error_connection_title),
                                    context.getString(R.string.error_connection_message),
                                    true,
                                    e,
                                    null,
                                    uri.toString(),
                                    null
                                )
                            )
                        )
                    } catch (t: Throwable) {
                        throw RuntimeException(t)
                    } finally {
                        closeSafely(body)
                    }
                }
            })
            result.get()
        } catch (t: Throwable) {
            FetchUserInfoResult(
                FetchUserInfoResultStatus.UNKNOWN_ERROR,
                RRError(
                    context.getString(R.string.error_unknown_title),
                    context.getString(R.string.error_unknown_message),
                    true,
                    t,
                    null,
                    uri.toString(),
                    null
                )
            )
        }
    }

    private fun loginAsynchronous(
        context: Context,
        redirectUri: Uri,
        listener: LoginListener
    ) {
        object : Thread() {
            override fun run() {
                try {
                    val fetchRefreshTokenResult = fetchRefreshTokenSynchronous(context, redirectUri)
                    if (fetchRefreshTokenResult.status
                        != FetchRefreshTokenResultStatus.SUCCESS
                    ) {
                        listener.onLoginFailure(
                            LoginError.fromFetchRefreshTokenStatus(
                                fetchRefreshTokenResult.status
                            ),
                            fetchRefreshTokenResult.error
                        )
                        return
                    }
                    val fetchUserInfoResult = fetchUserInfoSynchronous(
                        context,
                        fetchRefreshTokenResult.accessToken
                    )
                    if (fetchUserInfoResult.status != FetchUserInfoResultStatus.SUCCESS) {
                        listener.onLoginFailure(
                            LoginError.fromFetchUserInfoStatus(fetchUserInfoResult.status),
                            fetchUserInfoResult.error
                        )
                        return
                    }
                    val account = RedditAccount(
                        fetchUserInfoResult.username!!,
                        fetchRefreshTokenResult.refreshToken,
                        0,
						cachedAppId.hash
                    )
                    account.setAccessToken(fetchRefreshTokenResult.accessToken)
                    val accountManager = RedditAccountManager.getInstance(context)
                    accountManager.addAccount(account)
                    accountManager.defaultAccount = account
                    listener.onLoginSuccess(account)
                } catch (t: Throwable) {
                    listener.onLoginFailure(
                        LoginError.UNKNOWN_ERROR,
                        RRError(
                            context.getString(R.string.error_unknown_title),
                            context.getString(R.string.error_unknown_message),
                            true,
                            t
                        )
                    )
                }
            }
        }.start()
    }

    @JvmStatic
	fun fetchAccessTokenSynchronous(
        context: Context,
        user: RedditAccount
    ): FetchAccessTokenResult {

		checkAccess(context, user)?.apply {
			return FetchAccessTokenResult(FetchAccessTokenResultStatus.INVALID_REQUEST, this)
		}

        val uri = ACCESS_TOKEN_URL
        val postFields = ArrayList<PostField>(2)
        postFields.add(PostField("grant_type", "refresh_token"))
        postFields.add(PostField("refresh_token", user.refreshToken.token))
        return try {
            val request = HTTPBackend.getBackend()
                .prepareRequest(
                    context,
                    RequestDetails(
                        uriFromString(uri)!!,
                        Optional.of(HTTPRequestBodyPostFields(postFields))
                    )
                )
            request.addHeader(
                "Authorization",
                "Basic " + Base64.encodeToString(
					"$appId:".toByteArray(),
                    Base64.URL_SAFE or Base64.NO_WRAP
                )
            )
            val result = AtomicReference<FetchAccessTokenResult>()
            request.executeInThisThread(object : HTTPBackend.Listener {
                override fun onError(
                    @RequestFailureType failureType: Int,
                    exception: Throwable?,
                    httpStatus: Int?,
                    body: Optional<FailedRequestBody>
                ) {
                    result.set(
                        handleAccessTokenError(
                            exception,
                            httpStatus,
                            context,
                            uri
                        )
                    )
                }

                override fun onSuccess(
                    mimetype: String,
                    bodyBytes: Long,
                    body: InputStream
                ) {
                    try {
                        val jsonValue = JsonValue.parse(body)
                        val responseObject = jsonValue.asObject()
                        val accessTokenString = responseObject!!.getString("access_token")
                            ?: throw RuntimeException(
                                "Null access token: "
                                        + responseObject.getString("error")
                            )
                        val accessToken = AccessToken(accessTokenString)
                        result.set(FetchAccessTokenResult(accessToken))
                    } catch (e: IOException) {
                        result.set(
                            FetchAccessTokenResult(
                                FetchAccessTokenResultStatus.CONNECTION_ERROR,
                                RRError(
                                    context.getString(R.string.error_connection_title),
                                    context.getString(R.string.error_connection_message),
                                    true,
                                    e,
                                    null,
                                    uri,
                                    null
                                )
                            )
                        )
                    } catch (t: Throwable) {
                        throw RuntimeException(t)
                    } finally {
                        closeSafely(body)
                    }
                }
            })
            result.get()
        } catch (t: Throwable) {
            FetchAccessTokenResult(
                FetchAccessTokenResultStatus.UNKNOWN_ERROR,
                RRError(
                    context.getString(R.string.error_unknown_title),
                    context.getString(R.string.error_unknown_message),
                    true,
                    t,
                    null,
                    uri,
                    null
                )
            )
        }
    }

    @JvmStatic
	fun fetchAnonymousAccessTokenSynchronous(
        context: Context
    ): FetchAccessTokenResult {

		checkAccess(context, RedditAccountManager.getAnon())?.apply {
			return FetchAccessTokenResult(FetchAccessTokenResultStatus.INVALID_REQUEST, this)
		}

        val uri = ACCESS_TOKEN_URL
        val postFields = ArrayList<PostField>(2)
        postFields.add(
            PostField(
                "grant_type",
                "https://oauth.reddit.com/grants/installed_client"
            )
        )
        postFields.add(
            PostField(
                "device_id",
                "DO_NOT_TRACK_THIS_DEVICE"
            )
        )
        return try {
            val request = HTTPBackend.getBackend()
                .prepareRequest(
                    context,
                    RequestDetails(
                        uriFromString(uri)!!,
                        Optional.of(HTTPRequestBodyPostFields(postFields))
                    )
                )
            request.addHeader(
                "Authorization",
                "Basic " + Base64.encodeToString(
                    "$appId:".toByteArray(),
                    Base64.URL_SAFE or Base64.NO_WRAP
                )
            )
            val result = AtomicReference<FetchAccessTokenResult>()
            request.executeInThisThread(object : HTTPBackend.Listener {
                override fun onError(
                    @RequestFailureType failureType: Int,
                    exception: Throwable?,
                    httpStatus: Int?,
                    body: Optional<FailedRequestBody>
                ) {
                    result.set(
                        handleAccessTokenError(
                            exception,
                            httpStatus,
                            context,
                            uri
                        )
                    )
                }

                override fun onSuccess(
                    mimetype: String,
                    bodyBytes: Long,
                    body: InputStream
                ) {
                    try {
                        val jsonValue = JsonValue.parse(body)
                        val responseObject = jsonValue.asObject()
                        val accessTokenString = responseObject!!.getString("access_token")
                            ?: throw RuntimeException(
                                "Null access token: "
                                        + responseObject.getString("error")
                            )
                        val accessToken = AccessToken(accessTokenString)
                        result.set(FetchAccessTokenResult(accessToken))
                    } catch (e: IOException) {
                        result.set(
                            FetchAccessTokenResult(
                                FetchAccessTokenResultStatus.CONNECTION_ERROR,
                                RRError(
                                    context.getString(R.string.error_connection_title),
                                    context.getString(R.string.error_connection_message),
                                    true,
                                    e,
                                    null,
                                    uri,
                                    null
                                )
                            )
                        )
                    } catch (t: Throwable) {
                        throw RuntimeException(t)
                    } finally {
                        closeSafely(body)
                    }
                }
            })
            result.get()
        } catch (t: Throwable) {
            FetchAccessTokenResult(
                FetchAccessTokenResultStatus.UNKNOWN_ERROR,
                RRError(
                    context.getString(R.string.error_unknown_title),
                    context.getString(R.string.message_cannotlogin),
                    true,
                    t,
                    null,
                    uri,
                    null
                )
            )
        }
    }

    @JvmStatic
	fun completeLogin(
        activity: AppCompatActivity,
        uri: Uri,
        onDone: RunnableOnce
    ) {
        val progressDialog = ProgressDialog(activity)
        progressDialog.setTitle(R.string.accounts_loggingin)
        progressDialog.setMessage(
            activity.applicationContext.getString(
                R.string.accounts_loggingin_msg
            )
        )
        progressDialog.isIndeterminate = true
        progressDialog.setCancelable(true)
        progressDialog.setCanceledOnTouchOutside(false)
        val cancelled = AtomicBoolean(false)
        progressDialog.setOnCancelListener {
			if (!cancelled.getAndSet(true)) {
                safeDismissDialog(progressDialog)
                onDone.run()
            }
        }
        progressDialog.setOnKeyListener { _: DialogInterface?, keyCode: Int, _: KeyEvent? ->
            if (keyCode == KeyEvent.KEYCODE_BACK) {
                if (!cancelled.getAndSet(true)) {
                    safeDismissDialog(progressDialog)
                    onDone.run()
                }
            }
            true
        }
        progressDialog.show()
        loginAsynchronous(
            activity.applicationContext,
            uri,
            object : LoginListener {
                override fun onLoginSuccess(account: RedditAccount?) {
                    AndroidCommon.UI_THREAD_HANDLER.post {
                        if (cancelled.get()) {
                            return@post
                        }
                        safeDismissDialog(progressDialog)
                        val alertBuilder = MaterialAlertDialogBuilder(activity)
                        alertBuilder.setNeutralButton(
                            R.string.dialog_close
                        ) { _: DialogInterface?, _: Int -> onDone.run() }
                        alertBuilder.setOnCancelListener { onDone.run() }
                        if (Build.VERSION.SDK_INT >= 17) {
                            alertBuilder.setOnDismissListener { onDone.run() }
                        }
                        val context = activity.applicationContext
                        alertBuilder.setTitle(
                            context.getString(R.string.general_success)
                        )
                        alertBuilder.setMessage(
                            context.getString(R.string.message_nowloggedin)
                        )
                        alertBuilder.show()
                    }
                }

                override fun onLoginFailure(
                    error: LoginError?,
                    details: RRError?
                ) {
                    AndroidCommon.UI_THREAD_HANDLER.post {
                        if (cancelled.get()) {
                            return@post
                        }
                        safeDismissDialog(progressDialog)
                        val builder = MaterialAlertDialogBuilder(activity)
                        builder.setNeutralButton(
                            R.string.dialog_close
                        ) { _: DialogInterface?, _: Int -> onDone.run() }
                        builder.setOnCancelListener { onDone.run() }
                        if (Build.VERSION.SDK_INT >= 17) {
                            builder.setOnDismissListener { onDone.run() }
                        }
                        builder.setTitle(details!!.title)
                        builder.setMessage(details.message)
                        builder.show()
                    }
                }
            })
    }

    open class Token(@JvmField val token: String) {
        override fun toString(): String {
            return token
        }
    }

    class AccessToken(token: String?) : Token(token!!) {
        private val mMonotonicTimestamp = SystemClock.elapsedRealtime()

		val isExpired: Boolean
            get() {
                val halfHourInMs = (30 * 60 * 1000).toLong()
                return mMonotonicTimestamp + halfHourInMs < SystemClock.elapsedRealtime()
            }
    }

    class RefreshToken(token: String?) : Token(token!!)
    enum class FetchRefreshTokenResultStatus {
        SUCCESS, USER_REFUSED_PERMISSION, INVALID_REQUEST, INVALID_RESPONSE, CONNECTION_ERROR, UNKNOWN_ERROR
    }

    enum class FetchUserInfoResultStatus {
        SUCCESS, INVALID_RESPONSE, CONNECTION_ERROR, UNKNOWN_ERROR
    }

    private class FetchRefreshTokenResult {
        val status: FetchRefreshTokenResultStatus
        val error: RRError?
        val refreshToken: RefreshToken?
        val accessToken: AccessToken?

        constructor(
            status: FetchRefreshTokenResultStatus,
            error: RRError?
        ) {
            this.status = status
            this.error = error
            refreshToken = null
            accessToken = null
        }

        constructor(
            refreshToken: RefreshToken?,
            accessToken: AccessToken?
        ) {
            status = FetchRefreshTokenResultStatus.SUCCESS
            error = null
            this.refreshToken = refreshToken
            this.accessToken = accessToken
        }
    }

    private class FetchUserInfoResult {
        val status: FetchUserInfoResultStatus
        val error: RRError?
        val username: String?

        constructor(
            status: FetchUserInfoResultStatus,
            error: RRError?
        ) {
            this.status = status
            this.error = error
            username = null
        }

        constructor(username: String?) {
            status = FetchUserInfoResultStatus.SUCCESS
            error = null
            this.username = username
        }
    }

    enum class LoginError {
        SUCCESS, USER_REFUSED_PERMISSION, CONNECTION_ERROR, UNKNOWN_ERROR;

        companion object {
            fun fromFetchRefreshTokenStatus(status: FetchRefreshTokenResultStatus?) =
				when (status) {
					FetchRefreshTokenResultStatus.SUCCESS -> SUCCESS
					FetchRefreshTokenResultStatus.USER_REFUSED_PERMISSION -> USER_REFUSED_PERMISSION
					FetchRefreshTokenResultStatus.INVALID_REQUEST -> UNKNOWN_ERROR
					FetchRefreshTokenResultStatus.INVALID_RESPONSE -> UNKNOWN_ERROR
					FetchRefreshTokenResultStatus.CONNECTION_ERROR -> CONNECTION_ERROR
					FetchRefreshTokenResultStatus.UNKNOWN_ERROR -> UNKNOWN_ERROR
					else -> UNKNOWN_ERROR
				}

            fun fromFetchUserInfoStatus(status: FetchUserInfoResultStatus?) =
				when (status) {
					FetchUserInfoResultStatus.SUCCESS -> SUCCESS
					FetchUserInfoResultStatus.INVALID_RESPONSE -> UNKNOWN_ERROR
					FetchUserInfoResultStatus.CONNECTION_ERROR -> CONNECTION_ERROR
					FetchUserInfoResultStatus.UNKNOWN_ERROR -> UNKNOWN_ERROR
					else -> UNKNOWN_ERROR
				}
        }
    }

    interface LoginListener {
        fun onLoginSuccess(account: RedditAccount?)
        fun onLoginFailure(error: LoginError?, details: RRError?)
    }

    enum class FetchAccessTokenResultStatus {
        SUCCESS, INVALID_REQUEST, INVALID_RESPONSE, CONNECTION_ERROR, UNKNOWN_ERROR
    }

    class FetchAccessTokenResult {
        @JvmField
		val status: FetchAccessTokenResultStatus
        @JvmField
		val error: RRError?
        @JvmField
		val accessToken: AccessToken?

        constructor(
            status: FetchAccessTokenResultStatus,
            error: RRError?
        ) {
            this.status = status
            this.error = error
            accessToken = null
        }

        constructor(accessToken: AccessToken?) {
            status = FetchAccessTokenResultStatus.SUCCESS
            error = null
            this.accessToken = accessToken
        }
    }
}
