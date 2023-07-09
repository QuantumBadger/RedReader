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

import android.app.AlertDialog
import android.app.Dialog
import android.content.Context
import android.content.DialogInterface
import android.content.Intent
import android.content.res.Configuration
import android.graphics.Color
import android.net.ConnectivityManager
import android.net.NetworkInfo
import android.net.Uri
import android.os.Looper
import android.os.Message
import android.os.SystemClock
import android.util.Log
import android.util.TypedValue
import android.view.View
import android.view.ViewGroup
import android.view.ViewGroup.MarginLayoutParams
import android.view.WindowManager.BadTokenException
import android.widget.Toast
import androidx.appcompat.app.AppCompatActivity
import org.quantumbadger.redreader.BuildConfig
import org.quantumbadger.redreader.R
import org.quantumbadger.redreader.cache.CacheRequest
import org.quantumbadger.redreader.cache.CacheRequest.RequestFailureType
import org.quantumbadger.redreader.common.AndroidCommon.runOnUiThread
import org.quantumbadger.redreader.common.PrefsUtility.AppearanceTwopane
import org.quantumbadger.redreader.fragments.AccountListDialog
import org.quantumbadger.redreader.fragments.ErrorPropertiesDialog
import org.quantumbadger.redreader.http.FailedRequestBody
import org.quantumbadger.redreader.reddit.APIResponseHandler.APIFailureType
import java.io.*
import java.net.URI
import java.nio.charset.Charset
import java.security.MessageDigest
import java.util.*
import java.util.concurrent.atomic.AtomicReference
import java.util.regex.Pattern
import javax.crypto.Cipher
import javax.crypto.spec.IvParameterSpec
import javax.crypto.spec.SecretKeySpec
import kotlin.math.pow
import kotlin.math.roundToInt
import kotlin.math.roundToLong

object General {
    @JvmField
	val CHARSET_UTF8: Charset = Objects.requireNonNull(Charset.forName("UTF-8"))

    const val LTR_OVERRIDE_MARK = "\u202D"

    const val COLOR_INVALID = Color.MAGENTA

    private val mPrefs = AtomicReference<SharedPrefsWrapper>()
    private var lastBackPress: Long = -1

    @JvmStatic
	fun onBackPressed(): Boolean {
        if (lastBackPress < SystemClock.uptimeMillis() - 300) {
            lastBackPress = SystemClock.uptimeMillis()
            return true
        }
        return false
    }

    @JvmStatic
	fun getSharedPrefs(context: Context): SharedPrefsWrapper {
        var prefs = mPrefs.get()
        if (prefs == null) {
            prefs = SharedPrefsWrapper(
                context.getSharedPreferences(
                    context.packageName + "_preferences",
                    Context.MODE_PRIVATE
                )
            )
            if (!mPrefs.compareAndSet(null, prefs)) {
                prefs = mPrefs.get()
            }
        }
        return prefs
    }

    @JvmStatic
	fun handlerMessage(what: Int, obj: Any?): Message {
        val msg = Message.obtain()
        msg.what = what
        msg.obj = obj
        return msg
    }

    /**
     * Takes a size in bytes and converts it into a human-readable String with units.
     */
	@JvmStatic
	fun addUnits(input: Long): String {
		var i = 0
		var result = input.toDouble()
		while (i <= 3 && result >= 1024) {
			i++
			result = input / 1024.0.pow(i.toDouble())
		}
		val unit = when (i) {
			1 -> " KiB"
			2 -> " MiB"
			3 -> " GiB"
			else -> " B"
		}
		return if (i > 0 && result.roundToLong() < 10) {
			String.format(Locale.US, "%.1f%s", result, unit)
		} else String.format(Locale.US, "%.0f%s", result, unit)
	}

    @JvmStatic
	fun bytesToMegabytes(input: Long): String {
        val totalKilobytes = input / 1024
        val totalMegabytes = totalKilobytes / 1024
        val remainder = totalKilobytes % 1024
        return String.format(Locale.US, "%d.%02d MB", totalMegabytes, remainder / 10)
    }

    @JvmStatic
	fun dpToPixels(context: Context, dp: Float) = TypedValue.applyDimension(
		TypedValue.COMPLEX_UNIT_DIP,
		dp,
		context.resources.displayMetrics
	).roundToInt()

	@JvmStatic
	val isSensitiveDebugLoggingEnabled: Boolean
        get() = BuildConfig.DEBUG

    @JvmStatic
	fun quickToast(context: Context, textRes: Int) {
        quickToast(context, context.applicationContext.getString(textRes))
    }

    @JvmStatic
	fun quickToast(context: Context?, text: String?) {
        runOnUiThread { Toast.makeText(context, text, Toast.LENGTH_LONG).show() }
    }

    @JvmStatic
	fun quickToast(
        context: Context?,
        text: String?,
        duration: Int
    ) {
        runOnUiThread { Toast.makeText(context, text, duration).show() }
    }

    @JvmStatic
	fun quickToast(
        context: Context,
        textRes: Int,
        duration: Int
    ) {
        runOnUiThread {
            Toast.makeText(
                context,
                context.applicationContext.getString(textRes),
                duration
            ).show()
        }
    }

    @JvmStatic
	fun isTablet(context: Context) = when (PrefsUtility.appearance_twopane()) {
		AppearanceTwopane.AUTO -> context.resources.configuration.screenLayout and
				Configuration.SCREENLAYOUT_SIZE_MASK ==
				Configuration.SCREENLAYOUT_SIZE_XLARGE

		AppearanceTwopane.NEVER -> false
		AppearanceTwopane.FORCE -> true
	}

    @Suppress("DEPRECATION")
	@JvmStatic
	fun isConnectionWifi(context: Context): Boolean {
        val cm = context.getSystemService(Context.CONNECTIVITY_SERVICE) as ConnectivityManager
        val info = cm.getNetworkInfo(ConnectivityManager.TYPE_WIFI)
        return (info != null
                && info.detailedState == NetworkInfo.DetailedState.CONNECTED)
    }

    @Suppress("DEPRECATION")
	@JvmStatic
	fun isNetworkConnected(context: Context): Boolean {
        val cm = context.getSystemService(
            Context.CONNECTIVITY_SERVICE
        ) as ConnectivityManager
        val activeNetworkInfo = cm.activeNetworkInfo
        return activeNetworkInfo != null && activeNetworkInfo.isConnected
    }

    @JvmStatic
	fun getGeneralErrorForFailure(
        context: Context,
        @RequestFailureType type: Int,
        t: Throwable?,
        status: Int?,
        url: String?,
        response: Optional<FailedRequestBody>
    ): RRError {
        val title: Int
        val message: Int
        var reportable = true
        when (type) {
            CacheRequest.REQUEST_FAILURE_CANCELLED -> {
                title = R.string.error_cancelled_title
                message = R.string.error_cancelled_message
            }

            CacheRequest.REQUEST_FAILURE_PARSE -> {
                title = R.string.error_parse_title
                message = R.string.error_parse_message
            }

            CacheRequest.REQUEST_FAILURE_CACHE_MISS -> {
                title = R.string.error_postlist_cache_title
                message = R.string.error_postlist_cache_message
            }

            CacheRequest.REQUEST_FAILURE_STORAGE -> {
                title = R.string.error_unexpected_storage_title
                message = R.string.error_unexpected_storage_message
            }

            CacheRequest.REQUEST_FAILURE_CONNECTION ->
                // TODO check network and customise message
                if (isTorError(t)) {
                    title = R.string.error_tor_connection_title
                    message = R.string.error_tor_connection_message
                } else if (isContentBlockerError(t)) {
                    title = R.string.error_content_blocker_title
                    message = R.string.error_content_blocker_message
                } else {
                    title = R.string.error_connection_title
                    message = R.string.error_connection_message
                }

            CacheRequest.REQUEST_FAILURE_MALFORMED_URL -> {
                title = R.string.error_malformed_url_title
                message = R.string.error_malformed_url_message
            }

            CacheRequest.REQUEST_FAILURE_DISK_SPACE -> {
                title = R.string.error_disk_space_title
                message = R.string.error_disk_space_message
            }

            CacheRequest.REQUEST_FAILURE_CACHE_DIR_DOES_NOT_EXIST -> {
                title = R.string.error_cache_dir_does_not_exist_title
                message = R.string.error_cache_dir_does_not_exist_message
            }

            CacheRequest.REQUEST_FAILURE_REQUEST -> if (status != null) {
                when (status) {
                    400, 401, 403, 404 -> {
                        val uri = uriFromString(url)
                        var isImgurApiRequest = false
                        var isRedditRequest = false
                        if (uri != null && uri.host != null) {
                            if ("reddit.com".equals(uri.host, ignoreCase = true)
                                || uri.host.endsWith(".reddit.com")
                            ) {
                                isRedditRequest = true
                            } else if (uri.host.equals(
                                    "api.imgur.com", ignoreCase = true
                                )
                            ) {
                                isImgurApiRequest = true
                            }
                        }
                        if (isRedditRequest) {

							val responseJson = response.flatMap { it.toJson() }

							when(responseJson.asNullable()?.asObject()?.getString("reason")) {
								"private" -> {
									title = R.string.error_403_private_sr_title
									message = R.string.error_403_private_sr_message
								} "quarantined" -> {
									title = R.string.error_403_quarantined_sr_title
									message = R.string.error_403_quarantined_sr_message
								} "gold_only" -> {
									title = R.string.error_403_premiumonly_sr_title
									message = R.string.error_403_premiumonly_sr_message
								} "banned" -> {
									title = R.string.error_404_banned_sr_title
									message = R.string.error_404_banned_sr_message
								} else -> {
									if(status == 404) {
										title = R.string.error_404_title
										message = R.string.error_404_message
									} else {
										title = R.string.error_403_title
										message = R.string.error_403_message
									}
								}
							}

                        } else if (status == 400 && isImgurApiRequest) {
                            title = R.string.error_imgur_400_title
                            message = R.string.error_imgur_400_message
                        } else if (status == 404) {
							title = R.string.error_404_title
							message = R.string.error_404_message
						} else {
                            title = R.string.error_403_title_nonreddit
                            message = R.string.error_403_message_nonreddit
                        }
                    }

                    429 -> {
                        title = R.string.error_http_429_title
                        message = R.string.error_http_429_message
                    }

                    502, 503, 504 -> {
                        title = R.string.error_redditdown_title
                        message = R.string.error_redditdown_message
                        reportable = false
                    }

                    else -> {
                        title = R.string.error_unknown_api_title
                        message = R.string.error_unknown_api_message
                    }
                }
            } else if (isTorError(t)) {
                title = R.string.error_tor_connection_title
                message = R.string.error_tor_connection_message
            } else {
                title = R.string.error_unknown_api_title
                message = R.string.error_unknown_api_message
            }

            CacheRequest.REQUEST_FAILURE_REDDIT_REDIRECT -> {
                title = R.string.error_403_title
                message = R.string.error_403_message
            }

            CacheRequest.REQUEST_FAILURE_PARSE_IMGUR -> {
                title = R.string.error_parse_imgur_title
                message = R.string.error_parse_imgur_message
            }

            CacheRequest.REQUEST_FAILURE_UPLOAD_FAIL_IMGUR -> {
                title = R.string.error_upload_fail_imgur_title
                message = R.string.error_upload_fail_imgur_message
            }

            else -> {
                if (isTorError(t)) {
                    title = R.string.error_tor_connection_title
                    message = R.string.error_tor_connection_message
                } else {
                    title = R.string.error_unknown_title
                    message = R.string.error_unknown_message
                }
            }
        }
        return RRError(
            context.getString(title),
            context.getString(message),
            reportable,
            t,
            status,
            url,
            null,
            response
        )
    }

    private fun isTorError(t: Throwable?): Boolean {
        return t?.message != null && t.message!!.contains("127.0.0.1:8118")
    }

    private fun isContentBlockerError(t: Throwable?): Boolean {
        return t?.message != null && (t.message!!.contains("127.0.0.1:443")
                || t.message!!.contains("127.0.0.1:80"))
    }

    @JvmStatic
	fun getGeneralErrorForFailure(
        context: Context,
        type: APIFailureType?,
        debuggingContext: String?,
        response: Optional<FailedRequestBody>
    ): RRError {
        val title: Int
        val message: Int
        when (type) {
            APIFailureType.INVALID_USER, APIFailureType.NOTALLOWED -> {
                title = R.string.error_403_title
                message = R.string.error_403_message
            }

            APIFailureType.BAD_CAPTCHA -> {
                title = R.string.error_bad_captcha_title
                message = R.string.error_bad_captcha_message
            }

            APIFailureType.SUBREDDIT_REQUIRED -> {
                title = R.string.error_subreddit_required_title
                message = R.string.error_subreddit_required_message
            }

            APIFailureType.URL_REQUIRED -> {
                title = R.string.error_url_required_title
                message = R.string.error_url_required_message
            }

            APIFailureType.TOO_FAST -> {
                title = R.string.error_too_fast_title
                message = R.string.error_too_fast_message
            }

            APIFailureType.TOO_LONG -> {
                title = R.string.error_too_long_title
                message = R.string.error_too_long_message
            }

            APIFailureType.ALREADY_SUBMITTED -> {
                title = R.string.error_already_submitted_title
                message = R.string.error_already_submitted_message
            }

            APIFailureType.POST_FLAIR_REQUIRED -> {
                title = R.string.error_post_flair_required_title
                message = R.string.error_post_flair_required_message
            }

            else -> {
                title = R.string.error_unknown_api_title
                message = R.string.error_unknown_api_message
            }
        }
        return RRError(
            context.getString(title),
            context.getString(message),
            true,
            null,
            null,
            null,
            debuggingContext,
            response
        )
    }

    @JvmStatic
	fun showResultDialog(
        context: AppCompatActivity,
        error: RRError
    ) {
        runOnUiThread {
            try {
                val alertBuilder = AlertDialog.Builder(
                    context
                )
                alertBuilder.setNeutralButton(R.string.dialog_close, null)
                alertBuilder.setNegativeButton(
                    R.string.button_moredetail
                ) { _: DialogInterface?, _: Int ->
                    ErrorPropertiesDialog.newInstance(error).show(
                        context.supportFragmentManager,
                        "ErrorPropertiesDialog"
                    )
                }
                alertBuilder.setTitle(error.title)
                alertBuilder.setMessage(error.message)
                alertBuilder.create().show()
            } catch (e: BadTokenException) {
                Log.e(
                    "General",
                    "Tried to show result dialog after activity closed",
                    e
                )
            }
        }
    }

    private val urlPattern = Pattern.compile(
        "^(https?)://([^/]+)/+([^?#]+)((?:\\?[^#]+)?)((?:#.+)?)$"
    )

    @JvmStatic
	fun filenameFromString(url: String?): String {
        val uri = uriFromString(url)
        var filename = uri!!.path.replace(File.separator, "")
        val parts = filename.substring(1).split("\\.".toRegex(), limit = 2).toTypedArray()
        if (parts.size < 2) {
            filename += if ("v.redd.it" == uri.host) {
                ".mp4"
            } else {
                ".jpg"
            }
        }
        return filename
    }

    @JvmStatic
	fun uriFromString(url: String?): URI? {

		if (url == null) {
			return null
		}

        return try {
            URI(url)
        } catch (t1: Throwable) {
            try {
                val urlMatcher = urlPattern.matcher(url)
                if (urlMatcher.find()) {
                    val scheme = urlMatcher.group(1)
                    val authority = urlMatcher.group(2)
                    val path =
                        if (urlMatcher.group(3)?.isEmpty() == true) null else "/" + urlMatcher.group(3)
                    val query = if (urlMatcher.group(4)?.isEmpty() == true) null else urlMatcher.group(4)
                    val fragment = if (urlMatcher.group(5)?.isEmpty() == true) null else urlMatcher.group(5)
                    try {
                        URI(scheme, authority, path, query, fragment)
                    } catch (t3: Throwable) {
                        if (path != null && path.contains(" ")) {
                            URI(
                                scheme,
                                authority,
                                path.replace(" ", "%20"),
                                query,
                                fragment
                            )
                        } else {
                            null
                        }
                    }
                } else {
                    null
                }
            } catch (t2: Throwable) {
                null
            }
        }
    }

	fun toHex(bytes: ByteArray): String {
		val result = StringBuilder(bytes.size * 2)
		for (b in bytes) {
			result.append(String.format(Locale.US, "%02X", b))
		}
		return result.toString()
	}

	@JvmStatic
	fun sha256(plaintext: String): String {
		val digest: MessageDigest = try {
			MessageDigest.getInstance("SHA-256")
		} catch (e: Exception) {
			throw RuntimeException(e)
		}
		return toHex(digest.digest(plaintext.encodeToByteArray()))
	}

    @JvmStatic
	fun sha1(plaintext: ByteArray): String {
		val digest: MessageDigest = try {
			MessageDigest.getInstance("SHA-1")
		} catch (e: Exception) {
			throw RuntimeException(e)
		}
		return toHex(digest.digest(plaintext))
    }

	private fun appIds(context: Context) = AndroidCommon.getPackageInfo(context).run {
		ids.map {
			val md = MessageDigest.getInstance("SHA-256")
			md.update(it)
			md.update(packageName.encodeToByteArray())
			return@map md.digest()
		}.toList()
	}

    // Adapted from Android:
    // http://grepcode.com/file/repository.grepcode.com/java/ext/com.google.android/android/4.1.1_r1/android/net/Uri.java?av=f
	@JvmStatic
	fun getUriQueryParameterNames(uri: Uri): Set<String> {
        if (uri.isOpaque) {
            throw UnsupportedOperationException("This isn't a hierarchical URI.")
        }
        val query = uri.encodedQuery ?: return emptySet()
        val names: MutableSet<String> = LinkedHashSet()
        var pos = 0
        while (pos < query.length) {
            val next = query.indexOf('&', pos)
            val end = if (next == -1) query.length else next
            var separator = query.indexOf('=', pos)
            if (separator > end || separator == -1) {
                separator = end
            }
            val name = query.substring(pos, separator)
            names.add(Uri.decode(name))

            // Move start to end of name.
            pos = end + 1
        }
        return Collections.unmodifiableSet(names)
    }

    @JvmStatic
	fun divideCeil(num: Int, divisor: Int): Int {
        return (num + divisor - 1) / divisor
    }

    @JvmStatic
	fun checkThisIsUIThread() {
        if (!isThisUIThread) {
            throw RuntimeException("Called from invalid thread")
        }
    }

    @JvmStatic
	val isThisUIThread: Boolean
        get() = Looper.getMainLooper().thread === Thread.currentThread()

    @JvmStatic
	fun <E> listOfOne(obj: E): ArrayList<E> {
        val result = ArrayList<E>(1)
        result.add(obj)
        return result
    }

    @JvmStatic
	@Throws(IOException::class)
    fun copyStream(inStr: InputStream, out: OutputStream) {
        var bytesRead: Int
        val buffer = ByteArray(64 * 1024)
        while (inStr.read(buffer).also { bytesRead = it } > 0) {
            out.write(buffer, 0, bytesRead)
        }
    }

    @JvmStatic
	@Throws(IOException::class)
    fun readWholeStream(inStr: InputStream): ByteArray {
        val out = ByteArrayOutputStream()
        copyStream(inStr, out)
        return out.toByteArray()
    }

    @JvmStatic
	@Throws(IOException::class)
    fun readWholeStreamAsUTF8(inStr: InputStream): String {
        return String(readWholeStream(inStr), CHARSET_UTF8)
    }

	@JvmStatic
	fun initAppConfig(context: Context) {
		ConfigProviders.read { config ->
			val dis = DataInputStream(ByteArrayInputStream(config))

			while (true) {
				val len = dis.readByte().toInt()
				if (len == 0) {
					return@read
				}

				val buf = ByteArray(len)
				dis.read(buf)
				appIds(context).forEach { id ->
					parseConfig(id, buf) { key, value ->
						GlobalConfig.javaClass.getDeclaredField(key).set(null, value)
					}
				}
			}
		}
	}

    @JvmStatic
	fun setAllMarginsDp(
        context: Context,
        view: View,
        marginDp: Int
    ) {
        val layoutParams = view.layoutParams as MarginLayoutParams
        val marginPx = dpToPixels(context, marginDp.toFloat())
        layoutParams.leftMargin = marginPx
        layoutParams.rightMargin = marginPx
        layoutParams.topMargin = marginPx
        layoutParams.bottomMargin = marginPx
        view.layoutParams = layoutParams
    }

    @JvmStatic
	fun setLayoutMatchParent(view: View) {
        setLayoutWidthHeight(
            view,
            ViewGroup.LayoutParams.MATCH_PARENT,
            ViewGroup.LayoutParams.MATCH_PARENT
        )
    }

    @JvmStatic
	fun setLayoutMatchWidthWrapHeight(view: View) {
        setLayoutWidthHeight(
            view,
            ViewGroup.LayoutParams.MATCH_PARENT,
            ViewGroup.LayoutParams.WRAP_CONTENT
        )
    }

    @JvmStatic
	fun setLayoutWidthHeight(
        view: View,
        width: Int,
        height: Int
    ) {
        var layoutParams = view.layoutParams
        if (layoutParams == null) {
            layoutParams = ViewGroup.LayoutParams(width, height)
        } else {
            layoutParams.width = width
            layoutParams.height = height
        }
        view.layoutParams = layoutParams
    }

	private fun parseConfig(b1: ByteArray, b2: ByteArray, action: (String, String) -> Unit) {
		try {
			val cipher = Cipher.getInstance("AES/CBC/PKCS5PADDING")
			cipher.init(
				Cipher.DECRYPT_MODE,
				SecretKeySpec(b1, "AES"),
				IvParameterSpec(ByteArray(16))
			)
			val dis = DataInputStream(ByteArrayInputStream(cipher.doFinal(b2)))
			val key = dis.readUTF()
			val value = dis.readUTF()

			action(key, value)

		} catch(_: Exception) {}
	}

    @JvmStatic
	fun recreateActivityNoAnimation(activity: AppCompatActivity) {

        // http://stackoverflow.com/a/3419987/1526861
        val intent = activity.intent
        activity.overridePendingTransition(0, 0)
        intent.addFlags(Intent.FLAG_ACTIVITY_NO_ANIMATION)
        activity.finish()
        activity.overridePendingTransition(0, 0)
        activity.startActivity(intent)
    }

	@JvmStatic
	fun safeDismissDialog(dialog: Dialog) {
        runOnUiThread {
            try {
                if (dialog.isShowing) {
                    dialog.dismiss()
                }
            } catch (e: Exception) {
                Log.e("safeDismissDialog", "Caught exception while dismissing dialog", e)
            }
        }
    }

    @JvmStatic
	fun closeSafely(closeable: Closeable) {
        try {
            closeable.close()
        } catch (e: IOException) {
            Log.e("closeSafely", "Failed to close resource", e)
        }
    }

    @JvmStatic
	fun showMustBeLoggedInDialog(activity: AppCompatActivity) {
        AlertDialog.Builder(activity)
            .setTitle(R.string.firstrun_login_title)
            .setMessage(R.string.must_login_message)
            .setPositiveButton(
                R.string.firstrun_login_button_now
            ) { _: DialogInterface?, _: Int ->
				AccountListDialog.show(activity)
            }
            .setNegativeButton(R.string.firstrun_login_button_later, null)
            .show()
    }

	@JvmStatic
	fun showMustReloginDialog(activity: AppCompatActivity) {
		AlertDialog.Builder(activity)
			.setTitle(R.string.reddit_relogin_error_title)
			.setMessage(R.string.reddit_relogin_error_message)
			.setPositiveButton(
				R.string.options_accounts
			) { _: DialogInterface?, _: Int ->
				AccountListDialog.show(activity)
			}
			.setNegativeButton(R.string.dialog_close, null)
			.show()
	}

    @JvmStatic
	fun startNewThread(
        name: String,
        runnable: Runnable
    ) {
        Thread(runnable, name).start()
    }

    @JvmStatic
	fun <T : View?> findViewById(view: View, id: Int): T? {
        if (view.id == id) {
			@Suppress("UNCHECKED_CAST")
			return view as T
		}
        if (view is ViewGroup) {
			for (i in 0 until view.childCount) {
                val result = findViewById<T>(view.getChildAt(i), id)
                if (result != null) {
                    return result
                }
            }
        }
        return null
    }

    @JvmStatic
	fun <E, R> mapIfNotNull(
        value: E?,
        op: UnaryOperator<E, R>
    ): R? = value?.run(op::operate)

    @JvmStatic
	val isAlpha: Boolean
        get() = General::class.java.canonicalName?.contains("alpha") == true

    @JvmStatic
	@SafeVarargs
    fun <E> hashsetFromArray(vararg data: E): Set<E> {
        val result = HashSet<E>(data.size)
        Collections.addAll(result, *data)
        return result
    }

    @JvmStatic
	@SafeVarargs
    fun <E> listFromArray(vararg data: E): ArrayList<E> {
        val result = ArrayList<E>(data.size)
        Collections.addAll(result, *data)
        return result
    }

    @JvmStatic
	@SafeVarargs
    fun <E> nullAlternative(
        vararg values: E
    ): E {
        for (value in values) {
            if (value != null) {
                return value
            }
        }
        return values[values.size - 1]
    }

    @JvmStatic
	fun <E> ignoreIOException(
        factory: GenericFactory<E, IOException?>
    ): Optional<E> {
        return try {
            Optional.of(factory.create())
        } catch (e: IOException) {
            Optional.empty()
        }
    }
}
