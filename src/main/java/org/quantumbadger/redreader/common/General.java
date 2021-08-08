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

import android.app.AlertDialog;
import android.app.Dialog;
import android.content.Context;
import android.content.Intent;
import android.content.res.Configuration;
import android.graphics.Color;
import android.net.ConnectivityManager;
import android.net.NetworkInfo;
import android.net.Uri;
import android.os.Looper;
import android.os.Message;
import android.os.SystemClock;
import android.util.Log;
import android.util.TypedValue;
import android.view.View;
import android.view.ViewGroup;
import android.view.WindowManager;
import android.widget.Toast;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;
import org.quantumbadger.redreader.BuildConfig;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.activities.BugReportActivity;
import org.quantumbadger.redreader.cache.CacheRequest;
import org.quantumbadger.redreader.fragments.AccountListDialog;
import org.quantumbadger.redreader.fragments.ErrorPropertiesDialog;
import org.quantumbadger.redreader.http.FailedRequestBody;
import org.quantumbadger.redreader.reddit.APIResponseHandler;

import java.io.ByteArrayOutputStream;
import java.io.Closeable;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URI;
import java.nio.charset.Charset;
import java.security.MessageDigest;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.Locale;
import java.util.Set;
import java.util.concurrent.atomic.AtomicReference;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public final class General {

	@SuppressWarnings("CharsetObjectCanBeUsed")
	public static final Charset CHARSET_UTF8 = Charset.forName("UTF-8");

	public static final String LTR_OVERRIDE_MARK = "\u202D";

	public static final int COLOR_INVALID = Color.MAGENTA;

	private static final AtomicReference<SharedPrefsWrapper> mPrefs = new AtomicReference<>();

	private static long lastBackPress = -1;

	public static boolean onBackPressed() {

		if(lastBackPress < SystemClock.uptimeMillis() - 300) {
			lastBackPress = SystemClock.uptimeMillis();
			return true;
		}

		return false;
	}

	@NonNull
	public static SharedPrefsWrapper getSharedPrefs(@NonNull final Context context) {

		SharedPrefsWrapper prefs = mPrefs.get();

		if(prefs == null) {
			prefs = new SharedPrefsWrapper(context.getSharedPreferences(
					context.getPackageName() + "_preferences",
					Context.MODE_PRIVATE));

			if(!mPrefs.compareAndSet(null, prefs)) {
				prefs = mPrefs.get();
			}
		}

		return prefs;
	}

	public static Message handlerMessage(final int what, final Object obj) {
		final Message msg = Message.obtain();
		msg.what = what;
		msg.obj = obj;
		return msg;
	}

	/**
	 * Takes a size in bytes and converts it into a human-readable String with units.
	 */
	public static String addUnits(final long input) {
		int i = 0;
		double result = input;
		while(i <= 3 && result >= 1024) {
			result = input / Math.pow(1024, ++i);
		}

		final String unit;
		switch(i) {
			case 1:
				unit = " KiB";
				break;
			case 2:
				unit = " MiB";
				break;
			case 3:
				unit = " GiB";
				break;
			default:
				unit = " B";
		}

		if(i > 0 && Math.round(result) < 10) {
			return String.format(Locale.US, "%.1f%s", result, unit);
		}

		return String.format(Locale.US, "%.0f%s", result, unit);
	}

	public static String bytesToMegabytes(final long input) {
		final long totalKilobytes = input / 1024;
		final long totalMegabytes = totalKilobytes / 1024;
		final long remainder = totalKilobytes % 1024;
		return String.format(Locale.US, "%d.%02d MB", totalMegabytes, (remainder / 10));
	}

	public static int dpToPixels(final Context context, final float dp) {
		return Math.round(TypedValue.applyDimension(
				TypedValue.COMPLEX_UNIT_DIP,
				dp,
				context.getResources().getDisplayMetrics()));
	}

	public static int spToPixels(final Context context, final float sp) {
		return Math.round(TypedValue.applyDimension(
				TypedValue.COMPLEX_UNIT_SP,
				sp,
				context.getResources().getDisplayMetrics()));
	}

	public static boolean isSensitiveDebugLoggingEnabled() {
		return BuildConfig.DEBUG;
	}

	public static void quickToast(final Context context, final int textRes) {
		quickToast(context, context.getApplicationContext().getString(textRes));
	}

	public static void quickToast(final Context context, final String text) {
		AndroidCommon.runOnUiThread(
				() -> Toast.makeText(context, text, Toast.LENGTH_LONG).show());
	}

	public static void quickToast(
			final Context context,
			final String text,
			final int duration) {
		AndroidCommon.runOnUiThread(
				() -> Toast.makeText(context, text, duration).show());
	}

	public static void quickToast(
			final Context context,
			final int textRes,
			final int duration) {
		AndroidCommon.runOnUiThread(() -> Toast.makeText(
				context,
				context.getApplicationContext().getString(textRes),
				duration).show());
	}

	public static boolean isTablet(final Context context) {

		final PrefsUtility.AppearanceTwopane pref = PrefsUtility.appearance_twopane();

		switch(pref) {
			case AUTO:
				return (context.getResources().getConfiguration().screenLayout &
						Configuration.SCREENLAYOUT_SIZE_MASK) ==
						Configuration.SCREENLAYOUT_SIZE_XLARGE;
			case NEVER:
				return false;
			case FORCE:
				return true;
			default:
				BugReportActivity.handleGlobalError(
						context,
						"Unknown AppearanceTwopane value " + pref.name());
				return false;
		}
	}

	public static boolean isConnectionWifi(final Context context) {

		final ConnectivityManager cm
				= (ConnectivityManager)context.getSystemService(Context.CONNECTIVITY_SERVICE);

		final NetworkInfo info = cm.getNetworkInfo(ConnectivityManager.TYPE_WIFI);

		return info != null
				&& info.getDetailedState() == NetworkInfo.DetailedState.CONNECTED;
	}

	public static boolean isNetworkConnected(final Context context) {
		final ConnectivityManager cm = (ConnectivityManager)context.getSystemService(
				Context.CONNECTIVITY_SERVICE);
		final NetworkInfo activeNetworkInfo = cm.getActiveNetworkInfo();
		return activeNetworkInfo != null && activeNetworkInfo.isConnected();
	}

	public static RRError getGeneralErrorForFailure(
			@NonNull final Context context,
			@CacheRequest.RequestFailureType final int type,
			@Nullable final Throwable t,
			@Nullable final Integer status,
			@Nullable final String url,
			@NonNull final Optional<FailedRequestBody> response) {

		final int title;
		final int message;
		boolean reportable = true;

		switch(type) {
			case CacheRequest.REQUEST_FAILURE_CANCELLED:
				title = R.string.error_cancelled_title;
				message = R.string.error_cancelled_message;
				break;
			case CacheRequest.REQUEST_FAILURE_PARSE:
				title = R.string.error_parse_title;
				message = R.string.error_parse_message;
				break;
			case CacheRequest.REQUEST_FAILURE_CACHE_MISS:
				title = R.string.error_unexpected_cache_title;
				message = R.string.error_unexpected_cache_message;
				break;
			case CacheRequest.REQUEST_FAILURE_STORAGE:
				title = R.string.error_unexpected_storage_title;
				message = R.string.error_unexpected_storage_message;
				break;
			case CacheRequest.REQUEST_FAILURE_CONNECTION:

				// TODO check network and customise message
				if(isTorError(t)) {
					title = R.string.error_tor_connection_title;
					message = R.string.error_tor_connection_message;

				} else if(isContentBlockerError(t)) {
					title = R.string.error_content_blocker_title;
					message = R.string.error_content_blocker_message;

				} else {
					title = R.string.error_connection_title;
					message = R.string.error_connection_message;
				}

				break;

			case CacheRequest.REQUEST_FAILURE_MALFORMED_URL:
				title = R.string.error_malformed_url_title;
				message = R.string.error_malformed_url_message;
				break;
			case CacheRequest.REQUEST_FAILURE_DISK_SPACE:
				title = R.string.error_disk_space_title;
				message = R.string.error_disk_space_message;
				break;
			case CacheRequest.REQUEST_FAILURE_CACHE_DIR_DOES_NOT_EXIST:
				title = R.string.error_cache_dir_does_not_exist_title;
				message = R.string.error_cache_dir_does_not_exist_message;
				break;
			case CacheRequest.REQUEST_FAILURE_REQUEST:

				if(status != null) {
					switch(status) {
						case 400:
						case 401:
						case 403: {
							final URI uri = General.uriFromString(url);

							boolean isImgurApiRequest = false;
							boolean isRedditRequest = false;

							if(uri != null && uri.getHost() != null) {
								if("reddit.com".equalsIgnoreCase(uri.getHost())
										|| uri.getHost().endsWith(".reddit.com")) {

									isRedditRequest = true;

								} else if(uri.getHost().equalsIgnoreCase(
										"api.imgur.com")) {
									isImgurApiRequest = true;
								}
							}

							if(isRedditRequest) {
								title = R.string.error_403_title;
								message = R.string.error_403_message;

							} else if(status == 400 && isImgurApiRequest) {
								title = R.string.error_imgur_400_title;
								message = R.string.error_imgur_400_message;

							} else {
								title = R.string.error_403_title_nonreddit;
								message = R.string.error_403_message_nonreddit;
							}

							break;
						}
						case 404:
							title = R.string.error_404_title;
							message = R.string.error_404_message;
							break;
						case 429:
							title = R.string.error_http_429_title;
							message = R.string.error_http_429_message;
							break;
						case 502:
						case 503:
						case 504:
							title = R.string.error_redditdown_title;
							message = R.string.error_redditdown_message;
							reportable = false;
							break;
						default:
							title = R.string.error_unknown_api_title;
							message = R.string.error_unknown_api_message;
							break;
					}

				} else if(isTorError(t)) {
					title = R.string.error_tor_connection_title;
					message = R.string.error_tor_connection_message;

				} else {
					title = R.string.error_unknown_api_title;
					message = R.string.error_unknown_api_message;
				}

				break;
			case CacheRequest.REQUEST_FAILURE_REDDIT_REDIRECT:
				title = R.string.error_403_title;
				message = R.string.error_403_message;
				break;

			case CacheRequest.REQUEST_FAILURE_PARSE_IMGUR:
				title = R.string.error_parse_imgur_title;
				message = R.string.error_parse_imgur_message;
				break;

			case CacheRequest.REQUEST_FAILURE_UPLOAD_FAIL_IMGUR:
				title = R.string.error_upload_fail_imgur_title;
				message = R.string.error_upload_fail_imgur_message;
				break;

			default: {
				if(isTorError(t)) {
					title = R.string.error_tor_connection_title;
					message = R.string.error_tor_connection_message;

				} else {
					title = R.string.error_unknown_title;
					message = R.string.error_unknown_message;
				}

				break;
			}
		}

		return new RRError(
				context.getString(title),
				context.getString(message),
				reportable,
				t,
				status,
				url,
				null,
				response);
	}

	private static boolean isTorError(@Nullable final Throwable t) {
		return t != null
				&& t.getMessage() != null
				&& t.getMessage().contains("127.0.0.1:8118");
	}

	private static boolean isContentBlockerError(@Nullable final Throwable t) {
		return t != null
				&& t.getMessage() != null
				&& (t.getMessage().contains("127.0.0.1:443")
						|| t.getMessage().contains("127.0.0.1:80"));
	}

	public static RRError getGeneralErrorForFailure(
			final Context context,
			final APIResponseHandler.APIFailureType type,
			final String debuggingContext,
			@NonNull final Optional<FailedRequestBody> response) {

		final int title;
		final int message;

		switch(type) {

			case INVALID_USER:
			case NOTALLOWED:
				title = R.string.error_403_title;
				message = R.string.error_403_message;
				break;

			case BAD_CAPTCHA:
				title = R.string.error_bad_captcha_title;
				message = R.string.error_bad_captcha_message;
				break;

			case SUBREDDIT_REQUIRED:
				title = R.string.error_subreddit_required_title;
				message = R.string.error_subreddit_required_message;
				break;

			case URL_REQUIRED:
				title = R.string.error_url_required_title;
				message = R.string.error_url_required_message;
				break;

			case TOO_FAST:
				title = R.string.error_too_fast_title;
				message = R.string.error_too_fast_message;
				break;

			case TOO_LONG:
				title = R.string.error_too_long_title;
				message = R.string.error_too_long_message;
				break;

			case ALREADY_SUBMITTED:
				title = R.string.error_already_submitted_title;
				message = R.string.error_already_submitted_message;
				break;

			case POST_FLAIR_REQUIRED:
				title = R.string.error_post_flair_required_title;
				message = R.string.error_post_flair_required_message;
				break;

			default:
				title = R.string.error_unknown_api_title;
				message = R.string.error_unknown_api_message;
				break;
		}

		return new RRError(
				context.getString(title),
				context.getString(message),
				true,
				null,
				null,
				null,
				debuggingContext,
				response);
	}

	public static void showResultDialog(
			final AppCompatActivity context,
			final RRError error) {
		AndroidCommon.runOnUiThread(() -> {
			try {
				final AlertDialog.Builder alertBuilder = new AlertDialog.Builder(
						context);
				alertBuilder.setNeutralButton(R.string.dialog_close, null);
				alertBuilder.setNegativeButton(
						R.string.button_moredetail,
						(dialog, which) -> ErrorPropertiesDialog.newInstance(error).show(
								context.getSupportFragmentManager(),
								"ErrorPropertiesDialog"));
				alertBuilder.setTitle(error.title);
				alertBuilder.setMessage(error.message);
				alertBuilder.create().show();

			} catch(final WindowManager.BadTokenException e) {
				Log.e(
						"General",
						"Tried to show result dialog after activity closed",
						e);
			}
		});
	}

	private static final Pattern urlPattern = Pattern.compile(
			"^(https?)://([^/]+)/+([^\\?#]+)((?:\\?[^#]+)?)((?:#.+)?)$");

	public static String filenameFromString(final String url) {
		final URI uri = uriFromString(url);
		String filename = uri.getPath().replace(File.separator, "");
		final String[] parts = filename.substring(1).split("\\.", 2);
		if(parts.length < 2) {
			if("v.redd.it".equals(uri.getHost())) {
				filename += ".mp4";
			} else {
				filename += ".jpg";
			}
		}
		return filename;
	}

	@Nullable
	public static URI uriFromString(final String url) {

		try {
			return new URI(url);

		} catch(final Throwable t1) {
			try {

				final Matcher urlMatcher = urlPattern.matcher(url);

				if(urlMatcher.find()) {

					final String scheme = urlMatcher.group(1);
					final String authority = urlMatcher.group(2);
					final String path = urlMatcher.group(3).isEmpty()
							? null
							: "/" + urlMatcher.group(3);
					final String query = urlMatcher.group(4).isEmpty()
							? null
							: urlMatcher.group(4);
					final String fragment = urlMatcher.group(5).isEmpty()
							? null
							: urlMatcher.group(5);

					try {
						return new URI(scheme, authority, path, query, fragment);
					} catch(final Throwable t3) {

						if(path != null && path.contains(" ")) {
							return new URI(
									scheme,
									authority,
									path.replace(" ", "%20"),
									query,
									fragment);
						} else {
							return null;
						}
					}

				} else {
					return null;
				}

			} catch(final Throwable t2) {
				return null;
			}
		}
	}

	public static String sha1(final byte[] plaintext) {

		final MessageDigest digest;
		try {
			digest = MessageDigest.getInstance("SHA-1");
		} catch(final Exception e) {
			throw new RuntimeException(e);
		}

		digest.update(plaintext, 0, plaintext.length);
		final byte[] hash = digest.digest();
		final StringBuilder result = new StringBuilder(hash.length * 2);
		for(final byte b : hash) {
			result.append(String.format(Locale.US, "%02X", b));
		}
		return result.toString();
	}

	// Adapted from Android:
	// http://grepcode.com/file/repository.grepcode.com/java/ext/com.google.android/android/4.1.1_r1/android/net/Uri.java?av=f
	public static Set<String> getUriQueryParameterNames(final Uri uri) {

		if(uri.isOpaque()) {
			throw new UnsupportedOperationException("This isn't a hierarchical URI.");
		}

		final String query = uri.getEncodedQuery();
		if(query == null) {
			return Collections.emptySet();
		}

		final Set<String> names = new LinkedHashSet<>();
		int pos = 0;
		while(pos < query.length()) {
			final int next = query.indexOf('&', pos);
			final int end = (next == -1) ? query.length() : next;

			int separator = query.indexOf('=', pos);
			if(separator > end || separator == -1) {
				separator = end;
			}

			final String name = query.substring(pos, separator);
			names.add(Uri.decode(name));

			// Move start to end of name.
			pos = end + 1;
		}

		return Collections.unmodifiableSet(names);
	}

	public static int divideCeil(final int num, final int divisor) {
		return (num + divisor - 1) / divisor;
	}

	public static void checkThisIsUIThread() {
		if(!isThisUIThread()) {
			throw new RuntimeException("Called from invalid thread");
		}
	}

	public static boolean isThisUIThread() {
		return Looper.getMainLooper().getThread() == Thread.currentThread();
	}

	public static <E> ArrayList<E> listOfOne(final E obj) {
		final ArrayList<E> result = new ArrayList<>(1);
		result.add(obj);
		return result;
	}

	public static void copyStream(final InputStream in, final OutputStream out) throws IOException {

		int bytesRead;
		final byte[] buffer = new byte[64 * 1024];

		while((bytesRead = in.read(buffer)) > 0) {
			out.write(buffer, 0, bytesRead);
		}
	}

	public static byte[] readWholeStream(final InputStream in) throws IOException {

		final ByteArrayOutputStream baos = new ByteArrayOutputStream();
		copyStream(in, baos);
		return baos.toByteArray();
	}

	public static String readWholeStreamAsUTF8(final InputStream in) throws IOException {
		return new String(readWholeStream(in), CHARSET_UTF8);
	}

	public static void setAllMarginsDp(
			final Context context,
			final View view,
			final int marginDp) {

		final ViewGroup.MarginLayoutParams layoutParams
				= (ViewGroup.MarginLayoutParams)view.getLayoutParams();

		final int marginPx = dpToPixels(context, marginDp);

		layoutParams.leftMargin = marginPx;
		layoutParams.rightMargin = marginPx;
		layoutParams.topMargin = marginPx;
		layoutParams.bottomMargin = marginPx;

		view.setLayoutParams(layoutParams);
	}

	public static void setLayoutMatchParent(final View view) {
		setLayoutWidthHeight(
				view,
				ViewGroup.LayoutParams.MATCH_PARENT,
				ViewGroup.LayoutParams.MATCH_PARENT);
	}

	public static void setLayoutMatchWidthWrapHeight(final View view) {
		setLayoutWidthHeight(
				view,
				ViewGroup.LayoutParams.MATCH_PARENT,
				ViewGroup.LayoutParams.WRAP_CONTENT);
	}

	public static void setLayoutWidthHeight(
			final View view,
			final int width,
			final int height) {

		ViewGroup.LayoutParams layoutParams = view.getLayoutParams();

		if(layoutParams == null) {
			layoutParams = new ViewGroup.LayoutParams(width, height);

		} else {
			layoutParams.width = width;
			layoutParams.height = height;
		}

		view.setLayoutParams(layoutParams);
	}

	public static void recreateActivityNoAnimation(final AppCompatActivity activity) {

		// http://stackoverflow.com/a/3419987/1526861
		final Intent intent = activity.getIntent();
		activity.overridePendingTransition(0, 0);
		intent.addFlags(Intent.FLAG_ACTIVITY_NO_ANIMATION);
		activity.finish();
		activity.overridePendingTransition(0, 0);
		activity.startActivity(intent);
	}

	public static long hoursToMs(final long hours) {
		return hours * 60L * 60L * 1000L;
	}

	public static void safeDismissDialog(final Dialog dialog) {
		AndroidCommon.runOnUiThread(() -> {
			try {
				if(dialog.isShowing()) {
					dialog.dismiss();
				}
			} catch(final Exception e) {
				Log.e("safeDismissDialog", "Caught exception while dismissing dialog", e);
			}
		});
	}

	public static void closeSafely(final Closeable closeable) {

		try {
			closeable.close();
		} catch(final IOException e) {
			Log.e("closeSafely", "Failed to close resource", e);
		}
	}

	public static void showMustBeLoggedInDialog(final AppCompatActivity activity) {

		new AlertDialog.Builder(activity)
				.setTitle(R.string.firstrun_login_title)
				.setMessage(R.string.must_login_message)
				.setPositiveButton(
						R.string.firstrun_login_button_now,
						(dialog, which) -> new AccountListDialog().show(
								activity.getSupportFragmentManager(),
								null))
				.setNegativeButton(R.string.firstrun_login_button_later, null)
				.show();
	}

	public static void startNewThread(
			@NonNull final String name,
			@NonNull final Runnable runnable) {

		new Thread(runnable, name).start();
	}

	@Nullable
	public static <T extends View> T findViewById(@NonNull final View view, final int id) {

		if(view.getId() == id) {
			//noinspection unchecked
			return (T)view;
		}

		if(view instanceof ViewGroup) {

			final ViewGroup group = (ViewGroup)view;

			for(int i = 0; i < group.getChildCount(); i++) {

				final T result = findViewById(group.getChildAt(i), id);

				if(result != null) {
					return result;
				}
			}
		}

		return null;
	}

	public static <E> void ifNotNull(
			@Nullable final E value,
			@NonNull final Consumer<E> consumer) {

		if(value != null) {
			consumer.consume(value);
		}
	}

	@Nullable
	public static <E, R> R mapIfNotNull(
			@Nullable final E value,
			@NonNull final UnaryOperator<E, R> op) {

		if(value != null) {
			return op.operate(value);
		}

		return null;
	}

	public static boolean isAlpha() {
		//noinspection ConstantConditions
		return General.class.getCanonicalName().contains("alpha");
	}

	@SafeVarargs
	@NonNull
	public static <E> Set<E> hashsetFromArray(@NonNull final E... data) {
		final HashSet<E> result = new HashSet<>(data.length);
		Collections.addAll(result, data);
		return result;
	}

	@SafeVarargs
	public static <E> E nullAlternative(
			final E... values) {

		for(final E value : values) {
			if(value != null) {
				return value;
			}
		}

		return values[values.length - 1];
	}

	@NonNull
	public static <E> Optional<E> ignoreIOException(
			@NonNull final GenericFactory<E, IOException> factory) {

		try {
			return Optional.of(factory.create());
		} catch(final IOException e) {
			return Optional.empty();
		}
	}
}
