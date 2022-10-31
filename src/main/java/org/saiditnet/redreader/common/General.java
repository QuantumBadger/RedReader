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

package org.saiditnet.redreader.common;

import android.app.AlertDialog;
import android.app.Dialog;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.SharedPreferences;
import android.content.res.Configuration;
import android.graphics.Color;
import android.graphics.Typeface;
import android.net.ConnectivityManager;
import android.net.NetworkInfo;
import android.net.Uri;
import android.os.Build;
import android.os.Looper;
import android.os.Message;
import android.os.StatFs;
import android.os.SystemClock;
import android.preference.PreferenceManager;
import android.support.annotation.NonNull;
import android.support.v7.app.AppCompatActivity;
import android.util.Log;
import android.util.TypedValue;
import android.view.View;
import android.view.ViewGroup;
import android.view.WindowManager;
import android.widget.Toast;
import org.saiditnet.redreader.R;
import org.saiditnet.redreader.activities.BugReportActivity;
import org.saiditnet.redreader.cache.CacheRequest;
import org.saiditnet.redreader.fragments.ErrorPropertiesDialog;
import org.saiditnet.redreader.reddit.APIResponseHandler;

import java.io.ByteArrayOutputStream;
import java.io.Closeable;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URI;
import java.nio.charset.Charset;
import java.security.MessageDigest;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.Locale;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public final class General {

	public static final Charset CHARSET_UTF8 = Charset.forName("UTF-8");

	public static final int COLOR_INVALID = Color.MAGENTA;

	private static long lastBackPress = -1;

	public static boolean onBackPressed() {

		if(lastBackPress < SystemClock.uptimeMillis() - 300) {
			lastBackPress = SystemClock.uptimeMillis();
			return true;
		}

		return false;
	}

	private static Typeface monoTypeface;

	public static Typeface getMonoTypeface(Context context) {

		if(monoTypeface == null) {
			monoTypeface = Typeface.createFromAsset(context.getAssets(), "fonts/VeraMono.ttf");
		}

		return monoTypeface;
	}

	public static Message handlerMessage(int what, Object obj) {
		final Message msg = Message.obtain();
		msg.what = what;
		msg.obj = obj;
		return msg;
	}

	public static void moveFile(final File src, final File dst) throws IOException {

		if(!src.renameTo(dst)) {

			copyFile(src, dst);

			if(!src.delete()) {
				src.deleteOnExit();
			}
		}
	}

	public static void copyFile(final File src, final File dst) throws IOException {

		final FileInputStream fis = new FileInputStream(src);
		final FileOutputStream fos = new FileOutputStream(dst);

		copyFile(fis, fos);
	}

	public static void copyFile(final InputStream fis, final File dst) throws IOException {
		final FileOutputStream fos = new FileOutputStream(dst);
		copyFile(fis, fos);
	}

	public static void copyFile(final InputStream fis, final OutputStream fos) throws IOException {

		final byte[] buf = new byte[32 * 1024];

		int bytesRead;
		while((bytesRead = fis.read(buf)) > 0) {
			fos.write(buf, 0, bytesRead);
		}

		fis.close();
		fos.close();
	}

	public static boolean isCacheDiskFull(final Context context) {
		final long space = getFreeSpaceAvailable(PrefsUtility.pref_cache_location(context,
				PreferenceManager.getDefaultSharedPreferences(context)));
		return space < 128 * 1024 * 1024;
	}

	/// Get the number of free bytes that are available on the external storage.
	@SuppressWarnings("deprecation")
	public static long getFreeSpaceAvailable(String path) {
		StatFs stat = new StatFs(path);
		long availableBlocks;
		long blockSize;
		if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.JELLY_BEAN_MR2) {
			availableBlocks = stat.getAvailableBlocksLong();
			blockSize = stat.getBlockSizeLong();
		} else {
			availableBlocks = stat.getAvailableBlocks();
			blockSize = stat.getBlockSize();
		}
		return availableBlocks * blockSize;
	}

	/** Takes a size in bytes and converts it into a human-readable
	 * String with units.
	 */
	public static String addUnits(final long input) {
		int i = 0;
		long result = input;
		while (i <= 3 && result >= 1024)
			result = input / (long) Math.pow(1024, ++i);

		switch (i) {
		case 1: return result + " KiB";
		case 2: return result + " MiB";
		case 3: return result + " GiB";
		default: return result + " B";
		}
	}

	public static String bytesToMegabytes(final long input) {
		final long totalKilobytes = input / 1024;
		final long totalMegabytes = totalKilobytes / 1024;
		final long remainder = totalKilobytes % 1024;
		return String.format(Locale.US, "%d.%02d MB", totalMegabytes, (remainder / 10));
	}

	public static int dpToPixels(final Context context, final float dp) {
		return Math.round(TypedValue.applyDimension(TypedValue.COMPLEX_UNIT_DIP, dp, context.getResources().getDisplayMetrics()));
	}

	public static int spToPixels(final Context context, final float sp) {
		return Math.round(TypedValue.applyDimension(TypedValue.COMPLEX_UNIT_SP, sp, context.getResources().getDisplayMetrics()));
	}

	public static void quickToast(final Context context, final int textRes) {
		quickToast(context, context.getString(textRes));
	}

	public static void quickToast(final Context context, final String text) {
		AndroidCommon.UI_THREAD_HANDLER.post(new Runnable() {
			@Override
			public void run() {
				Toast.makeText(context, text, Toast.LENGTH_LONG).show();
			}
		});
	}

	public static void quickToast(final Context context, final String text, final int duration) {
		AndroidCommon.UI_THREAD_HANDLER.post(new Runnable() {
			@Override
			public void run() {
				Toast.makeText(context, text, duration).show();
			}
		});
	}

	public static boolean isTablet(final Context context, final SharedPreferences sharedPreferences) {

		final PrefsUtility.AppearanceTwopane pref = PrefsUtility.appearance_twopane(context, sharedPreferences);

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
				BugReportActivity.handleGlobalError(context, "Unknown AppearanceTwopane value " + pref.name());
				return false;
		}
	}

	public static boolean isConnectionWifi(final Context context){
		final ConnectivityManager cm = (ConnectivityManager)context.getSystemService(Context.CONNECTIVITY_SERVICE);
		final NetworkInfo info = cm.getNetworkInfo(ConnectivityManager.TYPE_WIFI);
		return info != null && info.getDetailedState() == NetworkInfo.DetailedState.CONNECTED;
	}

	public static boolean isNetworkConnected(final Context context) {
		final ConnectivityManager cm = (ConnectivityManager)context.getSystemService(Context.CONNECTIVITY_SERVICE);
		final NetworkInfo activeNetworkInfo = cm.getActiveNetworkInfo();
		return activeNetworkInfo != null && activeNetworkInfo.isConnected();
	}

	public static RRError getGeneralErrorForFailure(Context context, @CacheRequest.RequestFailureType int type, Throwable t, Integer status, String url) {

		final int title, message;

		switch (type) {
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
				title = R.string.error_connection_title;
				message = R.string.error_connection_message;
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
					switch (status) {
						case 400:
						case 401:
						case 403: {
							final URI uri = General.uriFromString(url);
							final boolean isRedditRequest
									= uri != null
											&& uri.getHost() != null
											&& ("saidit.net".equalsIgnoreCase(uri.getHost())
													|| uri.getHost().endsWith(".saidit.net"));

							if(isRedditRequest) {
								title = R.string.error_403_title;
								message = R.string.error_403_message;
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
						case 502:
						case 503:
						case 504:
							title = R.string.error_redditdown_title;
							message = R.string.error_redditdown_message;
							break;
						default:
							title = R.string.error_unknown_api_title;
							message = R.string.error_unknown_api_message;
							break;
					}
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

			default:
				title = R.string.error_unknown_title;
				message = R.string.error_unknown_message;
				break;
		}

		return new RRError(context.getString(title), context.getString(message), t, status, url);
	}

	public static RRError getGeneralErrorForFailure(Context context, final APIResponseHandler.APIFailureType type) {

		final int title, message;

		switch(type) {

			case INVALID_USER:
				title = R.string.error_403_title;
				message = R.string.error_403_message;
				break;

			case BAD_CAPTCHA:
				title = R.string.error_bad_captcha_title;
				message = R.string.error_bad_captcha_message;
				break;

			case NOTALLOWED:
				title = R.string.error_403_title;
				message = R.string.error_403_message;
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

			default:
				title = R.string.error_unknown_api_title;
				message = R.string.error_unknown_api_message;
				break;
		}

		return new RRError(context.getString(title), context.getString(message));
	}

	// TODO add button to show more detail
	public static void showResultDialog(final AppCompatActivity context, final RRError error) {
		AndroidCommon.UI_THREAD_HANDLER.post(new Runnable() {
			@Override
			public void run() {
				try {
					final AlertDialog.Builder alertBuilder = new AlertDialog.Builder(context);
					alertBuilder.setNeutralButton(R.string.dialog_close, null);
					alertBuilder.setNegativeButton(R.string.button_moredetail, new DialogInterface.OnClickListener() {
						@Override
						public void onClick(DialogInterface dialog, int which) {
							ErrorPropertiesDialog.newInstance(error).show(context.getSupportFragmentManager(), "ErrorPropertiesDialog");
						}
					});
					alertBuilder.setTitle(error.title);
					alertBuilder.setMessage(error.message);
					alertBuilder.create().show();
				} catch(final WindowManager.BadTokenException e) {
					Log.e("General", "Tried to show result dialog after activity closed", e);
				}
			}
		});
	}

	private static final Pattern urlPattern = Pattern.compile("^(https?)://([^/]+)/+([^\\?#]+)((?:\\?[^#]+)?)((?:#.+)?)$");

	public static String filenameFromString(String url) {
		String filename = uriFromString(url).getPath().replace(File.separator, "");
		String[] parts = filename.substring(1).split("\\.", 2);
		if(parts.length < 2)
			filename += ".jpg";

		return filename;
	}

	public static URI uriFromString(String url) {

		try {
			return new URI(url);

		} catch(Throwable t1) {
			try {

				Log.i("RR DEBUG uri", "Beginning aggressive parse of '" + url + "'");

				final Matcher urlMatcher = urlPattern.matcher(url);

				if(urlMatcher.find()) {

					final String scheme = urlMatcher.group(1);
					final String authority = urlMatcher.group(2);
					final String path = urlMatcher.group(3).length() == 0 ? null : "/" + urlMatcher.group(3);
					final String query = urlMatcher.group(4).length() == 0 ? null : urlMatcher.group(4);
					final String fragment = urlMatcher.group(5).length() == 0 ? null : urlMatcher.group(5);

					try {
						return new URI(scheme, authority, path, query, fragment);
					} catch(Throwable t3) {

						if(path != null && path.contains(" ")) {
							return new URI(scheme, authority, path.replace(" ", "%20"), query, fragment);
						} else {
							return null;
						}
					}

				} else {
					return null;
				}

			} catch(Throwable t2) {
				return null;
			}
		}
	}

	public static String sha1(final byte[] plaintext) {

		final MessageDigest digest;
		try {
			digest = MessageDigest.getInstance("SHA-1");
		} catch(Exception e) {
			throw new RuntimeException(e);
		}

		digest.update(plaintext, 0, plaintext.length);
		final byte[] hash = digest.digest();
		final StringBuilder result = new StringBuilder(hash.length * 2);
		for(byte b : hash) result.append(String.format(Locale.US, "%02X", b));
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
			int next = query.indexOf('&', pos);
			int end = (next == -1) ? query.length() : next;

			int separator = query.indexOf('=', pos);
			if (separator > end || separator == -1) {
				separator = end;
			}

			String name = query.substring(pos, separator);
			names.add(Uri.decode(name));

			// Move start to end of name.
			pos = end + 1;
		}

		return Collections.unmodifiableSet(names);
	}

	public static int divideCeil(int num, int divisor) {
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

	public static <E> ArrayList<E> listOfOne(E obj) {
		final ArrayList<E> result = new ArrayList<>(1);
		result.add(obj);
		return result;
	}

	public static String asciiUppercase(final String input) {

		final char[] chars = input.toCharArray();

		for(int i = 0; i < chars.length; i++) {
			if(chars[i] >= 'a' && chars[i] <= 'z') {
				chars[i] -= 'a';
				chars[i] += 'A';
			}
		}

		return new String(chars);
	}

	@NonNull
	public static String asciiLowercase(@NonNull final String input) {

		final char[] chars = input.toCharArray();

		for(int i = 0; i < chars.length; i++) {
			if(chars[i] >= 'A' && chars[i] <= 'Z') {
				chars[i] -= 'A';
				chars[i] += 'a';
			}
		}

		return new String(chars);
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

	public static void setAllMarginsDp(final Context context, final View view, final int marginDp) {

		final ViewGroup.MarginLayoutParams layoutParams = (ViewGroup.MarginLayoutParams)view.getLayoutParams();

		final int marginPx = dpToPixels(context, marginDp);

		layoutParams.leftMargin = marginPx;
		layoutParams.rightMargin = marginPx;
		layoutParams.topMargin = marginPx;
		layoutParams.bottomMargin = marginPx;
	}

	public static void setLayoutMatchParent(final View view) {
		final ViewGroup.LayoutParams layoutParams = view.getLayoutParams();
		layoutParams.width = ViewGroup.LayoutParams.MATCH_PARENT;
		layoutParams.height = ViewGroup.LayoutParams.MATCH_PARENT;
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
		try {
			if(dialog.isShowing()) dialog.dismiss();
		} catch(final Exception e) {
			Log.e("safeDismissDialog", "Caught exception while dismissing dialog", e);
		}
	}

	public static void closeSafely(final Closeable closeable) {

		try {
			closeable.close();
		} catch(final IOException e) {
			Log.e("closeSafely", "Failed to close resource", e);
		}
	}
}
