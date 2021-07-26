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

package org.quantumbadger.redreader.views.webview;

import android.annotation.SuppressLint;
import android.content.Context;
import android.os.Build;
import android.util.AttributeSet;
import android.util.Log;
import android.webkit.CookieManager;
import android.webkit.WebChromeClient;
import android.webkit.WebView;
import info.guardianproject.netcipher.web.WebkitProxy;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.RedReader;
import org.quantumbadger.redreader.activities.BugReportActivity;
import org.quantumbadger.redreader.common.AndroidCommon;
import org.quantumbadger.redreader.common.TorCommon;

import java.util.Map;

/**
 * Fixes the onWindowFocusChanged bug, by catching NullPointerException.
 * https://groups.google.com/d/topic/android-developers/ktbwY2gtLKQ/discussion
 *
 * @author Andrew
 * <p>
 * <p>
 * This class serves as a WebView to be used in conjunction with a VideoEnabledWebChromeClient. It
 * makes possible: - To detect the HTML5 video ended event so that the VideoEnabledWebChromeClient
 * can exit full-screen.
 * <p>
 * Important notes: - Javascript is enabled by default and must not be disabled with
 * getSettings().setJavaScriptEnabled(false). - setWebChromeClient() must be called before any
 * loadData(), loadDataWithBaseURL() or loadUrl() method.
 * <p>
 * For more information, see https://github.com/cprcrack/VideoEnabledWebView
 * @author Cristian Perez (http://cpr.name)
 */

// Taken from reddit-is-fun:
// https://github.com/talklittle/reddit-is-fun/blob/master/src/com/andrewshu/android/reddit/browser/WebViewFixed.java
// Also taken from cprcrack/VideoEnabledWebView
// https://github.com/cprcrack/VideoEnabledWebView/blob/master/app/src/main/java/name/cpr/VideoEnabledWebView.java
public class WebViewFixed extends WebView {

	public class JavascriptInterface {
		// Must match Javascript interface method of VideoEnabledWebChromeClient
		@android.webkit.JavascriptInterface
		@SuppressWarnings("unused")
		public void notifyVideoEnd() {
			// This code is not executed in the UI thread, so we must force that
			// to happen
			AndroidCommon.UI_THREAD_HANDLER.post(() -> {
				if(videoEnabledWebChromeClient != null) {
					videoEnabledWebChromeClient.onHideCustomView();
				}
			});
		}
	}

	private VideoEnabledWebChromeClient videoEnabledWebChromeClient;
	private boolean addedJavascriptInterface;

	public WebViewFixed(final Context context) {
		super(context);
		addedJavascriptInterface = false;
		setTor(context);
	}

	public WebViewFixed(
			final Context context,
			final AttributeSet attrs,
			final int defStyle) {
		super(context, attrs, defStyle);
		addedJavascriptInterface = false;
		setTor(context);
	}

	public WebViewFixed(final Context context, final AttributeSet attrs) {
		super(context, attrs);
		addedJavascriptInterface = false;
		setTor(context);
	}

	/**
	 * Indicates if the video is being displayed using a custom view (typically full-screen)
	 *
	 * @return true it the video is being displayed using a custom view (typically full-screen)
	 */
	@SuppressWarnings("unused")
	public boolean isVideoFullscreen() {
		return videoEnabledWebChromeClient != null
				&& videoEnabledWebChromeClient.isVideoFullscreen();
	}

	/**
	 * Pass only a VideoEnabledWebChromeClient instance.
	 */
	@Override
	@SuppressLint("SetJavaScriptEnabled")
	public void setWebChromeClient(final WebChromeClient client) {
		getSettings().setJavaScriptEnabled(true);

		if(client instanceof VideoEnabledWebChromeClient) {
			this.videoEnabledWebChromeClient = (VideoEnabledWebChromeClient)client;
		}

		super.setWebChromeClient(client);
	}

	@Override
	public void loadData(final String data, final String mimeType, final String encoding) {
		addJavascriptInterface();
		super.loadData(data, mimeType, encoding);
	}

	@Override
	public void loadDataWithBaseURL(
			final String baseUrl,
			final String data,
			final String mimeType,
			final String encoding,
			final String historyUrl) {
		addJavascriptInterface();
		super.loadDataWithBaseURL(baseUrl, data, mimeType, encoding, historyUrl);
	}

	public void loadHtmlUTF8WithBaseURL(final String baseUrl, final String html) {

		final String mimeType;

		if(Build.VERSION.SDK_INT < 21) {
			mimeType = "text/html";
		} else {
			mimeType = "text/html; charset=utf-8";
		}

		loadDataWithBaseURL(baseUrl, html, mimeType, "UTF-8", null);
	}

	@Override
	public void loadUrl(final String url) {
		addJavascriptInterface();
		super.loadUrl(url);
	}

	@Override
	public void loadUrl(final String url, final Map<String, String> additionalHttpHeaders) {
		addJavascriptInterface();
		super.loadUrl(url, additionalHttpHeaders);
	}

	@SuppressLint("AddJavascriptInterface")
	private void addJavascriptInterface() {
		if(!addedJavascriptInterface) {
			// Add javascript interface to be called when the video ends
			// (must be done before page load)
			// Must match Javascript interface name of VideoEnabledWebChromeClient
			addJavascriptInterface(new JavascriptInterface(), "_VideoEnabledWebView");

			addedJavascriptInterface = true;
		}
	}

	@Override
	public void onWindowFocusChanged(final boolean hasWindowFocus) {
		try {
			super.onWindowFocusChanged(hasWindowFocus);
		} catch(final NullPointerException ex) {
			Log.e("WebView", "WebView.onWindowFocusChanged", ex);
		}
	}

	private void setTor(final Context context) {
		if(TorCommon.isTorEnabled()) {
			try {
				clearBrowser();
				final boolean result = WebkitProxy.setProxy(
						RedReader.class.getCanonicalName(),
						context.getApplicationContext(),
						this,
						"127.0.0.1",
						8118);
				if(!result) {
					BugReportActivity.handleGlobalError(
							context,
							getResources().getString(R.string.error_tor_setting_failed));
				}
			} catch(final Exception e) {
				BugReportActivity.handleGlobalError(context, e);
			}
		}
	}

	public void clearBrowser() {
		this.clearCache(true);
		this.clearFormData();
		this.clearHistory();
		CookieManager.getInstance().removeAllCookie();
	}
}
