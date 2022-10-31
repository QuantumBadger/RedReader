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

package org.saiditnet.redreader.views.webview;

import android.annotation.SuppressLint;
import android.content.Context;
import android.os.Handler;
import android.os.Looper;
import android.util.AttributeSet;
import android.util.Log;
import android.webkit.CookieManager;
import android.webkit.WebChromeClient;
import android.webkit.WebView;
import info.guardianproject.netcipher.web.WebkitProxy;
import org.saiditnet.redreader.R;
import org.saiditnet.redreader.RedReader;
import org.saiditnet.redreader.activities.BugReportActivity;
import org.saiditnet.redreader.common.TorCommon;

import java.util.Map;

/**
 * Fixes the onWindowFocusChanged bug, by catching NullPointerException.
 * https://groups.google.com/d/topic/android-developers/ktbwY2gtLKQ/discussion
 * @author Andrew
 *
 *
 * This class serves as a WebView to be used in conjunction with a VideoEnabledWebChromeClient.
 * It makes possible:
 * - To detect the HTML5 video ended event so that the VideoEnabledWebChromeClient can exit full-screen.
 *
 * Important notes:
 * - Javascript is enabled by default and must not be disabled with getSettings().setJavaScriptEnabled(false).
 * - setWebChromeClient() must be called before any loadData(), loadDataWithBaseURL() or loadUrl() method.
 *
 * For more information, see https://github.com/cprcrack/VideoEnabledWebView
 * @author Cristian Perez (http://cpr.name)
 *
 */

// Taken from reddit-is-fun:
// https://github.com/talklittle/reddit-is-fun/blob/master/src/com/andrewshu/android/reddit/browser/WebViewFixed.java
// Also taken from cprcrack/VideoEnabledWebView
// https://github.com/cprcrack/VideoEnabledWebView/blob/master/app/src/main/java/name/cpr/VideoEnabledWebView.java
public class WebViewFixed extends WebView {

	public class JavascriptInterface
	{
		@android.webkit.JavascriptInterface @SuppressWarnings("unused")
		public void notifyVideoEnd() // Must match Javascript interface method of VideoEnabledWebChromeClient
		{
			// This code is not executed in the UI thread, so we must force that to happen
			new Handler(Looper.getMainLooper()).post(new Runnable()
			{
				@Override
				public void run()
				{
					if (videoEnabledWebChromeClient != null)
					{
						videoEnabledWebChromeClient.onHideCustomView();
					}
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

	public WebViewFixed(final Context context, final AttributeSet attrs, final int defStyle) {
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
	 * @return true it the video is being displayed using a custom view (typically full-screen)
	 */
	@SuppressWarnings("unused")
	public boolean isVideoFullscreen()
	{
		return videoEnabledWebChromeClient != null && videoEnabledWebChromeClient.isVideoFullscreen();
	}

	/**
	 * Pass only a VideoEnabledWebChromeClient instance.
	 */
	@Override @SuppressLint("SetJavaScriptEnabled")
	public void setWebChromeClient(WebChromeClient client)
	{
		getSettings().setJavaScriptEnabled(true);

		if (client instanceof VideoEnabledWebChromeClient)
		{
			this.videoEnabledWebChromeClient = (VideoEnabledWebChromeClient) client;
		}

		super.setWebChromeClient(client);
	}

	@Override
	public void loadData(String data, String mimeType, String encoding)
	{
		addJavascriptInterface();
		super.loadData(data, mimeType, encoding);
	}

	@Override
	public void loadDataWithBaseURL(String baseUrl, String data, String mimeType, String encoding, String historyUrl)
	{
		addJavascriptInterface();
		super.loadDataWithBaseURL(baseUrl, data, mimeType, encoding, historyUrl);
	}

	@Override
	public void loadUrl(String url)
	{
		addJavascriptInterface();
		super.loadUrl(url);
	}

	@Override
	public void loadUrl(String url, Map<String, String> additionalHttpHeaders)
	{
		addJavascriptInterface();
		super.loadUrl(url, additionalHttpHeaders);
	}

	private void addJavascriptInterface()
	{
		if (!addedJavascriptInterface)
		{
			// Add javascript interface to be called when the video ends (must be done before page load)
			//noinspection all
			addJavascriptInterface(new JavascriptInterface(), "_VideoEnabledWebView"); // Must match Javascript interface name of VideoEnabledWebChromeClient

			addedJavascriptInterface = true;
		}
	}

	@Override
	public void onWindowFocusChanged(final boolean hasWindowFocus) {
		try {
			super.onWindowFocusChanged(hasWindowFocus);
		} catch (NullPointerException ex) {
			Log.e("WebView", "WebView.onWindowFocusChanged", ex);
		}
	}

	private void setTor(final Context context) {
		if(TorCommon.isTorEnabled()) {
			try {
				clearBrowser();
				boolean result = WebkitProxy.setProxy(RedReader.class.getCanonicalName(), context.getApplicationContext(), this, "127.0.0.1", 8118);
				if(!result) {
					BugReportActivity.handleGlobalError(context, getResources().getString(R.string.error_tor_setting_failed));
				}
			} catch (Exception e) {
				e.printStackTrace();
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
