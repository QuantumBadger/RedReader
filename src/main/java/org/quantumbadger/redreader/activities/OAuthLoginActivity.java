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

package org.quantumbadger.redreader.activities;

import android.annotation.SuppressLint;
import android.content.Intent;
import android.net.Uri;
import android.os.Build;
import android.os.Bundle;
import android.webkit.ConsoleMessage;
import android.webkit.CookieManager;
import android.webkit.WebChromeClient;
import android.webkit.WebResourceRequest;
import android.webkit.WebSettings;
import android.webkit.WebView;
import android.webkit.WebViewClient;
import androidx.webkit.WebSettingsCompat;
import androidx.webkit.WebViewFeature;
import java.util.Map;
import java.util.HashMap;
import android.os.Message;
import android.net.http.SslError;
import android.webkit.SslErrorHandler;

import androidx.browser.customtabs.CustomTabsIntent;


import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.RedReader;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.common.TorCommon;
import org.quantumbadger.redreader.reddit.api.RedditOAuth;

import java.util.Objects;

import info.guardianproject.netcipher.webkit.WebkitProxy;

public class OAuthLoginActivity extends ViewsBaseActivity {

	private static final String OAUTH_HOST = "rr_oauth_redir";
	private static final String REDREADER_SCHEME = "redreader";
	private static final String HTTP_SCHEME = "http";

	private WebView mWebView;

	@Override
	protected void onDestroy() {
		super.onDestroy();
		final CookieManager cookieManager = CookieManager.getInstance();
		cookieManager.removeAllCookies(null);
	}

	@SuppressLint({"SetJavaScriptEnabled"})
	@Override
	public void onCreate(final Bundle savedInstanceState) {
		PrefsUtility.applyTheme(this);
		super.onCreate(savedInstanceState);

		mWebView = new WebView(this);

		// 1) Cookies & storage must be fully enabled
		CookieManager cookieMgr = CookieManager.getInstance();
		cookieMgr.setAcceptCookie(true);
		// third-party cookies are required for many IdP flows
		CookieManager.getInstance().setAcceptThirdPartyCookies(mWebView, true);

		final WebSettings settings = mWebView.getSettings();
		settings.setJavaScriptEnabled(true);
		settings.setDomStorageEnabled(true);
		settings.setDatabaseEnabled(true);
		settings.setSupportMultipleWindows(true); // for popups/2FA
		settings.setJavaScriptCanOpenWindowsAutomatically(true);
		settings.setLoadWithOverviewMode(true);
		settings.setUseWideViewPort(true);
		settings.setDisplayZoomControls(false);

		// Optional but helps with recaptcha/challenges
		if (WebViewFeature.isFeatureSupported(WebViewFeature.SAFE_BROWSING_ENABLE)) {
			try {
				WebSettingsCompat.setSafeBrowsingEnabled(settings, true);
			} catch (Throwable ignored) { /* no-op */ }
		}
		if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP) {
			settings.setMixedContentMode(WebSettings.MIXED_CONTENT_COMPATIBILITY_MODE);
		}

		// 2) Use a mobile Chrome UA (many sites special-case WebView UA)
		settings.setUserAgentString(
				"Mozilla/5.0 (Linux; Android " + Build.VERSION.RELEASE + ") "
						+ "AppleWebKit/537.36 (KHTML, like Gecko) "
						+ "Chrome/124.0.0.0 Mobile Safari/537.36"
		);

		// Remove/neutralize the X-Requested-With header (WebView adds it by default);

		final Map<String,String> firstLoadHeaders = new HashMap<>();
		firstLoadHeaders.put("X-Requested-With", ""); // override to empty

		mWebView.setWebChromeClient(new WebChromeClient() {
			@Override public boolean onCreateWindow(WebView view, boolean isDialog,
													boolean isUserGesture, Message resultMsg) {
				// open popup targets inside the same webview
				WebView.HitTestResult result = view.getHitTestResult();
				WebView newWebView = new WebView(view.getContext());
				newWebView.setWebViewClient(new WebViewClient());
				((WebView.WebViewTransport) resultMsg.obj).setWebView(newWebView);
				resultMsg.sendToTarget();
				return true;
			}
			@Override public boolean onConsoleMessage(ConsoleMessage consoleMessage) { return true; }
		});

		mWebView.setWebViewClient(new WebViewClient() {
			@Override
			public boolean shouldOverrideUrlLoading(WebView view, WebResourceRequest req) {
				Uri url = req.getUrl();

				// intercept the oauth redirect back to the app
				if (Objects.equals(url.getScheme(), "redreader")) {
					Intent intent = new Intent();
					intent.putExtra("url", url.toString());
					setResult(123, intent);
					finish();
					return true;
				}

				// keep everything else inside, but scrub X-Requested-With on every nav
				Map<String,String> headers = new HashMap<>();
				headers.put("X-Requested-With", "");
				view.loadUrl(url.toString(), headers);
				return true; // we handled it
			}

			@Override
			public void onReceivedSslError(WebView view, SslErrorHandler handler, SslError error) {
				// be strict; do not proceed on SSL errors
				handler.cancel();
			}
		});

		setBaseActivityListing(mWebView);

		// load the Reddit authorize URL with headers that neutralize X-Requested-With
		mWebView.loadUrl(RedditOAuth.getPromptUri().toString(), firstLoadHeaders);
	}



	@Override
	protected void onPause() {

		super.onPause();

		if (mWebView != null) {
			mWebView.onPause();
			mWebView.pauseTimers();
		}
	}

	@Override
	protected void onResume() {
		super.onResume();

		if (mWebView != null) {
			mWebView.resumeTimers();
			mWebView.onResume();
		}
	}
}
