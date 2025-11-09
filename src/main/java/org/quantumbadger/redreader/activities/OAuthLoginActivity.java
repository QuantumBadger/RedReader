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
import android.os.Message;
import android.util.Log;
import android.webkit.ConsoleMessage;
import android.webkit.CookieManager;
import android.webkit.WebChromeClient;
import android.webkit.WebResourceRequest;
import android.webkit.WebResourceResponse;
import android.webkit.WebSettings;
import android.webkit.WebView;
import android.webkit.WebViewClient;

import org.jetbrains.annotations.Nullable;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.RedReader;
import org.quantumbadger.redreader.common.DialogUtils;
import org.quantumbadger.redreader.common.LinkHandler;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.common.TorCommon;
import org.quantumbadger.redreader.reddit.api.RedditOAuth;

import java.util.ArrayList;
import java.util.Objects;

import info.guardianproject.netcipher.webkit.WebkitProxy;

public class OAuthLoginActivity extends ViewsBaseActivity {

	private static final String TAG = "OAuthLoginActivity";

	private static final String OAUTH_HOST = "rr_oauth_redir";
	private static final String REDREADER_SCHEME = "redreader";
	private static final String HTTP_SCHEME = "http";

	private final ArrayList<WebView> webViewStack = new ArrayList<>();

	@Override
	protected void onDestroy() {
		super.onDestroy();

		clearBaseActivityListing();

		for (final WebView w : webViewStack) {
			w.destroy();
		}

		final CookieManager cookieManager = CookieManager.getInstance();
		cookieManager.removeAllCookies(null);
	}

	@SuppressLint("SetJavaScriptEnabled")
	private @Nullable WebView createWebView() {
		final WebView view = new WebView(this);

		final CookieManager cookieManager = CookieManager.getInstance();
		cookieManager.removeAllCookies(null);
		cookieManager.setAcceptCookie(true);
		CookieManager.getInstance().setAcceptThirdPartyCookies(view, true);

		if (TorCommon.isTorEnabled()) {
			try {
				final boolean result = WebkitProxy.setProxy(
						RedReader.class.getCanonicalName(),
						getApplicationContext(),
						view,
						"127.0.0.1",
						8118);
				if (!result) {
					BugReportActivity.handleGlobalError(
							this,
							getResources().getString(R.string.error_tor_setting_failed));
					return null;
				}
			} catch (final Exception e) {
				BugReportActivity.handleGlobalError(this, e);
				return null;
			}
		}

		final WebSettings settings = view.getSettings();

		settings.setBuiltInZoomControls(false);
		settings.setJavaScriptEnabled(true);
		settings.setUseWideViewPort(true);
		settings.setLoadWithOverviewMode(true);
		settings.setDomStorageEnabled(true);
		if (Build.VERSION.SDK_INT < Build.VERSION_CODES.O) {
			settings.setSaveFormData(false);
		}
		settings.setDatabaseEnabled(false);
		settings.setCacheMode(WebSettings.LOAD_NO_CACHE);
		settings.setDisplayZoomControls(false);

		// Suggested by Reddit to work around ReCAPTCHA issues
		settings.setSupportMultipleWindows(true);
		settings.setJavaScriptCanOpenWindowsAutomatically(true);

		view.setWebChromeClient(new WebChromeClient() {
			@Override
			public boolean onConsoleMessage(final ConsoleMessage consoleMessage) {
				return true;
			}

			@Override
			public boolean onCreateWindow(
					final WebView view,
					final boolean isDialog,
					final boolean isUserGesture,
					final Message resultMsg) {

				// https://stackoverflow.com/a/11280814
				Log.i(TAG, "New window created");
				final WebView newWebView = createWebView();
				webViewStack.add(newWebView);
				setBaseActivityListing(newWebView);
				final WebView.WebViewTransport transport = (WebView.WebViewTransport) resultMsg.obj;
				transport.setWebView(newWebView);
				resultMsg.sendToTarget();
				return true;
			}

			@Override
			public void onCloseWindow(final WebView window) {
				if (webViewStack.size() > 1) {
					final WebView removed = webViewStack.remove(webViewStack.size() - 1);
					removed.destroy();
					setBaseActivityListing(webViewStack.get(webViewStack.size() - 1));
				}
			}
		});

		view.setWebViewClient(new WebViewClient() {
			@Override
			public boolean shouldOverrideUrlLoading(
					final WebView view,
					final WebResourceRequest request) {

				final Uri url = request.getUrl();
				if (Objects.equals(url.getHost(), OAUTH_HOST) &&
						(Objects.equals(url.getScheme(), REDREADER_SCHEME) ||
								Objects.equals(url.getScheme(), HTTP_SCHEME))) {
					final Intent intent = new Intent();
					intent.putExtra("url", url.toString());
					setResult(123, intent);
					finish();

				} else {
					setTitle(url.getHost());
					return false;
				}

				return true;
			}

			@Override
			public void onReceivedHttpError(
					final WebView view,
					final WebResourceRequest request,
					final WebResourceResponse errorResponse) {

				// onReceivedHttpError: https://www.reddit.com/svc/shreddit/account/login, error = 401
				Log.e(TAG, "onReceivedHttpError: "
						+ request.getUrl()
						+ ", error = "
						+ errorResponse.getStatusCode());

				if (request.getUrl().toString().equals("https://www.reddit.com/svc/shreddit/account/login")
						&& errorResponse.getStatusCode() / 100 == 4) {
					DialogUtils.showDialogPositiveNegative(
							OAuthLoginActivity.this,
							getString(R.string.login_reddit_error_title),
							getString(R.string.login_reddit_error_message),
							R.string.dialog_continue,
							R.string.dialog_cancel,
							() -> {
								LinkHandler.openCustomTab(
										OAuthLoginActivity.this,
										RedditOAuth.getPromptUri(),
										null,
										false
								);
								finish();
							},
							() -> {
								finish();
							}
					);
				}
			}
		});

		return view;
	}

	@SuppressLint("SetJavaScriptEnabled")
	@Override
	public void onCreate(final Bundle savedInstanceState) {

		PrefsUtility.applyTheme(this);

		super.onCreate(savedInstanceState);

		final WebView webView = createWebView();

		if (webView != null) {
			webViewStack.add(webView);
			setBaseActivityListing(webView);
			webView.loadUrl(RedditOAuth.getPromptUri().toString());
		}
	}

	@Override
	protected void onPause() {

		super.onPause();

		for (final WebView w : webViewStack) {
			w.onPause();
			w.pauseTimers();
		}
	}

	@Override
	protected void onResume() {
		super.onResume();

		for (final WebView w : webViewStack) {
			w.resumeTimers();
			w.onResume();
		}
	}
}
