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
import android.content.Context;
import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.webkit.ConsoleMessage;
import android.webkit.CookieManager;
import android.webkit.WebChromeClient;
import android.webkit.WebSettings;
import android.webkit.WebView;
import android.webkit.WebViewClient;

import androidx.activity.result.contract.ActivityResultContract;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import info.guardianproject.netcipher.webkit.WebkitProxy;

import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.RedReader;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.common.TorCommon;
import org.quantumbadger.redreader.reddit.api.RedditOAuth;

public class OAuthLoginActivity extends ViewsBaseActivity {

	private WebView mWebView;

	@Override
	protected void onDestroy() {
		super.onDestroy();
		final CookieManager cookieManager = CookieManager.getInstance();
		cookieManager.removeAllCookie();
	}

	@SuppressLint("SetJavaScriptEnabled")
	@Override
	public void onCreate(final Bundle savedInstanceState) {

		PrefsUtility.applyTheme(this);

		super.onCreate(savedInstanceState);

		mWebView = new WebView(this);

		if(TorCommon.isTorEnabled()) {
			try {
				final boolean result = WebkitProxy.setProxy(
						RedReader.class.getCanonicalName(),
						getApplicationContext(),
						mWebView,
						"127.0.0.1",
						8118);
				if(!result) {
					BugReportActivity.handleGlobalError(
							this,
							getResources().getString(R.string.error_tor_setting_failed));
				}
			} catch(final Exception e) {
				BugReportActivity.handleGlobalError(this, e);
			}
		}

		final WebSettings settings = mWebView.getSettings();

		settings.setBuiltInZoomControls(false);
		settings.setJavaScriptEnabled(true);
		settings.setJavaScriptCanOpenWindowsAutomatically(false);
		settings.setUseWideViewPort(true);
		settings.setLoadWithOverviewMode(true);
		settings.setDomStorageEnabled(true);
		settings.setSaveFormData(false);
		settings.setSavePassword(false);
		settings.setDatabaseEnabled(false);
		settings.setCacheMode(WebSettings.LOAD_NO_CACHE);
		settings.setDisplayZoomControls(false);

		mWebView.setWebChromeClient(new WebChromeClient() {
			@Override
			public boolean onConsoleMessage(final ConsoleMessage consoleMessage) {
				return true;
			}
		});

		mWebView.setWebViewClient(new WebViewClient() {
			@Override
			public boolean shouldOverrideUrlLoading(
					final WebView view,
					final String url) {

				if(url.startsWith("http://rr_oauth_redir")
						|| url.startsWith("redreader://rr_oauth_redir")) { // TODO constant

					final Intent intent = new Intent();
					intent.setData(Uri.parse(url));
					setResult(RESULT_OK, intent);
					finish();

				} else {
					setTitle(Uri.parse(url).getHost());
					return false;
				}

				return true;
			}
		});

		setBaseActivityListing(mWebView);

		mWebView.loadUrl(RedditOAuth.getPromptUri().toString());
	}

	@Override
	protected void onPause() {

		super.onPause();

		if(mWebView != null) {
			mWebView.onPause();
			mWebView.pauseTimers();
		}
	}

	@Override
	protected void onResume() {
		super.onResume();

		if(mWebView != null) {
			mWebView.resumeTimers();
			mWebView.onResume();
		}
	}

	public static class ResultContract extends ActivityResultContract<Void, Uri> {
		@NonNull
		@Override
		public Intent createIntent(@NonNull final Context context, final Void unused) {
			return new Intent(context, OAuthLoginActivity.class);
		}

		@Override
		public Uri parseResult(final int resultCode, @Nullable final Intent intent) {
			if (resultCode == RESULT_OK && intent != null) {
				return intent.getData();
			}
			return null;
		}
	}
}
