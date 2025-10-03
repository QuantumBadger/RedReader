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

	@SuppressLint("SetJavaScriptEnabled")
	@Override
	protected void onCreate(final Bundle savedInstanceState) {
		PrefsUtility.applyTheme(this);
		super.onCreate(savedInstanceState);

		// Build the OAuth authorize URL
		final android.net.Uri authUri =
				org.quantumbadger.redreader.reddit.api.RedditOAuth.getPromptUri();

		try {
			// Try Chrome Custom Tabs first (needs androidx.browser dependency)
			androidx.browser.customtabs.CustomTabsIntent.Builder builder =
					new androidx.browser.customtabs.CustomTabsIntent.Builder();
			androidx.browser.customtabs.CustomTabsIntent cti = builder.build();
			cti.launchUrl(this, authUri);
		} catch (Throwable ignored) {
			// Fallback: plain external browser
			final Intent i = new Intent(Intent.ACTION_VIEW, authUri);
			// Ensure we don’t accidentally route back to ourselves
			i.addCategory(Intent.CATEGORY_BROWSABLE);
			i.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
			startActivity(i);
		}


		finish();
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
