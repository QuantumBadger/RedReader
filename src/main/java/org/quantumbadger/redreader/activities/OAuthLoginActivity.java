/*******************************************************************************
 * This file is part of Scroll.
 *
 * Scroll is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Scroll is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Scroll.  If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/

package com.konneh.scroll.activities;

import android.annotation.SuppressLint;
import android.annotation.TargetApi;
import android.content.Intent;
import android.os.Build;
import android.os.Bundle;
import android.view.MenuItem;
import android.webkit.*;
import com.konneh.scroll.common.AndroidApi;
import com.konneh.scroll.common.PrefsUtility;
import com.konneh.scroll.reddit.api.RedditOAuth;

import java.io.ByteArrayInputStream;

public class OAuthLoginActivity extends BaseActivity {

	private WebView mWebView;

	private static final String CSS_FIXES
			= "li {\n" +
			"  list-style-type: none;\n" +
			"  margin:10px\n" +
			"}\n" +
			"\n" +
			"label {\n" +
			"  margin-right: 10px;\n" +
			"}\n" +
			"\n" +
			"div.icon, div.infobar, div.mobile-web-redirect-bar, div#topbar {\n" +
			"  display: none;\n" +
			"  visibility: collapse;\n" +
			"  height: 0px;\n" +
			"  padding: 0px;\n" +
			"  margin:0px;\n" +
			"}\n" +
			"\n" +
			"div.content {\n" +
			"  padding: 0px;\n" +
			"  margin: 20px;\n" +
			"}\n" +
			"\n" +
			"body {\n" +
			"  background-color: #FFF;\n" +
			"}\n" +
			"\n" +
			"input.newbutton {\n" +
			"  background-color: #888;\n" +
			"  font-size: 20pt;\n" +
			"  margin: 10px;\n" +
			"  border-image-source: none;\n" +
			"  color: #FFF;\n" +
			"  border: none;\n" +
			"  padding-left:10px;\n" +
			"  padding-right:10px;\n" +
			"  padding-top:6px;\n" +
			"  padding-bottom:6px;\n" +
			"}\n" +
			"\n" +
			"button {\n" +
			"  background-color: #888;\n" +
			"  font-size: 15pt;\n" +
			"  border-image-source: none;\n" +
			"  color: #FFF;\n" +
			"  border: none;\n" +
			"  padding-left:10px;\n" +
			"  padding-right:10px;\n" +
			"  padding-top:6px;\n" +
			"  padding-bottom:6px;\n" +
			"}\n" +
			"\n" +
			"input.allow {\n" +
			"  background-color: #0A0;\n" +
			"}\n" +
			"\n" +
			"input.allow:active, input.allow:hover {\n" +
			"  background-color: #0F0;\n" +
			"}\n" +
			"\n" +
			"input.decline {\n" +
			"  background-color: #A00;\n" +
			"}\n" +
			"\n" +
			"input.decline:active, input.decline:hover {\n" +
			"  background-color: #F00;\n" +
			"}\n" +
			"\n" +
			"form.pretty-form {\n" +
			"  float: left;\n" +
			"}\n" +
			"\n";

	@Override
	protected void onDestroy() {
		super.onDestroy();
		final CookieManager cookieManager = CookieManager.getInstance();
		cookieManager.removeAllCookie();
	}

	public void onCreate(final Bundle savedInstanceState) {

		PrefsUtility.applyTheme(this);

		getActionBar().setHomeButtonEnabled(true);
		getActionBar().setDisplayHomeAsUpEnabled(true);

		super.onCreate(savedInstanceState);

		mWebView = new WebView(this);

		final WebSettings settings = mWebView.getSettings();

		settings.setBuiltInZoomControls(false);
		settings.setJavaScriptEnabled(false);
		settings.setJavaScriptCanOpenWindowsAutomatically(false);
		settings.setUseWideViewPort(true);
		settings.setLoadWithOverviewMode(true);
		settings.setDomStorageEnabled(false);
		settings.setSaveFormData(false);
		settings.setSavePassword(false);
		settings.setDatabaseEnabled(false);
		settings.setAppCacheEnabled(false);
		disableZoomDeprecated(settings);

		setTitle(RedditOAuth.getPromptUri().toString());
		mWebView.loadUrl(RedditOAuth.getPromptUri().toString());

		mWebView.setWebViewClient(new WebViewClient() {
			@Override
			public boolean shouldOverrideUrlLoading(final WebView view, final String url) {

				if(url.startsWith("http://rr_oauth_redir")) { // TODO constant

					final Intent intent = new Intent();
					intent.putExtra("url", url);
					setResult(123, intent);
					finish();

				} else {
					setTitle(url);
					mWebView.loadUrl(url);
				}

				return true;
			}

			@Override
			@TargetApi(Build.VERSION_CODES.HONEYCOMB)
			public WebResourceResponse shouldInterceptRequest(final WebView view, final String url) {

				if(url.matches(".*compact.*\\.css")) {
					return new WebResourceResponse("text/css", "UTF-8", new ByteArrayInputStream(CSS_FIXES.getBytes()));
				}

				return null;
			}
		});

		setContentView(mWebView);
	}

	@Override
	@SuppressLint("NewApi")
	protected void onPause() {

		super.onPause();

		if(mWebView != null) {

			if(AndroidApi.isHoneyCombOrLater()) {
				mWebView.onPause();
			}

			mWebView.pauseTimers();
		}
	}

	@Override
	@SuppressLint("NewApi")
	protected void onResume() {
		super.onResume();

		if(mWebView != null) {

			mWebView.resumeTimers();

			if(AndroidApi.isHoneyCombOrLater()) {
				mWebView.onResume();
			}
		}
	}

	@SuppressLint("NewApi")
	private static void disableZoomDeprecated(final WebSettings settings) {
		if (AndroidApi.isHoneyCombOrLater()) {
			settings.setDisplayZoomControls(false);
		}
	}

	@Override
	public boolean onOptionsItemSelected(final MenuItem item) {

		switch(item.getItemId()) {

			case android.R.id.home:
				finish();
				return true;

			default:
				return super.onOptionsItemSelected(item);
		}
	}

}