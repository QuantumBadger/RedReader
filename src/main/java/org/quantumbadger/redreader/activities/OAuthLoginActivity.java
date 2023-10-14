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
import android.os.Bundle;
import android.webkit.ConsoleMessage;
import android.webkit.CookieManager;
import android.webkit.WebChromeClient;
import android.webkit.WebSettings;
import android.webkit.WebView;
import android.webkit.WebViewClient;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import info.guardianproject.netcipher.web.WebkitProxy;
import org.mozilla.geckoview.AllowOrDeny;
import org.mozilla.geckoview.GeckoResult;
import org.mozilla.geckoview.GeckoRuntime;
import org.mozilla.geckoview.GeckoRuntimeSettings;
import org.mozilla.geckoview.GeckoSession;
import org.mozilla.geckoview.GeckoSessionSettings;
import org.mozilla.geckoview.GeckoView;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.RedReader;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.common.TorCommon;
import org.quantumbadger.redreader.reddit.api.RedditOAuth;

import java.net.URI;
import java.util.List;

public class OAuthLoginActivity extends BaseActivity {

	private static GeckoRuntime sRuntime;

	private GeckoView mWebView;

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

		mWebView = new GeckoView(this);

		final GeckoRuntimeSettings.Builder runtimeSettings = new GeckoRuntimeSettings.Builder();

		runtimeSettings.debugLogging(false);

		if(TorCommon.isTorEnabled()) {
			try {
				// TODO test
				runtimeSettings.arguments(new String[] {"socks://127.0.0.1:8118"});
			} catch(final Exception e) {
				BugReportActivity.handleGlobalError(this, e);
			}
		}

		// TODO destroy?
		final GeckoSession session = new GeckoSession(new GeckoSessionSettings.Builder()
				.usePrivateMode(true)
				.build());

		// Workaround for Bug 1758212
		session.setContentDelegate(new GeckoSession.ContentDelegate() {});

		if (sRuntime == null) {
			// GeckoRuntime can only be initialized once per process
			sRuntime = GeckoRuntime.create(this, runtimeSettings.build());
		}

		session.open(sRuntime);
		mWebView.setSession(session);

		setTitle(RedditOAuth.getPromptUri().toString());

		session.setNavigationDelegate(new GeckoSession.NavigationDelegate() {

			@Nullable
			@Override
			public GeckoResult<AllowOrDeny> onLoadRequest(
					@NonNull GeckoSession session,
					@NonNull LoadRequest request) {

				if(request.uri.startsWith("http://rr_oauth_redir")
						|| request.uri.startsWith("redreader://rr_oauth_redir")) { // TODO constant

					final Intent intent = new Intent();
					intent.putExtra("url", request.uri);
					setResult(123, intent);
					finish();

				} else {
					setTitle(General.mapIfNotNull(General.uriFromString(request.uri), URI::getHost));
				}

				return GeckoResult.allow();
			}
		});

		setBaseActivityListing(mWebView);

		session.loadUri(RedditOAuth.getPromptUri().toString());
	}
}
