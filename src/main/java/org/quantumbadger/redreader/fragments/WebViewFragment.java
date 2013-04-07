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

package org.quantumbadger.redreader.fragments;

import android.graphics.Bitmap;
import android.os.Bundle;
import android.view.View;
import android.view.ViewGroup;
import android.webkit.WebChromeClient;
import android.webkit.WebSettings;
import android.webkit.WebView;
import android.webkit.WebViewClient;
import org.holoeverywhere.LayoutInflater;
import org.holoeverywhere.app.Fragment;
import org.holoeverywhere.widget.FrameLayout;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.views.WebViewFixed;
import org.quantumbadger.redreader.views.liststatus.LoadingView;

public class WebViewFragment extends Fragment {

	private String url;

	private WebViewFixed webView;
	private LoadingView loadingView;

	public static WebViewFragment newInstance(final String url) {

		final WebViewFragment f = new WebViewFragment();

		final Bundle bundle = new Bundle(1);
		bundle.putString("url", url);
		f.setArguments(bundle);

		return f;
	}

	@Override
	public void onCreate(final Bundle savedInstanceState) {
		// TODO load position/etc?
		super.onCreate(savedInstanceState);
		url = getArguments().getString("url");
	}

	@Override
	public View onCreateView(final LayoutInflater inflater, final ViewGroup container, final Bundle savedInstanceState) {

		final FrameLayout outer = (FrameLayout)inflater.inflate(R.layout.web_view_fragment);

		webView = (WebViewFixed)outer.findViewById(R.id.web_view_fragment_webviewfixed);
		final FrameLayout loadingViewFrame = (FrameLayout)outer.findViewById(R.id.web_view_fragment_loadingview_frame);

		loadingView = new LoadingView(inflater.getContext());
		loadingViewFrame.addView(loadingView);

		final WebSettings settings = webView.getSettings();

		settings.setBuiltInZoomControls(true);
		settings.setJavaScriptEnabled(true);
		settings.setJavaScriptCanOpenWindowsAutomatically(false);
		settings.setUseWideViewPort(true);
		settings.setLoadWithOverviewMode(true);

		try {
			settings.setDisplayZoomControls(false);
		} catch(NoSuchMethodError e) {
			// Old version of Android...
		}

		// TODO handle long clicks

		webView.setWebViewClient(new WebViewClient() {
			@Override
			public boolean shouldOverrideUrlLoading(final WebView view, final String url) {
				// TODO handle reddit URLs in the app
				webView.loadUrl(url);
				return true;
			}

			@Override
			public void onPageStarted(WebView view, String url, Bitmap favicon) {
				super.onPageStarted(view, url, favicon);
				getSupportActivity().setTitle(url);
			}
		});

		webView.setWebChromeClient(new WebChromeClient() {
			@Override
			public void onProgressChanged(WebView view, int newProgress) {

				super.onProgressChanged(view, newProgress);

				loadingView.setProgress(R.string.download_downloading, (float)newProgress / 100.0f);
				loadingView.setVisibility(newProgress == 100 ? View.GONE : View.VISIBLE);
			}
		});


		webView.loadUrl(url);

		return outer;
	}

	public boolean onBackButtonPressed() {

		/*
		if(webView.canGoBack()) {
			webView.goBack();
			return true;
		}*/ // Websites with redirects cause this to fail

		return false;
	}
}
