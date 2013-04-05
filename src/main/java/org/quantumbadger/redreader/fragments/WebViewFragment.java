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

import android.os.Bundle;
import android.view.View;
import android.view.ViewGroup;
import android.webkit.WebSettings;
import android.webkit.WebView;
import android.webkit.WebViewClient;
import org.holoeverywhere.LayoutInflater;
import org.holoeverywhere.app.Fragment;
import org.quantumbadger.redreader.views.WebViewFixed;

public class WebViewFragment extends Fragment {

	private String url;

	private WebViewFixed webView;

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

		webView = new WebViewFixed(container.getContext());

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
		});

		webView.loadUrl(url);

		return webView;
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
