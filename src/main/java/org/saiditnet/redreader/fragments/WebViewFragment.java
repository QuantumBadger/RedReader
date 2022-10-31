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

package org.saiditnet.redreader.fragments;

import android.content.DialogInterface;
import android.content.Intent;
import android.graphics.Bitmap;
import android.net.Uri;
import android.os.Bundle;
import android.preference.PreferenceManager;
import android.support.v4.app.Fragment;
import android.support.v7.app.AlertDialog;
import android.support.v7.app.AppCompatActivity;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.WindowManager;
import android.webkit.CookieManager;
import android.webkit.CookieSyncManager;
import android.webkit.DownloadListener;
import android.webkit.WebSettings;
import android.webkit.WebView;
import android.webkit.WebViewClient;
import android.widget.FrameLayout;
import android.widget.ProgressBar;
import android.widget.Toast;

import org.saiditnet.redreader.R;
import org.saiditnet.redreader.cache.CacheManager;
import org.saiditnet.redreader.common.AndroidCommon;
import org.saiditnet.redreader.common.General;
import org.saiditnet.redreader.common.LinkHandler;
import org.saiditnet.redreader.common.PrefsUtility;
import org.saiditnet.redreader.reddit.prepared.RedditParsedPost;
import org.saiditnet.redreader.reddit.prepared.RedditPreparedPost;
import org.saiditnet.redreader.reddit.things.RedditPost;
import org.saiditnet.redreader.reddit.url.RedditURLParser;
import org.saiditnet.redreader.views.RedditPostView;
import org.saiditnet.redreader.views.webview.VideoEnabledWebChromeClient;
import org.saiditnet.redreader.views.webview.WebViewFixed;
import org.saiditnet.redreader.views.bezelmenu.BezelSwipeOverlay;
import org.saiditnet.redreader.views.bezelmenu.SideToolbarOverlay;

import java.util.Locale;
import java.util.Timer;
import java.util.TimerTask;

public class WebViewFragment extends Fragment implements RedditPostView.PostSelectionListener {

	private AppCompatActivity mActivity;

	private String mUrl, html;
	private volatile String currentUrl;
	private volatile boolean goingBack;
	private volatile int lastBackDepthAttempt;

	private WebViewFixed webView;
	private ProgressBar progressView;
	private FrameLayout outer;

	public static WebViewFragment newInstance(final String url, final RedditPost post) {

		final WebViewFragment f = new WebViewFragment();

		final Bundle bundle = new Bundle(1);
		bundle.putString("url", url);
		if (post != null) bundle.putParcelable("post", post);
		f.setArguments(bundle);

		return f;
	}

	public static WebViewFragment newInstanceHtml(final String html) {

		final WebViewFragment f = new WebViewFragment();

		final Bundle bundle = new Bundle(1);
		bundle.putString("html", html);
		f.setArguments(bundle);

		return f;
	}

	@Override
	public void onCreate(final Bundle savedInstanceState) {
		// TODO load position/etc?
		super.onCreate(savedInstanceState);
		mUrl = getArguments().getString("url");
		html = getArguments().getString("html");
	}

	@Override
	public View onCreateView(final LayoutInflater inflater, final ViewGroup container, final Bundle savedInstanceState) {

		mActivity = (AppCompatActivity) getActivity();

		CookieSyncManager.createInstance(mActivity);

		outer = (FrameLayout) inflater.inflate(R.layout.web_view_fragment, null);

		final RedditPost src_post = getArguments().getParcelable("post");
		final RedditPreparedPost post;

		if (src_post != null) {

			final RedditParsedPost parsedPost = new RedditParsedPost(src_post, false);

			post = new RedditPreparedPost(
					mActivity,
					CacheManager.getInstance(mActivity),
					0,
					parsedPost,
					-1,
					false,
					false);

		} else {
			post = null;
		}

		webView = (WebViewFixed) outer.findViewById(R.id.web_view_fragment_webviewfixed);
		final FrameLayout loadingViewFrame = (FrameLayout) outer.findViewById(R.id.web_view_fragment_loadingview_frame);

		progressView = new ProgressBar(mActivity, null, android.R.attr.progressBarStyleHorizontal);
		loadingViewFrame.addView(progressView);
		loadingViewFrame.setPadding(General.dpToPixels(mActivity, 10), 0, General.dpToPixels(mActivity, 10), 0);
		final FrameLayout fullscreenViewFrame = (FrameLayout) outer.findViewById(R.id.web_view_fragment_fullscreen_frame);

		VideoEnabledWebChromeClient chromeClient = new VideoEnabledWebChromeClient(loadingViewFrame, fullscreenViewFrame) {
			@Override
			public void onProgressChanged(WebView view, final int newProgress) {

				super.onProgressChanged(view, newProgress);

				AndroidCommon.UI_THREAD_HANDLER.post(new Runnable() {
					@Override
					public void run() {
						progressView.setProgress(newProgress);
						progressView.setVisibility(newProgress == 100 ? View.GONE : View.VISIBLE);
					}
				});
			}
		};

		chromeClient.setOnToggledFullscreen(new VideoEnabledWebChromeClient.ToggledFullscreenCallback()
		{
			@Override
			public void toggledFullscreen(boolean fullscreen)
			{
				// Your code to handle the full-screen change, for example showing and hiding the title bar. Example:
				if (fullscreen)
				{
					WindowManager.LayoutParams attrs = getActivity().getWindow().getAttributes();
					attrs.flags |= WindowManager.LayoutParams.FLAG_FULLSCREEN;
					attrs.flags |= WindowManager.LayoutParams.FLAG_KEEP_SCREEN_ON;
					getActivity().getWindow().setAttributes(attrs);
					((AppCompatActivity) getActivity()).getSupportActionBar().hide();
					if (android.os.Build.VERSION.SDK_INT >= 14)
					{
						//noinspection all
						getActivity().getWindow().getDecorView().setSystemUiVisibility(View.SYSTEM_UI_FLAG_LOW_PROFILE);
					}
				}
				else
				{
					WindowManager.LayoutParams attrs = getActivity().getWindow().getAttributes();
					//only re-enable status bar if there is no contradicting preference set
					if (!PrefsUtility.pref_appearance_hide_android_status(getContext(),
							PreferenceManager.getDefaultSharedPreferences(getContext()))) {
						attrs.flags &= ~WindowManager.LayoutParams.FLAG_FULLSCREEN;
					}
					attrs.flags &= ~WindowManager.LayoutParams.FLAG_KEEP_SCREEN_ON;
					getActivity().getWindow().setAttributes(attrs);
					((AppCompatActivity) getActivity()).getSupportActionBar().show();
					if (android.os.Build.VERSION.SDK_INT >= 14)
					{
						//noinspection all
						getActivity().getWindow().getDecorView().setSystemUiVisibility(View.SYSTEM_UI_FLAG_VISIBLE);
					}
				}

			}
		});

		/*handle download links show an alert box to load this outside the internal browser*/
		webView.setDownloadListener(new DownloadListener() {
			@Override
			public void onDownloadStart(final String url, String userAgent, String contentDisposition, String mimetype, long contentLength) {
				{
					new AlertDialog.Builder(mActivity)
							.setTitle(R.string.download_link_title)
							.setMessage(R.string.download_link_message)
							.setPositiveButton(android.R.string.yes, new DialogInterface.OnClickListener() {
								public void onClick(DialogInterface dialog, int which) {
									Intent i = new Intent(Intent.ACTION_VIEW);
									i.setData(Uri.parse(url));
									getContext().startActivity(i);
									mActivity.onBackPressed(); //get back from internal browser
								}
							})
							.setNegativeButton(android.R.string.no, new DialogInterface.OnClickListener() {
								public void onClick(DialogInterface dialog, int which) {
									mActivity.onBackPressed(); //get back from internal browser
								}
							})
							.setIcon(android.R.drawable.ic_dialog_alert)
							.show();
				}
			}
		});
		/*handle download links end*/


		final WebSettings settings = webView.getSettings();

		settings.setBuiltInZoomControls(true);
		settings.setJavaScriptEnabled(true);
		settings.setJavaScriptCanOpenWindowsAutomatically(false);
		settings.setUseWideViewPort(true);
		settings.setLoadWithOverviewMode(true);
		settings.setDomStorageEnabled(true);
		settings.setDisplayZoomControls(false);

		// TODO handle long clicks

		webView.setWebChromeClient(chromeClient);


		if (mUrl != null) {
			webView.loadUrl(mUrl);
		} else {
			webView.loadDataWithBaseURL("https://saidit.net/", html, "text/html; charset=UTF-8", null, null);
		}

		webView.setWebViewClient(new WebViewClient() {
			@Override
			public boolean shouldOverrideUrlLoading(final WebView view, final String url) {

				if (url == null) return false;

				if (url.startsWith("data:")) {
					// Prevent imgur bug where we're directed to some random data URI
					return true;
				}

				// Go back if loading same page to prevent redirect loops.
				if (goingBack && currentUrl != null && url.equals(currentUrl)) {

					General.quickToast(mActivity,
							String.format(Locale.US, "Handling redirect loop (level %d)", -lastBackDepthAttempt), Toast.LENGTH_SHORT);

					lastBackDepthAttempt--;

					if (webView.canGoBackOrForward(lastBackDepthAttempt)) {
						webView.goBackOrForward(lastBackDepthAttempt);
					} else {
						mActivity.finish();
					}
				} else {

					if (RedditURLParser.parse(Uri.parse(url)) != null) {
						LinkHandler.onLinkClicked(mActivity, url, false);
					} else {
						webView.loadUrl(url);
						currentUrl = url;
					}
				}

				return true;
			}

			@Override
			public void onPageStarted(WebView view, String url, Bitmap favicon) {
				super.onPageStarted(view, url, favicon);

				if (mUrl != null && url != null) {

					final AppCompatActivity activity = mActivity;

					if (activity != null) {
						activity.setTitle(url);
					}
				}
			}

			@Override
			public void onPageFinished(final WebView view, final String url) {
				super.onPageFinished(view, url);

				new Timer().schedule(new TimerTask() {
					@Override
					public void run() {

						AndroidCommon.UI_THREAD_HANDLER.post(new Runnable() {
							@Override
							public void run() {

								if (currentUrl == null || url == null) return;

								if (!url.equals(view.getUrl())) return;

								if (goingBack && url.equals(currentUrl)) {

									General.quickToast(mActivity,
											String.format(Locale.US, "Handling redirect loop (level %d)", -lastBackDepthAttempt));

									lastBackDepthAttempt--;

									if (webView.canGoBackOrForward(lastBackDepthAttempt)) {
										webView.goBackOrForward(lastBackDepthAttempt);
									} else {
										mActivity.finish();
									}

								} else {
									goingBack = false;
								}
							}
						});
					}
				}, 1000);
			}

			@Override
			public void doUpdateVisitedHistory(WebView view, String url, boolean isReload) {
				super.doUpdateVisitedHistory(view, url, isReload);
			}
		});

		final FrameLayout outerFrame = new FrameLayout(mActivity);
		outerFrame.addView(outer);

		if (post != null) {

			final SideToolbarOverlay toolbarOverlay = new SideToolbarOverlay(mActivity);

			final BezelSwipeOverlay bezelOverlay = new BezelSwipeOverlay(mActivity, new BezelSwipeOverlay.BezelSwipeListener() {
				@Override
				public boolean onSwipe(@BezelSwipeOverlay.SwipeEdge int edge) {

					toolbarOverlay.setContents(post.generateToolbar(mActivity, false, toolbarOverlay));
					toolbarOverlay.show(edge == BezelSwipeOverlay.LEFT ?
							SideToolbarOverlay.SideToolbarPosition.LEFT : SideToolbarOverlay.SideToolbarPosition.RIGHT);
					return true;
				}

				@Override
				public boolean onTap() {

					if (toolbarOverlay.isShown()) {
						toolbarOverlay.hide();
						return true;
					}

					return false;
				}
			});

			outerFrame.addView(bezelOverlay);
			outerFrame.addView(toolbarOverlay);

			bezelOverlay.getLayoutParams().width = android.widget.FrameLayout.LayoutParams.MATCH_PARENT;
			bezelOverlay.getLayoutParams().height = android.widget.FrameLayout.LayoutParams.MATCH_PARENT;

			toolbarOverlay.getLayoutParams().width = android.widget.FrameLayout.LayoutParams.MATCH_PARENT;
			toolbarOverlay.getLayoutParams().height = android.widget.FrameLayout.LayoutParams.MATCH_PARENT;
		}

		return outerFrame;
	}

	@Override
	public void onDestroyView() {

		webView.stopLoading();
		webView.loadData("<html></html>", "text/plain", "UTF-8");
		webView.reload();
		webView.loadUrl("about:blank");
		outer.removeAllViews();
		webView.destroy();

		final CookieManager cookieManager = CookieManager.getInstance();
		cookieManager.removeAllCookie();

		super.onDestroyView();
	}

	public boolean onBackButtonPressed() {

		if (webView.canGoBack()) {
			goingBack = true;
			lastBackDepthAttempt = -1;
			webView.goBack();
			return true;
		}

		return false;
	}

	public void onPostSelected(final RedditPreparedPost post) {
		((RedditPostView.PostSelectionListener) mActivity).onPostSelected(post);
	}

	public void onPostCommentsSelected(final RedditPreparedPost post) {
		((RedditPostView.PostSelectionListener) mActivity).onPostCommentsSelected(post);
	}

	public String getCurrentUrl() {
		return (currentUrl != null) ? currentUrl : mUrl;
	}

	@Override
	public void onPause() {
		super.onPause();
		webView.onPause();
		webView.pauseTimers();
	}

	@Override
	public void onResume() {
		super.onResume();
		webView.resumeTimers();
		webView.onResume();
	}

	public void clearCache() {
		webView.clearBrowser();
	}
}
