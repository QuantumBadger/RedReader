package org.quantumbadger.redreader.views;

import android.content.Context;
import android.os.Build;
import android.util.AttributeSet;
import android.util.Log;
import android.webkit.CookieManager;
import android.webkit.ValueCallback;
import android.webkit.WebView;

import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.RedReader;
import org.quantumbadger.redreader.activities.BaseActivity;
import org.quantumbadger.redreader.activities.BugReportActivity;

import info.guardianproject.netcipher.web.WebkitProxy;

/**
 * Fixes the onWindowFocusChanged bug, by catching NullPointerException.
 * https://groups.google.com/d/topic/android-developers/ktbwY2gtLKQ/discussion
 * @author Andrew
 *
 */

// Taken from reddit-is-fun:
// https://github.com/talklittle/reddit-is-fun/blob/master/src/com/andrewshu/android/reddit/browser/WebViewFixed.java

public class WebViewFixed extends WebView {

	public WebViewFixed(final Context context) {
		super(context);
		setTor(context);
	}

	public WebViewFixed(final Context context, final AttributeSet attrs, final int defStyle) {
		super(context, attrs, defStyle);
		setTor(context);
	}

	public WebViewFixed(final Context context, final AttributeSet attrs) {
		super(context, attrs);
		setTor(context);
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
		if(BaseActivity.getTorStatus()) {
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
