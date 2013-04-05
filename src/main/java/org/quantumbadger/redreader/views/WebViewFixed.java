package org.quantumbadger.redreader.views;

import android.content.Context;
import android.util.AttributeSet;
import android.util.Log;
import android.webkit.WebView;

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
	}

	public WebViewFixed(final Context context, final AttributeSet attrs, final int defStyle) {
		super(context, attrs, defStyle);
	}

	public WebViewFixed(final Context context, final AttributeSet attrs) {
		super(context, attrs);
	}

	@Override
	public void onWindowFocusChanged(final boolean hasWindowFocus) {
		try {
			super.onWindowFocusChanged(hasWindowFocus);
		} catch (NullPointerException ex) {
			Log.e("WebView", "WebView.onWindowFocusChanged", ex);
		}
	}
}