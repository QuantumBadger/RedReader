package org.quantumbadger.redreader.common;

import android.os.Build;
import android.os.Handler;
import android.os.Looper;

public class AndroidApi {

	public static final Handler UI_THREAD_HANDLER = new Handler(Looper.getMainLooper());

	private static final int CURRENT_API_VERSION = android.os.Build.VERSION.SDK_INT;

	public static boolean isGreaterThanOrEqualTo(int apiVersion) {
		return CURRENT_API_VERSION >= apiVersion;
	}

	public static boolean isHoneyCombOrLater() {
		return isGreaterThanOrEqualTo(Build.VERSION_CODES.HONEYCOMB);
	}

	public static boolean isIceCreamSandwichOrLater() {
		return isGreaterThanOrEqualTo(Build.VERSION_CODES.ICE_CREAM_SANDWICH);
	}
}
