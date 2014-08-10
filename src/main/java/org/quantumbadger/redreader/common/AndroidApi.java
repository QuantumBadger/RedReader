package org.quantumbadger.redreader.common;

import android.os.Build;

public class AndroidApi {
	private static final int CURRENT_API_VERSION = android.os.Build.VERSION.SDK_INT;

	public static boolean isGreaterThan(int apiVersion) {
		return CURRENT_API_VERSION >= apiVersion;
	}

	public static boolean isHoneyCombOrLater() {
		return isGreaterThan(Build.VERSION_CODES.HONEYCOMB);
	}

	public static boolean isIceCreamSandwichOrLater() {
		return isGreaterThan(Build.VERSION_CODES.ICE_CREAM_SANDWICH);
	}
}
