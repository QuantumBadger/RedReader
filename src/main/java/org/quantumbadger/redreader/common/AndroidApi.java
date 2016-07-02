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
