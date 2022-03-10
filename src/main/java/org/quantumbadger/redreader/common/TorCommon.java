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

import org.quantumbadger.redreader.cache.CacheDownload;
import org.quantumbadger.redreader.http.HTTPBackend;

import java.util.concurrent.atomic.AtomicBoolean;

public class TorCommon {

	private static final AtomicBoolean sIsTorEnabled = new AtomicBoolean(false);

	public static void updateTorStatus() {

		General.checkThisIsUIThread();

		final boolean torEnabled = PrefsUtility.network_tor();
		final boolean torChanged = (torEnabled != isTorEnabled());

		sIsTorEnabled.set(torEnabled);

		if(torChanged) {
			HTTPBackend.getBackend().recreateHttpBackend();
			CacheDownload.resetUserCredentialsOnNextRequest();
		}
	}

	public static boolean isTorEnabled() {
		return sIsTorEnabled.get();
	}
}
