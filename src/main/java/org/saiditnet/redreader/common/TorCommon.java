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

package org.saiditnet.redreader.common;

import android.app.AlertDialog;
import android.content.Context;
import android.content.DialogInterface;
import android.content.SharedPreferences;
import android.preference.PreferenceManager;
import info.guardianproject.netcipher.proxy.OrbotHelper;
import org.saiditnet.redreader.R;
import org.saiditnet.redreader.cache.CacheDownload;
import org.saiditnet.redreader.http.HTTPBackend;

import java.util.concurrent.atomic.AtomicBoolean;

public class TorCommon {

	private static final AtomicBoolean sIsTorEnabled = new AtomicBoolean(false);

	public static void promptToInstallOrbot(final Context context) {

		General.checkThisIsUIThread();

		AlertDialog.Builder notInstalled = new AlertDialog.Builder(context);

		notInstalled.setMessage(R.string.error_tor_not_installed);
		notInstalled.setPositiveButton(R.string.dialog_yes, new DialogInterface.OnClickListener() {
			@Override
			public void onClick(DialogInterface dialog, int id) {
				context.startActivity(OrbotHelper.getOrbotInstallIntent(context));
				dialog.dismiss();
			}
		});
		notInstalled.setNegativeButton(R.string.dialog_no, new DialogInterface.OnClickListener() {
			@Override
			public void onClick(DialogInterface dialog, int id) {
				dialog.cancel();
			}
		});
		AlertDialog notInstalledAlert = notInstalled.create();
		notInstalledAlert.show();
	}

	public static void updateTorStatus(final Context context) {

		General.checkThisIsUIThread();

		final SharedPreferences sharedPreferences = PreferenceManager.getDefaultSharedPreferences(context);

		final boolean torEnabled = PrefsUtility.network_tor(context, sharedPreferences);
		final boolean torChanged = (torEnabled != isTorEnabled());

		sIsTorEnabled.set(torEnabled);

		if(torEnabled) {
			verifyTorSetup(context);
		}

		if(torChanged) {
			HTTPBackend.getBackend().recreateHttpBackend();
			CacheDownload.resetUserCredentialsOnNextRequest();
		}
	}

	private static void verifyTorSetup(final Context context) {

		General.checkThisIsUIThread();

		if(!sIsTorEnabled.get()) {
			return;
		}

		if(!OrbotHelper.isOrbotInstalled(context)) {
			promptToInstallOrbot(context);
			return;
		}

		ensureTorIsRunning(context);
	}

	private static void ensureTorIsRunning(final Context context) {

		if(!OrbotHelper.isOrbotRunning(context)) {
			if(!OrbotHelper.requestStartTor(context)) {

				final AlertDialog.Builder builder = new AlertDialog.Builder(context);
				builder.setMessage(R.string.error_tor_start_failed);

				AlertDialog dialog = builder.create();
				dialog.show();
			}
		}
	}

	public static boolean isTorEnabled() {
		return sIsTorEnabled.get();
	}
}
