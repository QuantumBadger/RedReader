package org.quantumbadger.redreader.common;

import android.app.AlertDialog;
import android.content.Context;
import android.content.DialogInterface;
import android.content.SharedPreferences;
import android.preference.PreferenceManager;
import info.guardianproject.netcipher.proxy.OrbotHelper;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.cache.CacheDownload;
import org.quantumbadger.redreader.http.okhttp.OKHTTPBackend;

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
			OKHTTPBackend.recreateHttpBackend();
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
