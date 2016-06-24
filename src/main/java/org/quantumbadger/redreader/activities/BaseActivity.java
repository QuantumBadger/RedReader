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

package org.quantumbadger.redreader.activities;

import android.app.AlertDialog;
import android.content.DialogInterface;
import android.content.SharedPreferences;
import android.content.pm.ActivityInfo;
import android.content.pm.PackageManager;
import android.os.Build;
import android.os.Bundle;
import android.preference.PreferenceManager;
import android.support.annotation.NonNull;
import android.support.v7.app.AppCompatActivity;
import info.guardianproject.netcipher.proxy.OrbotHelper;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.cache.CacheDownload;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.http.okhttp.OKHTTPBackend;

import java.util.HashMap;
import java.util.concurrent.atomic.AtomicInteger;

public class BaseActivity extends AppCompatActivity implements SharedPreferences.OnSharedPreferenceChangeListener {

	private SharedPreferences sharedPreferences;
	private static boolean tor;

	private static boolean closingAll = false;

	private final AtomicInteger mPermissionRequestIdGenerator = new AtomicInteger();
	private final HashMap<Integer, PermissionCallback> mPermissionRequestCallbacks = new HashMap<>();

	public interface PermissionCallback {
		void onPermissionGranted();
		void onPermissionDenied();
	}

	public void closeAllExceptMain() {
		closingAll = true;
		closeIfNecessary();
	}

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		sharedPreferences = PreferenceManager.getDefaultSharedPreferences(this);
		sharedPreferences.registerOnSharedPreferenceChangeListener(this);
		setOrientationFromPrefs();
		setTorFromPrefs();
		closeIfNecessary();
	}

	@Override
	protected void onResume() {
		super.onResume();
		setOrientationFromPrefs();
		setTorFromPrefs();
		closeIfNecessary();
	}


	@Override
	protected void onDestroy() {
		super.onDestroy();
		sharedPreferences.unregisterOnSharedPreferenceChangeListener(this);
	}

	private void closeIfNecessary() {
		if(closingAll) {
			if(this instanceof MainActivity) {
				closingAll = false;
			} else {
				finish();
			}
		}
	}

	public void requestPermissionWithCallback(
				@NonNull final String permission,
				@NonNull final PermissionCallback callback) {

		General.checkThisIsUIThread();

		if(Build.VERSION.SDK_INT >= Build.VERSION_CODES.M) {

			if(checkSelfPermission(permission) == PackageManager.PERMISSION_GRANTED) {
				callback.onPermissionGranted();

			} else {
				final int requestCode = mPermissionRequestIdGenerator.incrementAndGet();
				mPermissionRequestCallbacks.put(requestCode, callback);
				requestPermissions(new String[]{permission}, requestCode);
			}

		} else {
			callback.onPermissionGranted();
		}
	}

	@Override
	public void onRequestPermissionsResult(
			final int requestCode,
			@NonNull final String[] permissions,
			@NonNull final int[] grantResults) {

		final PermissionCallback callback = mPermissionRequestCallbacks.remove(requestCode);

		if(callback != null) {
			if(permissions.length != 1) {
				throw new RuntimeException("Unexpected permission result");
			}

			if(grantResults[0] == PackageManager.PERMISSION_GRANTED) {
				callback.onPermissionGranted();
			} else {
				callback.onPermissionDenied();
			}
		}
	}

	private void setOrientationFromPrefs() {
		PrefsUtility.ScreenOrientation orientation = PrefsUtility.pref_behaviour_screen_orientation(this, sharedPreferences);
		if (orientation == PrefsUtility.ScreenOrientation.AUTO)
			setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_UNSPECIFIED);
		else if (orientation == PrefsUtility.ScreenOrientation.PORTRAIT)
			setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_PORTRAIT);
		else if (orientation == PrefsUtility.ScreenOrientation.LANDSCAPE)
			setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_LANDSCAPE);
	}

	private void setTorFromPrefs() {
		tor = PrefsUtility.network_tor(this, sharedPreferences);
		if (tor) {
			if (OrbotHelper.isOrbotInstalled(this)){
				if(!OrbotHelper.isOrbotRunning(this)) {
					OrbotHelper.requestStartTor(this);
				}
			} else {
				AlertDialog.Builder notInstalled = new AlertDialog.Builder(this);
				notInstalled.setMessage(R.string.error_tor_not_installed);
				notInstalled.setPositiveButton(R.string.dialog_yes, new DialogInterface.OnClickListener() {
					@Override
					public void onClick(DialogInterface dialog, int id) {
						startActivity(OrbotHelper.getOrbotInstallIntent(getApplicationContext()));
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
		}
		OKHTTPBackend.recreateHttpBackend();
		CacheDownload.resetUserCredentialsOnNextRequest();
	}

	protected void onSharedPreferenceChangedInner(final SharedPreferences prefs, final String key) {
		// Do nothing
	}

	@Override
	public final void onSharedPreferenceChanged(final SharedPreferences prefs, final String key) {

		onSharedPreferenceChangedInner(prefs, key);

		if(key.equals(getString(R.string.pref_network_https_key))) {
			PrefsUtility.network_https(this, prefs);

		} else if(key.equals(getString(R.string.pref_menus_optionsmenu_items_key))) {
			invalidateOptionsMenu();
		} else if(key.equals(R.string.pref_network_tor_key)) {
			setTorFromPrefs();
		}
	}

	public static boolean getTorStatus() {
		return tor;
	}
}
