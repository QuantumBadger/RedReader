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

import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.content.pm.PackageManager;
import android.graphics.Color;
import android.os.Build;
import android.os.Bundle;
import android.view.Window;
import android.view.WindowManager;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;
import androidx.core.view.WindowCompat;
import androidx.core.view.WindowInsetsCompat;
import androidx.core.view.WindowInsetsControllerCompat;

import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.GlobalExceptionHandler;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.common.SharedPrefsWrapper;
import org.quantumbadger.redreader.common.TorCommon;

import java.util.HashMap;
import java.util.concurrent.atomic.AtomicInteger;

public abstract class BaseActivity extends AppCompatActivity
		implements SharedPrefsWrapper.OnSharedPreferenceChangeListener {

	private SharedPrefsWrapper mSharedPreferences;

	private static boolean closingAll = false;

	private final AtomicInteger mRequestIdGenerator = new AtomicInteger(10_000);

	private final HashMap<Integer, PermissionCallback> mPermissionRequestCallbacks
			= new HashMap<>();

	private final HashMap<Integer, ActivityResultCallback> mActivityResultCallbacks
			= new HashMap<>();

	public interface PermissionCallback {
		void onPermissionGranted();

		void onPermissionDenied();
	}

	public interface ActivityResultCallback {
		void onActivityResult(int resultCode, @Nullable Intent data);
	}

	public void closeAllExceptMain() {
		closingAll = true;
		closeIfNecessary();
	}

	@Override
	protected void onCreate(final Bundle savedInstanceState) {

		super.onCreate(savedInstanceState);

		mSharedPreferences = General.getSharedPrefs(this);

		if (baseActivityConfiguresEdgeToEdge()) {
			applyEdgeToEdge();
		}

		if (PrefsUtility.pref_appearance_android_status()
				== PrefsUtility.AppearanceStatusBarMode.ALWAYS_HIDE) {
			hideStatusBar();
		}

		if (PrefsUtility.behaviour_block_screenshots()) {
			getWindow().addFlags(WindowManager.LayoutParams.FLAG_SECURE);
		}

		if (PrefsUtility.pref_behaviour_keep_screen_awake()) {
			getWindow().addFlags(WindowManager.LayoutParams.FLAG_KEEP_SCREEN_ON);
		}

		mSharedPreferences.registerOnSharedPreferenceChangeListener(this);
		setOrientationFromPrefs();
		closeIfNecessary();

		GlobalExceptionHandler.handleLastCrash(this);
	}

	/**
	 * Whether this class should configure the window for edge-to-edge display.
	 * Subclasses which call enableEdgeToEdge() themselves should return false.
	 */
	protected boolean baseActivityConfiguresEdgeToEdge() {
		return true;
	}

	/**
	 * Lays the window out edge-to-edge on all API levels, with transparent
	 * system bars and light bar icons (matching the app's pre-edge-to-edge
	 * appearance). Bar backgrounds are drawn by the app -- see
	 * ViewsBaseActivity.
	 */
	private void applyEdgeToEdge() {

		final Window window = getWindow();

		// The first getDecorView() call runs PhoneWindow.generateLayout(),
		// which reads the enforce*Contrast attributes back out of the theme.
		// The decor must therefore exist before the contrast flags below are
		// set, or they'd be silently reverted (leaving the system to draw a
		// theme-tinted scrim over the 3-button navigation bar).
		final WindowInsetsControllerCompat controller
				= WindowCompat.getInsetsController(window, window.getDecorView());

		WindowCompat.setDecorFitsSystemWindows(window, false);

		if (Build.VERSION.SDK_INT < 35) {
			// Deprecated and a no-op from SDK 35 onwards: the status bar is
			// always transparent once edge-to-edge is enforced
			window.setStatusBarColor(Color.TRANSPARENT);
		}

		// Deliberately called on every API level, even though it's deprecated
		// and draws nothing from SDK 35 onwards: it marks the nav bar colour
		// as app-specified, which stops DecorView deriving the nav button
		// appearance from the window background's luminance
		// (APPEARANCE_FORCE_LIGHT_NAVIGATION_BARS) and overriding the
		// appearance requested via the insets controller
		window.setNavigationBarColor(Color.TRANSPARENT);

		if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.Q) {
			// The app draws its own bar backgrounds, so the system shouldn't
			// add a contrast scrim of its own (e.g. for 3-button navigation)
			window.setStatusBarContrastEnforced(false);
			window.setNavigationBarContrastEnforced(false);
		}

		controller.setAppearanceLightStatusBars(false);
		controller.setAppearanceLightNavigationBars(false);
	}

	protected final void hideStatusBar() {

		final WindowInsetsControllerCompat controller = WindowCompat.getInsetsController(
				getWindow(),
				getWindow().getDecorView());

		controller.setSystemBarsBehavior(
				WindowInsetsControllerCompat.BEHAVIOR_SHOW_TRANSIENT_BARS_BY_SWIPE);
		controller.hide(WindowInsetsCompat.Type.statusBars());
	}

	protected final void showStatusBar() {
		WindowCompat.getInsetsController(getWindow(), getWindow().getDecorView())
				.show(WindowInsetsCompat.Type.statusBars());
	}

	@Override
	protected void onResume() {
		super.onResume();
		setOrientationFromPrefs();
		closeIfNecessary();
		TorCommon.updateTorStatus();
	}


	@Override
	protected void onDestroy() {
		super.onDestroy();
		mSharedPreferences.unregisterOnSharedPreferenceChangeListener(this);
	}

	private void closeIfNecessary() {
		if (closingAll) {
			if (this instanceof MainActivity) {
				closingAll = false;
			} else {
				finish();
			}
		}
	}

	public final void requestPermissionWithCallback(
			@NonNull final String permission,
			@NonNull final PermissionCallback callback) {

		General.checkThisIsUIThread();

		if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.M) {

			if (checkSelfPermission(permission) == PackageManager.PERMISSION_GRANTED) {
				callback.onPermissionGranted();

			} else {
				final int requestCode = mRequestIdGenerator.incrementAndGet();
				mPermissionRequestCallbacks.put(requestCode, callback);
				requestPermissions(new String[]{permission}, requestCode);
			}

		} else {
			callback.onPermissionGranted();
		}
	}

	@Override
	public final void onRequestPermissionsResult(
			final int requestCode,
			@NonNull final String[] permissions,
			@NonNull final int[] grantResults) {

		super.onRequestPermissionsResult(requestCode, permissions, grantResults);

		final PermissionCallback callback
				= mPermissionRequestCallbacks.remove(requestCode);

		if (callback == null) {
			return;
		}

		if (permissions.length != 1) {
			throw new RuntimeException("Unexpected permission result");
		}

		if (grantResults[0] == PackageManager.PERMISSION_GRANTED) {
			callback.onPermissionGranted();
		} else {
			callback.onPermissionDenied();
		}
	}

	public final void startActivityForResultWithCallback(
			@NonNull final Intent intent,
			@NonNull final ActivityResultCallback callback) {

		final int requestCode = mRequestIdGenerator.incrementAndGet();
		mActivityResultCallbacks.put(requestCode, callback);
		startActivityForResult(intent, requestCode);
	}

	@Override
	protected final void onActivityResult(
			final int requestCode,
			final int resultCode,
			@Nullable final Intent data) {

		super.onActivityResult(requestCode, resultCode, data);

		final ActivityResultCallback callback
				= mActivityResultCallbacks.remove(requestCode);

		if (callback == null) {
			return;
		}

		callback.onActivityResult(resultCode, data);
	}

	private void setOrientationFromPrefs() {
		final PrefsUtility.ScreenOrientation orientation
				= PrefsUtility.pref_behaviour_screen_orientation();

		if (orientation == PrefsUtility.ScreenOrientation.AUTO) {
			//noinspection SourceLockedOrientationActivity
			setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_UNSPECIFIED);

		} else if (orientation == PrefsUtility.ScreenOrientation.PORTRAIT) {
			//noinspection SourceLockedOrientationActivity
			setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_PORTRAIT);

		} else if (orientation == PrefsUtility.ScreenOrientation.LANDSCAPE) {
			//noinspection SourceLockedOrientationActivity
			setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_LANDSCAPE);
		}
	}


	protected void onSharedPreferenceChangedInner(
			final SharedPrefsWrapper prefs,
			final String key) {
		// Do nothing
	}

	@Override
	public void onSharedPreferenceChanged(
			@NonNull final SharedPrefsWrapper prefs,
			@NonNull final String key) {

		onSharedPreferenceChangedInner(prefs, key);

		if (key.equals(getString(R.string.pref_behaviour_screenorientation_key))) {
			setOrientationFromPrefs();
		}
	}
}
