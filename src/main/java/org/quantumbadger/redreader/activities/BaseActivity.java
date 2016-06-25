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

import android.content.SharedPreferences;
import android.content.pm.ActivityInfo;
import android.content.pm.PackageManager;
import android.os.Build;
import android.os.Bundle;
import android.preference.PreferenceManager;
import android.support.annotation.LayoutRes;
import android.support.annotation.NonNull;
import android.support.v7.app.AppCompatActivity;
import android.support.v7.widget.Toolbar;
import android.view.View;
import android.widget.FrameLayout;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.common.TorCommon;

import java.util.HashMap;
import java.util.concurrent.atomic.AtomicInteger;

public class BaseActivity extends AppCompatActivity implements SharedPreferences.OnSharedPreferenceChangeListener {

	private SharedPreferences sharedPreferences;

	private static boolean closingAll = false;

	private final AtomicInteger mPermissionRequestIdGenerator = new AtomicInteger();
	private final HashMap<Integer, PermissionCallback> mPermissionRequestCallbacks = new HashMap<>();

	private FrameLayout mContentView;

	private boolean mToolbarActionBarEnabled = true;

	public interface PermissionCallback {
		void onPermissionGranted();
		void onPermissionDenied();
	}

	public void closeAllExceptMain() {
		closingAll = true;
		closeIfNecessary();
	}

	public void setToolbarActionBarEnabled(boolean toolbarActionBarEnabled) {
		mToolbarActionBarEnabled = toolbarActionBarEnabled;
	}

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		sharedPreferences = PreferenceManager.getDefaultSharedPreferences(this);
		sharedPreferences.registerOnSharedPreferenceChangeListener(this);
		setOrientationFromPrefs();
		closeIfNecessary();

		if(mToolbarActionBarEnabled) {
			final View outerView = getLayoutInflater().inflate(R.layout.rr_actionbar, null);

			final Toolbar toolbar = (Toolbar) outerView.findViewById(R.id.rr_actionbar_toolbar);
			mContentView = (FrameLayout) outerView.findViewById(R.id.rr_actionbar_content);

			super.setContentView(outerView);
			setSupportActionBar(toolbar);
		}
	}

	public void setBaseActivityContentView(@LayoutRes int layoutResID) {
		if(mContentView != null) {
			mContentView.removeAllViews();
			getLayoutInflater().inflate(layoutResID, mContentView, true);
		} else {
			super.setContentView(layoutResID);
		}
	}

	public void setBaseActivityContentView(@NonNull final View view) {
		if(mContentView != null) {
			mContentView.removeAllViews();
			mContentView.addView(view);
		} else {
			super.setContentView(view);
		}
	}

	@Override
	protected void onResume() {
		super.onResume();
		setOrientationFromPrefs();
		closeIfNecessary();
		TorCommon.updateTorStatus(this);
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



	protected void onSharedPreferenceChangedInner(final SharedPreferences prefs, final String key) {
		// Do nothing
	}

	@Override
	public final void onSharedPreferenceChanged(final SharedPreferences prefs, final String key) {

		onSharedPreferenceChangedInner(prefs, key);

		if(key.equals(getString(R.string.pref_menus_optionsmenu_items_key))) {
			invalidateOptionsMenu();
		}
	}
}
