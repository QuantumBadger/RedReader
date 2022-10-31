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

package org.saiditnet.redreader.activities;

import android.content.SharedPreferences;
import android.content.pm.ActivityInfo;
import android.content.pm.PackageManager;
import android.content.res.TypedArray;
import android.os.Build;
import android.os.Bundle;
import android.preference.PreferenceManager;
import android.support.annotation.LayoutRes;
import android.support.annotation.NonNull;
import android.support.v7.app.ActionBar;
import android.support.v7.app.AppCompatActivity;
import android.support.v7.widget.Toolbar;
import android.view.View;
import android.view.WindowManager;
import android.widget.FrameLayout;
import android.widget.ImageView;
import android.widget.TextView;
import org.saiditnet.redreader.R;
import org.saiditnet.redreader.common.General;
import org.saiditnet.redreader.common.PrefsUtility;
import org.saiditnet.redreader.common.TorCommon;

import java.util.HashMap;
import java.util.concurrent.atomic.AtomicInteger;

public abstract class BaseActivity extends AppCompatActivity implements SharedPreferences.OnSharedPreferenceChangeListener {

	private SharedPreferences mSharedPreferences;

	private static boolean closingAll = false;

	private final AtomicInteger mPermissionRequestIdGenerator = new AtomicInteger();
	private final HashMap<Integer, PermissionCallback> mPermissionRequestCallbacks = new HashMap<>();

	private TextView mActionbarTitleTextView;
	private FrameLayout mContentView;

	private ImageView mActionbarBackIconView;
	private View mActionbarTitleOuterView;

	protected boolean baseActivityIsToolbarActionBarEnabled() {
		return true;
	}

	protected boolean baseActivityIsActionBarBackEnabled() {
		return true;
	}

	@Override
	public void setTitle(final CharSequence text) {
		super.setTitle(text);

		if(mActionbarTitleTextView != null) {
			mActionbarTitleTextView.setText(text);
		}
	}

	@Override
	public void setTitle(final int res) {
		setTitle(getText(res));
	}

	public interface PermissionCallback {
		void onPermissionGranted();
		void onPermissionDenied();
	}

	public void closeAllExceptMain() {
		closingAll = true;
		closeIfNecessary();
	}

	// Avoids IDE warnings about null pointers
	@NonNull
	public final ActionBar getSupportActionBarOrThrow() {

		final ActionBar result = getSupportActionBar();

		if(result == null) {
			throw new RuntimeException("Action bar is null");
		}

		return result;
	}

	protected void configBackButton(boolean isVisible, View.OnClickListener listener) {
		if(isVisible) {
			mActionbarBackIconView.setVisibility(View.VISIBLE);
			mActionbarTitleOuterView.setOnClickListener(listener);
			mActionbarTitleOuterView.setClickable(true);
		} else {
			mActionbarBackIconView.setVisibility(View.GONE);
			mActionbarTitleOuterView.setClickable(false);
		}
	}

	@Override
	protected void onCreate(Bundle savedInstanceState) {

		super.onCreate(savedInstanceState);

		mSharedPreferences = PreferenceManager.getDefaultSharedPreferences(this);

		if(PrefsUtility.pref_appearance_hide_android_status(this, mSharedPreferences)) {
			getWindow().setFlags(
					WindowManager.LayoutParams.FLAG_FULLSCREEN,
					WindowManager.LayoutParams.FLAG_FULLSCREEN);
		}


		mSharedPreferences.registerOnSharedPreferenceChangeListener(this);
		setOrientationFromPrefs();
		closeIfNecessary();

		if(baseActivityIsToolbarActionBarEnabled()) {
			final View outerView = getLayoutInflater().inflate(R.layout.rr_actionbar, null);

			final Toolbar toolbar = (Toolbar) outerView.findViewById(R.id.rr_actionbar_toolbar);
			mContentView = (FrameLayout) outerView.findViewById(R.id.rr_actionbar_content);

			super.setContentView(outerView);
			setSupportActionBar(toolbar);

			getSupportActionBarOrThrow().setCustomView(R.layout.actionbar_title);
			getSupportActionBarOrThrow().setDisplayShowCustomEnabled(true);
			getSupportActionBarOrThrow().setDisplayShowTitleEnabled(false);
			toolbar.setContentInsetsAbsolute(0, 0);

			mActionbarTitleTextView = (TextView)toolbar.findViewById(R.id.actionbar_title_text);

			mActionbarBackIconView = (ImageView)toolbar.findViewById(R.id.actionbar_title_back_image);
			mActionbarTitleOuterView = toolbar.findViewById(R.id.actionbar_title_outer);

			if(getTitle() != null) {
				// Update custom action bar text
				setTitle(getTitle());
			}

			configBackButton(baseActivityIsActionBarBackEnabled(), new View.OnClickListener() {
				@Override
				public void onClick(final View v) {
					finish();
				}
			});

			if(Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP) {

				final PrefsUtility.AppearanceNavbarColour navbarColour = PrefsUtility.appearance_navbar_colour(
						this,
						mSharedPreferences);

				if(navbarColour != PrefsUtility.AppearanceNavbarColour.BLACK) {

					final int colour;
					{
						final TypedArray appearance = obtainStyledAttributes(new int[]{
								R.attr.colorPrimary,
								R.attr.colorPrimaryDark});

						if(navbarColour == PrefsUtility.AppearanceNavbarColour.PRIMARY) {
							colour = appearance.getColor(0, General.COLOR_INVALID);
						} else {
							colour = appearance.getColor(1, General.COLOR_INVALID);
						}

						appearance.recycle();
					}

					getWindow().setNavigationBarColor(colour);
				}
			}
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
		mSharedPreferences.unregisterOnSharedPreferenceChangeListener(this);
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
		PrefsUtility.ScreenOrientation orientation = PrefsUtility.pref_behaviour_screen_orientation(this,
				mSharedPreferences);
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
