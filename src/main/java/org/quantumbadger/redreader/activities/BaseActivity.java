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
import android.content.res.TypedArray;
import android.graphics.Color;
import android.os.Build;
import android.os.Bundle;
import android.view.View;
import android.view.WindowManager;
import android.widget.FrameLayout;
import android.widget.ImageView;
import android.widget.TextView;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.ActionBar;
import androidx.appcompat.app.AppCompatActivity;
import androidx.appcompat.widget.Toolbar;
import androidx.core.text.TextUtilsCompat;
import androidx.core.view.ViewCompat;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.GlobalExceptionHandler;
import org.quantumbadger.redreader.common.Optional;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.common.SharedPrefsWrapper;
import org.quantumbadger.redreader.common.TorCommon;

import java.util.HashMap;
import java.util.Locale;
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

	@NonNull private Optional<TextView> mActionbarTitleTextView = Optional.empty();

	private FrameLayout mContentListing;
	private FrameLayout mContentOverlay;

	private ImageView mActionbarBackIconView;
	private View mActionbarTitleOuterView;

	protected boolean baseActivityIsToolbarActionBarEnabled() {
		return true;
	}

	protected boolean baseActivityIsToolbarSearchBarEnabled() {
		return false;
	}

	protected boolean baseActivityIsActionBarBackEnabled() {
		return true;
	}

	@Override
	public void setTitle(final CharSequence text) {
		super.setTitle(text);
		mActionbarTitleTextView.apply(titleView -> titleView.setText(text));
	}

	@Override
	public void setTitle(final int res) {
		setTitle(getText(res));
	}

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

	// Avoids IDE warnings about null pointers
	@NonNull
	public final ActionBar getSupportActionBarOrThrow() {

		final ActionBar result = getSupportActionBar();

		if(result == null) {
			throw new RuntimeException("Action bar is null");
		}

		return result;
	}

	protected void configBackButton(final boolean isVisible, final View.OnClickListener listener) {

		if(Build.VERSION.SDK_INT >= 19) {

			mActionbarBackIconView.setImportantForAccessibility(
					View.IMPORTANT_FOR_ACCESSIBILITY_NO_HIDE_DESCENDANTS);

			mActionbarTitleTextView.apply(
					titleView -> titleView.setImportantForAccessibility(
							View.IMPORTANT_FOR_ACCESSIBILITY_NO_HIDE_DESCENDANTS));
		}

		if(isVisible) {
			mActionbarBackIconView.setVisibility(View.VISIBLE);
			mActionbarTitleOuterView.setOnClickListener(listener);
			mActionbarTitleOuterView.setClickable(true);

			if(Build.VERSION.SDK_INT >= Build.VERSION_CODES.JELLY_BEAN) {
				mActionbarTitleOuterView.setContentDescription(getString(R.string.action_back));
				mActionbarTitleOuterView.setImportantForAccessibility(
						View.IMPORTANT_FOR_ACCESSIBILITY_YES);
			}

			if(TextUtilsCompat.getLayoutDirectionFromLocale(Locale.getDefault())
					== ViewCompat.LAYOUT_DIRECTION_RTL) {

				mActionbarBackIconView.setImageResource(R.drawable.ic_action_forward_dark);
			}

		} else {
			mActionbarBackIconView.setVisibility(View.GONE);
			mActionbarTitleOuterView.setClickable(false);

			mActionbarTitleOuterView.setContentDescription(null);

			if(Build.VERSION.SDK_INT >= 19) {
				mActionbarTitleOuterView.setImportantForAccessibility(
						View.IMPORTANT_FOR_ACCESSIBILITY_NO_HIDE_DESCENDANTS);
			}
		}
	}

	protected boolean baseActivityAllowToolbarHideOnScroll() {
		// Disallow by default
		return false;
	}

	@Override
	protected void onCreate(final Bundle savedInstanceState) {

		super.onCreate(savedInstanceState);

		mSharedPreferences = General.getSharedPrefs(this);

		if(PrefsUtility.pref_appearance_android_status()
				== PrefsUtility.AppearanceStatusBarMode.ALWAYS_HIDE) {
			getWindow().setFlags(
					WindowManager.LayoutParams.FLAG_FULLSCREEN,
					WindowManager.LayoutParams.FLAG_FULLSCREEN);
		}

		if(PrefsUtility.behaviour_block_screenshots()) {
			getWindow().addFlags(WindowManager.LayoutParams.FLAG_SECURE);
		}

		mSharedPreferences.registerOnSharedPreferenceChangeListener(this);
		setOrientationFromPrefs();
		closeIfNecessary();

		if(baseActivityIsToolbarActionBarEnabled()) {

			final View outerView;

			final boolean isTablet = General.isTablet(this);

			final boolean prefBottomToolbar
					= PrefsUtility.pref_appearance_bottom_toolbar();

			final boolean prefHideOnScroll = PrefsUtility.pref_appearance_hide_toolbar_on_scroll();

			final int layoutRes;

			if(prefHideOnScroll && !isTablet) {

				if(baseActivityAllowToolbarHideOnScroll()) {
					layoutRes = R.layout.rr_actionbar_hide_on_scroll;
				} else {
					layoutRes = R.layout.rr_actionbar;
				}

			} else if(prefBottomToolbar) {
				layoutRes = R.layout.rr_actionbar_reverse;

			} else {
				layoutRes = R.layout.rr_actionbar;
			}

			outerView = getLayoutInflater().inflate(layoutRes, null);

			final Toolbar toolbar = outerView.findViewById(R.id.rr_actionbar_toolbar);
			mContentListing = outerView.findViewById(R.id.rr_actionbar_content_listing);
			mContentOverlay = outerView.findViewById(R.id.rr_actionbar_content_overlay);

			super.setContentView(outerView);
			setSupportActionBar(toolbar);

			final ActionBar supportActionBar = getSupportActionBarOrThrow();

			if(baseActivityIsToolbarSearchBarEnabled()) {
				supportActionBar.setCustomView(R.layout.actionbar_search);
				General.setLayoutMatchParent(supportActionBar.getCustomView());

			} else {
				supportActionBar.setCustomView(R.layout.actionbar_title);
			}

			supportActionBar.setDisplayShowCustomEnabled(true);
			supportActionBar.setDisplayShowTitleEnabled(false);
			toolbar.setContentInsetsAbsolute(0, 0);

			mActionbarBackIconView = toolbar.findViewById(R.id.actionbar_title_back_image);
			mActionbarTitleOuterView = toolbar.findViewById(R.id.actionbar_title_outer);

			if(baseActivityIsToolbarSearchBarEnabled()) {
				mActionbarTitleTextView = Optional.empty();
			} else {
				mActionbarTitleTextView = Optional.of(
						toolbar.findViewById(R.id.actionbar_title_text));
			}

			if(getTitle() != null) {
				// Update custom action bar text
				setTitle(getTitle());
			}

			configBackButton(
					baseActivityIsActionBarBackEnabled(),
					v -> onBackPressed());

			if(Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP) {

				final PrefsUtility.AppearanceNavbarColour navbarColour
						= PrefsUtility.appearance_navbar_colour();

				if(navbarColour == PrefsUtility.AppearanceNavbarColour.BLACK) {
					getWindow().setNavigationBarColor(Color.BLACK);

				} else if(navbarColour == PrefsUtility.AppearanceNavbarColour.WHITE) {
					getWindow().setNavigationBarColor(Color.WHITE);

				} else {
					final int colour;
					{
						final TypedArray appearance = obtainStyledAttributes(new int[] {
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

		} else {
			mContentListing = new FrameLayout(this);
			mContentOverlay = new FrameLayout(this);

			final FrameLayout outer = new FrameLayout(this);
			outer.addView(mContentListing);
			outer.addView(mContentOverlay);

			super.setContentView(outer);
		}

		GlobalExceptionHandler.handleLastCrash(this);
	}

	public void setBaseActivityListing(@NonNull final View view) {
		mContentListing.removeAllViews();
		mContentListing.addView(view);
	}

	public void setBaseActivityListing(final int layoutRes) {
		mContentListing.removeAllViews();
		getLayoutInflater().inflate(layoutRes, mContentListing, true);
	}

	public void setBaseActivityOverlay(@NonNull final View view) {
		mContentOverlay.removeAllViews();
		mContentOverlay.addView(view);
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
		if(closingAll) {
			if(this instanceof MainActivity) {
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

		if(Build.VERSION.SDK_INT >= Build.VERSION_CODES.M) {

			if(checkSelfPermission(permission) == PackageManager.PERMISSION_GRANTED) {
				callback.onPermissionGranted();

			} else {
				final int requestCode = mRequestIdGenerator.incrementAndGet();
				mPermissionRequestCallbacks.put(requestCode, callback);
				requestPermissions(new String[] {permission}, requestCode);
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

		if(callback == null) {
			return;
		}

		if(permissions.length != 1) {
			throw new RuntimeException("Unexpected permission result");
		}

		if(grantResults[0] == PackageManager.PERMISSION_GRANTED) {
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

		if(callback == null) {
			return;
		}

		callback.onActivityResult(resultCode, data);
	}

	private void setOrientationFromPrefs() {
		final PrefsUtility.ScreenOrientation orientation
				= PrefsUtility.pref_behaviour_screen_orientation();

		if(orientation == PrefsUtility.ScreenOrientation.AUTO) {
			//noinspection SourceLockedOrientationActivity
			setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_UNSPECIFIED);

		} else if(orientation == PrefsUtility.ScreenOrientation.PORTRAIT) {
			//noinspection SourceLockedOrientationActivity
			setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_PORTRAIT);

		} else if(orientation == PrefsUtility.ScreenOrientation.LANDSCAPE) {
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
	public final void onSharedPreferenceChanged(
			@NonNull final SharedPrefsWrapper prefs,
			@NonNull final String key) {

		onSharedPreferenceChangedInner(prefs, key);

		if(key.startsWith(getString(R.string.pref_menus_appbar_prefix))
				|| key.equals(getString(R.string.pref_menus_quick_account_switcher_key))
				|| key.equals(getString(R.string.pref_pinned_subreddits_key))) {
			invalidateOptionsMenu();
		}

		if(key.equals(getString(R.string.pref_behaviour_screenorientation_key))) {
			setOrientationFromPrefs();
		}
	}
}
