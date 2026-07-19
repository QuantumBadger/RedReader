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

import android.annotation.SuppressLint;
import android.content.res.TypedArray;
import android.graphics.Color;
import android.os.Bundle;
import android.text.TextUtils;
import android.view.Gravity;
import android.view.View;
import android.widget.FrameLayout;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.appcompat.app.ActionBar;
import androidx.appcompat.widget.Toolbar;
import androidx.core.graphics.ColorUtils;
import androidx.core.graphics.Insets;
import androidx.core.view.ViewCompat;
import androidx.core.view.WindowCompat;
import androidx.core.view.WindowInsetsCompat;

import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.Optional;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.common.SharedPrefsWrapper;

import java.util.Locale;

public abstract class ViewsBaseActivity extends BaseActivity {

	@NonNull
	private Optional<TextView> mActionbarTitleTextView = Optional.empty();

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

	// Avoids IDE warnings about null pointers
	@NonNull
	public final ActionBar getSupportActionBarOrThrow() {

		final ActionBar result = getSupportActionBar();

		if (result == null) {
			throw new RuntimeException("Action bar is null");
		}

		return result;
	}

	protected void configBackButton(final boolean isVisible, final View.OnClickListener listener) {
		mActionbarBackIconView.setImportantForAccessibility(
				View.IMPORTANT_FOR_ACCESSIBILITY_NO_HIDE_DESCENDANTS);

		mActionbarTitleTextView.apply(
				titleView -> titleView.setImportantForAccessibility(
						View.IMPORTANT_FOR_ACCESSIBILITY_NO_HIDE_DESCENDANTS));

		if (isVisible) {
			mActionbarBackIconView.setVisibility(View.VISIBLE);
			mActionbarTitleOuterView.setOnClickListener(listener);
			mActionbarTitleOuterView.setClickable(true);
			mActionbarTitleOuterView.setContentDescription(getString(R.string.action_back));
			mActionbarTitleOuterView.setImportantForAccessibility(
					View.IMPORTANT_FOR_ACCESSIBILITY_YES);

			if (TextUtils.getLayoutDirectionFromLocale(Locale.getDefault())
					== View.LAYOUT_DIRECTION_RTL) {

				mActionbarBackIconView.setImageResource(R.drawable.ic_action_forward_dark);
			}

		} else {
			mActionbarBackIconView.setVisibility(View.GONE);
			mActionbarTitleOuterView.setClickable(false);

			mActionbarTitleOuterView.setContentDescription(null);

			mActionbarTitleOuterView.setImportantForAccessibility(
					View.IMPORTANT_FOR_ACCESSIBILITY_NO_HIDE_DESCENDANTS);
		}
	}

	protected boolean baseActivityAllowToolbarHideOnScroll() {
		// Disallow by default
		return false;
	}

	@Override
	protected void onCreate(final Bundle savedInstanceState) {

		super.onCreate(savedInstanceState);

		if (baseActivityIsToolbarActionBarEnabled()) {

			final View outerView;

			final boolean isTablet = General.isTablet(this);

			final boolean prefBottomToolbar
					= PrefsUtility.pref_appearance_bottom_toolbar();

			final boolean prefHideOnScroll = PrefsUtility.pref_appearance_hide_toolbar_on_scroll();

			final int layoutRes;

			if (prefHideOnScroll && !isTablet) {

				if (baseActivityAllowToolbarHideOnScroll()) {
					layoutRes = R.layout.rr_actionbar_hide_on_scroll;
				} else {
					layoutRes = R.layout.rr_actionbar;
				}

			} else if (prefBottomToolbar) {
				layoutRes = R.layout.rr_actionbar_reverse;

			} else {
				layoutRes = R.layout.rr_actionbar;
			}

			outerView = getLayoutInflater().inflate(layoutRes, null);

			final Toolbar toolbar = outerView.findViewById(R.id.rr_actionbar_toolbar);
			mContentListing = outerView.findViewById(R.id.rr_actionbar_content_listing);
			mContentOverlay = outerView.findViewById(R.id.rr_actionbar_content_overlay);

			super.setContentView(wrapWithSystemBarScrims(outerView));
			setSupportActionBar(toolbar);

			final ActionBar supportActionBar = getSupportActionBarOrThrow();

			if (baseActivityIsToolbarSearchBarEnabled()) {
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

			if (baseActivityIsToolbarSearchBarEnabled()) {
				mActionbarTitleTextView = Optional.empty();
			} else {
				mActionbarTitleTextView = Optional.of(
						toolbar.findViewById(R.id.actionbar_title_text));
			}

			if (getTitle() != null) {
				// Update custom action bar text
				setTitle(getTitle());
			}

			configBackButton(
					baseActivityIsActionBarBackEnabled(),
					v -> onBackPressed());

		} else {
			mContentListing = new FrameLayout(this);
			mContentOverlay = new FrameLayout(this);

			final FrameLayout outer = new FrameLayout(this);
			outer.addView(mContentListing);
			outer.addView(mContentOverlay);

			super.setContentView(wrapWithSystemBarScrims(outer));
		}
	}

	/**
	 * The colour drawn behind the navigation bar, replicating the old
	 * window-level navigation bar colour.
	 */
	protected int baseActivityNavigationBarColour() {

		final PrefsUtility.AppearanceNavbarColour navbarColour
				= PrefsUtility.appearance_navbar_colour();

		if (navbarColour == PrefsUtility.AppearanceNavbarColour.BLACK) {
			return Color.BLACK;

		} else if (navbarColour == PrefsUtility.AppearanceNavbarColour.WHITE) {
			return Color.WHITE;
		}

		final int colour;
		{
			final TypedArray appearance = obtainStyledAttributes(new int[]{
					androidx.appcompat.R.attr.colorPrimary,
					androidx.appcompat.R.attr.colorPrimaryDark});

			if (navbarColour == PrefsUtility.AppearanceNavbarColour.PRIMARY) {
				colour = appearance.getColor(0, General.COLOR_INVALID);
			} else {
				colour = appearance.getColor(1, General.COLOR_INVALID);
			}

			appearance.recycle();
		}

		return colour;
	}

	private View makeScrim(final int gravity) {
		final View scrim = new View(this);
		scrim.setLayoutParams(new FrameLayout.LayoutParams(0, 0, gravity));
		return scrim;
	}

	private static void setScrimBounds(
			@NonNull final View scrim,
			final int width,
			final int height) {

		final FrameLayout.LayoutParams params
				= (FrameLayout.LayoutParams)scrim.getLayoutParams();
		params.width = width;
		params.height = height;
		scrim.setLayoutParams(params);
	}

	/**
	 * The window is laid out edge-to-edge, so the activity content is inset by
	 * the window insets here, and the system bar areas are painted to match
	 * the app's pre-edge-to-edge appearance: the status bar in the theme's
	 * colorPrimaryDark, and the navigation bar in the colour from
	 * baseActivityNavigationBarColour().
	 */
	// Window insets are physical coordinates, so the left/right scrims must
	// stay on their physical edges regardless of layout direction
	@SuppressLint("RtlHardcoded")
	@NonNull
	private View wrapWithSystemBarScrims(@NonNull final View content) {

		final int statusBarColour;
		{
			final TypedArray appearance = obtainStyledAttributes(new int[]{
					androidx.appcompat.R.attr.colorPrimaryDark});
			statusBarColour = appearance.getColor(0, General.COLOR_INVALID);
			appearance.recycle();
		}

		final int navBarColour = baseActivityNavigationBarColour();

		// Report the effective nav bar colour to the system, even though the
		// visible pixels come from the scrim view below. From SDK 35 this
		// deprecated call draws nothing, but an opaque colour here keeps
		// SystemUI out of "transparent bar" mode, in which it ignores
		// the light/dark icon appearance requested below.
		getWindow().setNavigationBarColor(navBarColour);

		WindowCompat.getInsetsController(getWindow(), getWindow().getDecorView())
				.setAppearanceLightNavigationBars(
						ColorUtils.calculateLuminance(navBarColour) > 0.5);

		final FrameLayout root = new FrameLayout(this);

		final View scrimLeft = makeScrim(Gravity.LEFT);
		final View scrimRight = makeScrim(Gravity.RIGHT);
		final View scrimTop = makeScrim(Gravity.TOP);
		final View scrimBottom = makeScrim(Gravity.BOTTOM);

		root.addView(content);

		// Side scrims first, so that the status/nav bar colours win in the
		// corners, as if the horizontal bars spanned the full screen width
		root.addView(scrimLeft);
		root.addView(scrimRight);
		root.addView(scrimTop);
		root.addView(scrimBottom);

		ViewCompat.setOnApplyWindowInsetsListener(root, (v, insets) -> {

			final Insets bars = insets.getInsets(
					WindowInsetsCompat.Type.systemBars()
							| WindowInsetsCompat.Type.displayCutout());

			final Insets navBars
					= insets.getInsets(WindowInsetsCompat.Type.navigationBars());

			final int imeBottom
					= insets.getInsets(WindowInsetsCompat.Type.ime()).bottom;

			final FrameLayout.LayoutParams contentParams
					= (FrameLayout.LayoutParams)content.getLayoutParams();
			contentParams.setMargins(
					bars.left,
					bars.top,
					bars.right,
					Math.max(bars.bottom, imeBottom));
			content.setLayoutParams(contentParams);

			// Areas which are pure display cutout (no bar drawn over them)
			// are painted black, matching the old letterboxing behaviour
			scrimTop.setBackgroundColor(
					insets.isVisible(WindowInsetsCompat.Type.statusBars())
							? statusBarColour
							: Color.BLACK);
			scrimBottom.setBackgroundColor(
					navBars.bottom > 0 ? navBarColour : Color.BLACK);
			scrimLeft.setBackgroundColor(
					navBars.left > 0 ? navBarColour : Color.BLACK);
			scrimRight.setBackgroundColor(
					navBars.right > 0 ? navBarColour : Color.BLACK);

			setScrimBounds(scrimTop, FrameLayout.LayoutParams.MATCH_PARENT, bars.top);
			setScrimBounds(scrimBottom, FrameLayout.LayoutParams.MATCH_PARENT, bars.bottom);
			setScrimBounds(scrimLeft, bars.left, FrameLayout.LayoutParams.MATCH_PARENT);
			setScrimBounds(scrimRight, bars.right, FrameLayout.LayoutParams.MATCH_PARENT);

			return WindowInsetsCompat.CONSUMED;
		});

		return root;
	}

	public void setBaseActivityListing(@NonNull final View view) {
		mContentListing.removeAllViews();
		mContentListing.addView(view);
	}

	public void clearBaseActivityListing() {
		mContentListing.removeAllViews();
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
	public final void onSharedPreferenceChanged(
			@NonNull final SharedPrefsWrapper prefs,
			@NonNull final String key) {

		super.onSharedPreferenceChanged(prefs, key);

		if (key.startsWith(getString(R.string.pref_menus_appbar_prefix))
				|| key.equals(getString(R.string.pref_menus_quick_account_switcher_key))
				|| key.equals(getString(R.string.pref_pinned_subreddits_key))) {
			invalidateOptionsMenu();
		}
	}
}
