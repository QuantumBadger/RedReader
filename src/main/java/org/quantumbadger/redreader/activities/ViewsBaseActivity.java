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
import android.os.Build;
import android.os.Bundle;
import android.text.TextUtils;
import android.view.Gravity;
import android.view.View;
import android.view.ViewGroup;
import android.widget.FrameLayout;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.ActionBar;
import androidx.appcompat.widget.Toolbar;
import androidx.core.graphics.ColorUtils;
import androidx.core.graphics.Insets;
import androidx.core.view.ViewCompat;
import androidx.core.view.WindowCompat;
import androidx.core.view.WindowInsetsCompat;
import androidx.core.view.WindowInsetsControllerCompat;

import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.Optional;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.common.SharedPrefsWrapper;

import java.util.Locale;

public abstract class ViewsBaseActivity extends BaseActivity {

	// Default alpha of the scrim drawn behind the 3-button navigation bar,
	// used when the navigation bar opacity preference is "Automatic", matching
	// the light and dark scrims used by the AndroidX enableEdgeToEdge() default
	private static final int NAV_BAR_SCRIM_ALPHA_LIGHT = 0xE6;
	private static final int NAV_BAR_SCRIM_ALPHA_DARK = 0x80;

	@NonNull
	private Optional<TextView> mActionbarTitleTextView = Optional.empty();

	private FrameLayout mContentListing;
	private FrameLayout mContentOverlay;

	// The view which paints the system bar areas (see wrapWithSystemBarScrims)
	@Nullable private View mSystemBarScrimRoot;

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

	/**
	 * Whether the activity's content is laid out behind the navigation bar.
	 * If true, the bottom navigation bar inset is passed down to the content
	 * rather than being applied as a margin on the content as a whole, and
	 * the activity is responsible for keeping its bottom-most views clear of
	 * the bar (see General.applySystemBarPadding()). Activities
	 * whose bottom-most view scrolls (e.g. a listing) should return true, so
	 * that the listing scrolls behind the bar.
	 *
	 * Ignored when the toolbar is at the bottom of the screen, in which case
	 * the toolbar takes the inset instead.
	 */
	protected boolean baseActivityContentExtendsBehindNavigationBar() {
		return false;
	}

	/**
	 * Whether the activity's content is laid out behind the status bar. If
	 * true, the status bar is drawn over a translucent black scrim rather
	 * than an opaque one, the status bar's top inset (including any display
	 * cutout) is passed down to the content, and the activity is responsible
	 * for keeping its top-most views clear of it (see
	 * General.applySystemBarPadding()). Intended for media viewers.
	 */
	protected boolean baseActivityContentExtendsBehindStatusBar() {
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

			super.setContentView(wrapWithSystemBarScrims(
					outerView,
					layoutRes == R.layout.rr_actionbar_reverse ? toolbar : null));
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
					v -> getOnBackPressedDispatcher().onBackPressed());

		} else {
			mContentListing = new FrameLayout(this);
			mContentOverlay = new FrameLayout(this);

			final FrameLayout outer = new FrameLayout(this);
			outer.addView(mContentListing);
			outer.addView(mContentOverlay);

			super.setContentView(wrapWithSystemBarScrims(outer, null));
		}
	}

	/**
	 * The tint of the translucent scrim drawn behind the 3-button navigation
	 * bar (and the opaque colour behind a navigation bar at the side of the
	 * screen), replicating the old window-level navigation bar colour. Not
	 * used with gesture navigation, where nothing is drawn behind the handle.
	 * The scrim's opacity comes from navigationBarScrimColour().
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

	/**
	 * The colour of the scrim drawn behind the 3-button navigation bar: the
	 * navigation bar colour at the opacity chosen in the preferences, or at a
	 * default opacity which depends on how light the colour is.
	 */
	private static int navigationBarScrimColour(final int navBarColour) {

		final Optional<Integer> opacityPercent = PrefsUtility.appearance_navbar_opacity();

		final int alpha;

		if (opacityPercent.isPresent()) {
			alpha = Math.round(opacityPercent.get() * 255f / 100f);

		} else if (ColorUtils.calculateLuminance(navBarColour) > 0.5) {
			alpha = NAV_BAR_SCRIM_ALPHA_LIGHT;

		} else {
			alpha = NAV_BAR_SCRIM_ALPHA_DARK;
		}

		return ColorUtils.setAlphaComponent(navBarColour, alpha);
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
	 * the window insets here, and the system bar areas are painted: the status
	 * bar in the theme's colorPrimaryDark (or a translucent black scrim if
	 * baseActivityContentExtendsBehindStatusBar() is true), and a 3-button
	 * navigation bar with a translucent scrim tinted with
	 * baseActivityNavigationBarColour(), which the content shows through.
	 * Nothing is drawn behind the gesture navigation handle, which floats
	 * over the content.
	 *
	 * The scrim colours are read from the preferences each time the insets
	 * are applied, so that a change to the navigation bar opacity takes
	 * effect without restarting the activity (see onSharedPreferenceChanged).
	 *
	 * The bottom navigation bar inset is taken by whichever view touches the
	 * bottom of the screen: the toolbar if it's at the bottom, otherwise the
	 * content if baseActivityContentExtendsBehindNavigationBar() is true,
	 * otherwise the content as a whole is inset here. Likewise the status
	 * bar's top inset is passed to the content if
	 * baseActivityContentExtendsBehindStatusBar() is true.
	 */
	// Window insets are physical coordinates, so the left/right scrims must
	// stay on their physical edges regardless of layout direction
	@SuppressLint("RtlHardcoded")
	@NonNull
	private View wrapWithSystemBarScrims(
			@NonNull final View content,
			@Nullable final Toolbar bottomToolbar) {

		final int statusBarColour;
		final boolean isLightTheme;
		final int windowBackgroundColour;
		{
			final TypedArray appearance = obtainStyledAttributes(new int[]{
					androidx.appcompat.R.attr.colorPrimaryDark,
					androidx.appcompat.R.attr.isLightTheme,
					android.R.attr.colorBackground});
			statusBarColour = appearance.getColor(0, General.COLOR_INVALID);
			isLightTheme = appearance.getBoolean(1, false);
			windowBackgroundColour = appearance.getColor(
					2,
					isLightTheme ? Color.WHITE : Color.BLACK);
			appearance.recycle();
		}

		final int navBarColour = baseActivityNavigationBarColour();
		final boolean navBarColourIsLight
				= ColorUtils.calculateLuminance(navBarColour) > 0.5;

		// Drawn behind the status bar when the content extends behind it
		final int statusBarScrimColour
				= ColorUtils.setAlphaComponent(Color.BLACK, NAV_BAR_SCRIM_ALPHA_DARK);

		if (Build.VERSION.SDK_INT >= 35) {
			// This deprecated call draws nothing from SDK 35 onwards, but
			// reporting an opaque colour keeps SystemUI out of "transparent
			// bar" mode, in which it ignores the light/dark icon appearance
			// requested below. On older versions the system would paint this
			// colour over the bar, so there the colour stays transparent (as
			// set in BaseActivity) and the app's scrim shows instead.
			getWindow().setNavigationBarColor(navBarColour);
		}

		final WindowInsetsControllerCompat insetsController
				= WindowCompat.getInsetsController(getWindow(), getWindow().getDecorView());

		// Until the insets arrive, assume the bar floats over the content
		insetsController.setAppearanceLightNavigationBars(isLightTheme);

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

		mSystemBarScrimRoot = root;

		final boolean contentExtendsBehindNavBar
				= baseActivityContentExtendsBehindNavigationBar();

		final boolean contentExtendsBehindStatusBar
				= baseActivityContentExtendsBehindStatusBar();

		final int bottomToolbarBaseHeight = bottomToolbar != null
				? bottomToolbar.getLayoutParams().height
				: 0;

		ViewCompat.setOnApplyWindowInsetsListener(root, (v, insets) -> {

			final Insets bars = insets.getInsets(
					WindowInsetsCompat.Type.systemBars()
							| WindowInsetsCompat.Type.displayCutout());

			final Insets navBars
					= insets.getInsets(WindowInsetsCompat.Type.navigationBars());

			final Insets cutout
					= insets.getInsets(WindowInsetsCompat.Type.displayCutout());

			final int imeBottom
					= insets.getInsets(WindowInsetsCompat.Type.ime()).bottom;

			// A 3-button navigation bar is tappable along its whole height,
			// whereas the gesture navigation handle has no tappable area
			final int tappableBottom
					= insets.getInsets(WindowInsetsCompat.Type.tappableElement()).bottom;

			final boolean navBarInsetTakenBelow
					= bottomToolbar != null || contentExtendsBehindNavBar;

			final FrameLayout.LayoutParams contentParams
					= (FrameLayout.LayoutParams)content.getLayoutParams();
			contentParams.setMargins(
					bars.left,
					contentExtendsBehindStatusBar ? 0 : bars.top,
					bars.right,
					Math.max(bars.bottom, imeBottom)
							- (navBarInsetTakenBelow ? navBars.bottom : 0));
			content.setLayoutParams(contentParams);

			if (bottomToolbar != null) {
				// The toolbar's background extends behind the bar, with its
				// buttons kept above it
				final ViewGroup.LayoutParams toolbarParams
						= bottomToolbar.getLayoutParams();
				toolbarParams.height = bottomToolbarBaseHeight + navBars.bottom;
				bottomToolbar.setLayoutParams(toolbarParams);
				bottomToolbar.setPadding(
						bottomToolbar.getPaddingLeft(),
						bottomToolbar.getPaddingTop(),
						bottomToolbar.getPaddingRight(),
						navBars.bottom);
			}

			final boolean statusBarVisible
					= insets.isVisible(WindowInsetsCompat.Type.statusBars());

			final int scrimTopHeight;

			if (contentExtendsBehindStatusBar) {
				// The content shows through the scrim, and extends under any
				// display cutout when the status bar is hidden
				scrimTop.setBackgroundColor(statusBarScrimColour);
				scrimTopHeight = statusBarVisible ? bars.top : 0;

			} else {
				// Areas which are pure display cutout (no bar drawn over
				// them) are painted black, matching the old letterboxing
				// behaviour
				scrimTop.setBackgroundColor(
						statusBarVisible ? statusBarColour : Color.BLACK);
				scrimTopHeight = bars.top;
			}

			scrimLeft.setBackgroundColor(
					navBars.left > 0 ? navBarColour : Color.BLACK);
			scrimRight.setBackgroundColor(
					navBars.right > 0 ? navBarColour : Color.BLACK);

			final int navBarScrimColour = navigationBarScrimColour(navBarColour);

			// The scrim is translucent, so the bar's icons must contrast with
			// its appearance over the window background, not the tint alone
			final boolean navBarScrimIsLight = ColorUtils.calculateLuminance(
					ColorUtils.compositeColors(navBarScrimColour, windowBackgroundColour))
					> 0.5;

			final int scrimBottomHeight;

			if (tappableBottom > 0) {
				scrimBottom.setBackgroundColor(navBarScrimColour);
				scrimBottomHeight = tappableBottom;
			} else {
				scrimBottom.setBackgroundColor(Color.BLACK);
				scrimBottomHeight = cutout.bottom;
			}

			setScrimBounds(scrimTop, FrameLayout.LayoutParams.MATCH_PARENT, scrimTopHeight);
			setScrimBounds(scrimBottom, FrameLayout.LayoutParams.MATCH_PARENT, scrimBottomHeight);
			setScrimBounds(scrimLeft, bars.left, FrameLayout.LayoutParams.MATCH_PARENT);
			setScrimBounds(scrimRight, bars.right, FrameLayout.LayoutParams.MATCH_PARENT);

			// Where a bar is drawn over the app's scrim, the bar's icons need
			// to contrast with the scrim. The gesture handle floats over the
			// content, so it needs to contrast with the theme instead.
			final boolean navBarIconsOverLightBackground;

			if (tappableBottom > 0) {
				navBarIconsOverLightBackground = navBarScrimIsLight;
			} else if (navBars.left > 0 || navBars.right > 0) {
				navBarIconsOverLightBackground = navBarColourIsLight;
			} else {
				navBarIconsOverLightBackground = isLightTheme;
			}

			insetsController.setAppearanceLightNavigationBars(
					navBarIconsOverLightBackground);

			final boolean passNavBarInsetDown
					= contentExtendsBehindNavBar && bottomToolbar == null;

			if (passNavBarInsetDown || contentExtendsBehindStatusBar) {
				// Only the insets the content extends behind are passed down,
				// everything else having been handled here. The status bar's
				// inset includes any display cutout, so that the content can
				// keep clear of it even when the status bar is hidden.
				return new WindowInsetsCompat.Builder(insets)
						.setInsets(
								WindowInsetsCompat.Type.statusBars(),
								contentExtendsBehindStatusBar
										? Insets.of(0, bars.top, 0, 0)
										: Insets.NONE)
						.setInsets(
								WindowInsetsCompat.Type.navigationBars(),
								passNavBarInsetDown
										? Insets.of(0, 0, 0, navBars.bottom)
										: Insets.NONE)
						.setInsets(WindowInsetsCompat.Type.displayCutout(), Insets.NONE)
						.setInsets(WindowInsetsCompat.Type.ime(), Insets.NONE)
						.build();
			}

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

		if (key.equals(getString(R.string.pref_appearance_navbar_opacity_key))
				&& mSystemBarScrimRoot != null) {
			// Re-run the insets listener, which repaints the scrims
			ViewCompat.requestApplyInsets(mSystemBarScrimRoot);
		}
	}
}
