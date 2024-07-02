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

import android.content.res.TypedArray;
import android.graphics.Color;
import android.os.Bundle;
import android.text.TextUtils;
import android.view.View;
import android.widget.FrameLayout;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.appcompat.app.ActionBar;
import androidx.appcompat.widget.Toolbar;

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

			super.setContentView(outerView);
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

			final PrefsUtility.AppearanceNavbarColour navbarColour
					= PrefsUtility.appearance_navbar_colour();

			if (navbarColour == PrefsUtility.AppearanceNavbarColour.BLACK) {
				getWindow().setNavigationBarColor(Color.BLACK);

			} else if (navbarColour == PrefsUtility.AppearanceNavbarColour.WHITE) {
				getWindow().setNavigationBarColor(Color.WHITE);

			} else {
				final int colour;
				{
					final TypedArray appearance = obtainStyledAttributes(new int[]{
							com.google.android.material.R.attr.colorPrimary,
							com.google.android.material.R.attr.colorPrimaryDark});

					if (navbarColour == PrefsUtility.AppearanceNavbarColour.PRIMARY) {
						colour = appearance.getColor(0, General.COLOR_INVALID);
					} else {
						colour = appearance.getColor(1, General.COLOR_INVALID);
					}

					appearance.recycle();
				}

				getWindow().setNavigationBarColor(colour);
			}

		} else {
			mContentListing = new FrameLayout(this);
			mContentOverlay = new FrameLayout(this);

			final FrameLayout outer = new FrameLayout(this);
			outer.addView(mContentListing);
			outer.addView(mContentOverlay);

			super.setContentView(outer);
		}
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
