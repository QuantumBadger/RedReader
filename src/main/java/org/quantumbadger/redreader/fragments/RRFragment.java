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

package org.quantumbadger.redreader.fragments;

import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.FrameLayout;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;
import org.quantumbadger.redreader.activities.BaseActivity;
import org.quantumbadger.redreader.common.General;

public abstract class RRFragment {

	@NonNull private final AppCompatActivity mParent;

	protected RRFragment(
			@NonNull final AppCompatActivity parent,
			final Bundle savedInstanceState) {
		mParent = parent;
	}

	@NonNull
	protected final Context getContext() {
		return mParent;
	}

	@NonNull
	protected final AppCompatActivity getActivity() {
		return mParent;
	}

	protected final String getString(final int resource) {
		return mParent.getApplicationContext().getString(resource);
	}

	protected final void startActivity(final Intent intent) {
		mParent.startActivity(intent);
	}

	protected final void startActivityForResult(
			final Intent intent,
			final int requestCode) {
		mParent.startActivityForResult(intent, requestCode);
	}

	public void onCreateOptionsMenu(final Menu menu) {
	}

	public boolean onOptionsItemSelected(final MenuItem item) {
		return false;
	}

	public abstract View getListingView();

	@Nullable
	public View getOverlayView() {
		// Null by default
		return null;
	}

	@NonNull
	public final View createCombinedListingAndOverlayView() {

		final FrameLayout outer = new FrameLayout(mParent);

		{
			final View view = getListingView();
			outer.addView(view);
			General.setLayoutMatchParent(view);
		}

		{
			final View overlayView = getOverlayView();

			if(overlayView != null) {
				outer.addView(overlayView);
				General.setLayoutMatchParent(overlayView);
			}
		}

		return outer;
	}

	public final void setBaseActivityContent(@NonNull final BaseActivity baseActivity) {

		{
			final View view = getListingView();
			baseActivity.setBaseActivityListing(view);
			General.setLayoutMatchParent(view);
		}

		{
			final View overlayView = getOverlayView();

			if(overlayView != null) {
				baseActivity.setBaseActivityOverlay(overlayView);
				General.setLayoutMatchParent(overlayView);
			}
		}
	}

	public abstract Bundle onSaveInstanceState();
}
