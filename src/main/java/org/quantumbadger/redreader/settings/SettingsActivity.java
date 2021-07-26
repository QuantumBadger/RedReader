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

package org.quantumbadger.redreader.settings;

import android.graphics.Color;
import android.os.Build;
import android.os.Bundle;
import androidx.annotation.NonNull;
import androidx.fragment.app.FragmentTransaction;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.activities.BaseActivity;
import org.quantumbadger.redreader.common.PrefsUtility;

public class SettingsActivity extends BaseActivity {

	private void launchFragment(@NonNull final String panel) {

		final Bundle bundle = new Bundle();
		bundle.putString("panel", panel);

		getSupportFragmentManager()
				.beginTransaction()
				.setReorderingAllowed(false)
				.setTransition(FragmentTransaction.TRANSIT_FRAGMENT_OPEN)
				.replace(R.id.single_fragment_container, SettingsFragment.class, bundle)
				.addToBackStack("Settings: " + panel)
				.commit();
	}

	@Override
	protected void onCreate(final Bundle savedInstanceState) {

		PrefsUtility.applySettingsTheme(this);

		super.onCreate(savedInstanceState);

		if(Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP) {
			getWindow().setNavigationBarColor(Color.rgb(0x55, 0x55, 0x55));
		}

		setBaseActivityListing(R.layout.single_fragment_layout);

		final Bundle bundle = new Bundle();
		bundle.putString("panel", "root");

		getSupportFragmentManager()
				.beginTransaction()
				.setReorderingAllowed(false)
				.replace(R.id.single_fragment_container, SettingsFragment.class, bundle)
				.commit();
	}

	public void onPanelSelected(@NonNull final String panel) {
		launchFragment(panel);
	}
}
