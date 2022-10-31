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

import android.os.Bundle;
import android.view.MenuItem;
import android.widget.LinearLayout;
import android.widget.ScrollView;
import org.saiditnet.redreader.R;
import org.saiditnet.redreader.common.ChangelogManager;
import org.saiditnet.redreader.common.PrefsUtility;

public class ChangelogActivity extends BaseActivity {

	@Override
	protected boolean baseActivityIsToolbarActionBarEnabled() {
		return false;
	}

	@Override
	protected void onCreate(final Bundle savedInstanceState) {

		PrefsUtility.applySettingsTheme(this);

		super.onCreate(savedInstanceState);

		getSupportActionBarOrThrow().setTitle(R.string.title_changelog);
		getSupportActionBarOrThrow().setHomeButtonEnabled(true);
		getSupportActionBarOrThrow().setDisplayHomeAsUpEnabled(true);

		final LinearLayout items = new LinearLayout(this);
		items.setOrientation(LinearLayout.VERTICAL);

		ChangelogManager.generateViews(this, items, true);

		final ScrollView sv = new ScrollView(this);
		sv.addView(items);
		setBaseActivityContentView(sv);
	}

	@Override
	public boolean onOptionsItemSelected(final MenuItem item) {
		switch(item.getItemId()) {
			case android.R.id.home:
				finish();
				return true;
			default:
				return false;
		}
	}
}
