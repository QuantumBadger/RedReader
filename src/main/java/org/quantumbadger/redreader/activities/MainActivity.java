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

import android.os.Bundle;
import org.holoeverywhere.app.Activity;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.ui.frag.RRFragmentLayout;
import org.quantumbadger.redreader.ui.prefs.RRPrefs;

public class MainActivity extends Activity {

	private RRFragmentLayout layout;

	@Override
	protected void onCreate(final Bundle savedInstanceState) {

		try {
			RRPrefs.getPrefs(this);
		} catch(Exception e) {
			throw new RuntimeException(e);
		}

		layout = new RRFragmentLayout(this);

		setContentView(layout);
	}

	@Override
	public void onBackPressed() {

		if(!General.onBackPressed()) return;

		if(layout.getFragmentCount() > 1) {
			layout.removeTopFragment();

		} else {
			super.onBackPressed();
		}
	}
}
