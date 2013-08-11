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

import android.net.Uri;
import android.os.Bundle;
import org.holoeverywhere.app.Activity;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.settings.RRPrefs;
import org.quantumbadger.redreader.ui.frag.RRFragmentLayout;
import org.quantumbadger.redreader.ui.frag.RRSequentialUriHandler;
import org.quantumbadger.redreader.ui.frag.RRTestUriHandler;
import org.quantumbadger.redreader.ui.settings.PrefsUriHandler;

public class MainActivity extends Activity {

	private RRFragmentLayout layout;

	@Override
	protected void onCreate(final Bundle savedInstanceState) {

		super.onCreate(savedInstanceState);

		try {
			RRPrefs.getPrefs(this);
		} catch(Exception e) {
			throw new RuntimeException(e);
		}

		layout = new RRFragmentLayout(this);

		final RRSequentialUriHandler uriHandler = new RRSequentialUriHandler();
		uriHandler.addHandler(new PrefsUriHandler());
		uriHandler.addHandler(new RRTestUriHandler());
		layout.setUriHandler(uriHandler);

		setContentView(layout);

		//layout.handleUri(Constants.Internal.getUri(Constants.Internal.URI_HOST_PREFSPAGE));
		layout.handleUri(Uri.parse("rr://listtest"));
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

	@Override
	protected void onResume() {
		super.onResume();
		layout.onResume();
	}

	@Override
	protected void onPause() {
		super.onPause();
		layout.onPause();
	}
}
