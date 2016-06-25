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
import android.os.Bundle;
import android.view.MenuItem;
import android.view.View;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.fragments.WebViewFragment;

public class HtmlViewActivity extends BaseActivity {

	private WebViewFragment webView;

	public void onCreate(final Bundle savedInstanceState) {

		PrefsUtility.applyTheme(this);

		super.onCreate(savedInstanceState);

		getSupportActionBar().setHomeButtonEnabled(true);
		getSupportActionBar().setDisplayHomeAsUpEnabled(true);

		final Intent intent = getIntent();

		final String html = intent.getStringExtra("html");
		final String title = intent.getStringExtra("title");
		setTitle(title);

		if(html == null) {
			BugReportActivity.handleGlobalError(this, "No HTML");
		}

		webView = WebViewFragment.newInstanceHtml(html);

		setBaseActivityContentView(View.inflate(this, R.layout.main_single, null));

		getSupportFragmentManager().beginTransaction().add(R.id.main_single_frame, webView).commit();
	}

	@Override
	public void onBackPressed() {

		if(General.onBackPressed() && !webView.onBackButtonPressed())
			super.onBackPressed();
	}

	@Override
	public boolean onOptionsItemSelected(final MenuItem item) {

		switch(item.getItemId()) {

			case android.R.id.home:
				finish();
				return true;

			default:
				return super.onOptionsItemSelected(item);
		}
	}

}
