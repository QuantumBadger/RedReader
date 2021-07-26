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

import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.view.View;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.fragments.WebViewFragment;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;

public class HtmlViewActivity extends BaseActivity {

	private WebViewFragment webView;

	public static void showAsset(
			final Context context,
			final String filename) {

		final String html;

		try(InputStream asset = context.getAssets().open(filename)) {

			final ByteArrayOutputStream baos = new ByteArrayOutputStream(16_384);

			final byte[] buf = new byte[8192];
			int bytesRead;

			while((bytesRead = asset.read(buf)) > 0) {
				baos.write(buf, 0, bytesRead);
			}

			html = baos.toString("UTF-8");

		} catch(final IOException e) {
			BugReportActivity.handleGlobalError(context, e);
			return;
		}

		final Intent intent = new Intent(context, HtmlViewActivity.class);
		intent.putExtra("html", html);
		context.startActivity(intent);
	}

	@Override
	public void onCreate(final Bundle savedInstanceState) {

		PrefsUtility.applyTheme(this);

		super.onCreate(savedInstanceState);

		final Intent intent = getIntent();

		final String html = intent.getStringExtra("html");
		final String title = intent.getStringExtra("title");
		setTitle(title);

		if(html == null) {
			BugReportActivity.handleGlobalError(this, "No HTML");
		}

		webView = WebViewFragment.newInstanceHtml(html);

		setBaseActivityListing(View.inflate(this, R.layout.main_single, null));

		getSupportFragmentManager().beginTransaction()
				.add(R.id.main_single_frame, webView)
				.commit();
	}

	@Override
	public void onBackPressed() {

		if(General.onBackPressed() && !webView.onBackButtonPressed()) {
			super.onBackPressed();
		}
	}
}
