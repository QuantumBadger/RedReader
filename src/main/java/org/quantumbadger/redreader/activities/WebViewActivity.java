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
import android.net.Uri;
import android.os.Bundle;
import android.view.View;
import android.widget.Toast;
import com.actionbarsherlock.view.Menu;
import com.actionbarsherlock.view.MenuItem;
import org.holoeverywhere.app.Activity;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.LinkHandler;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.fragments.WebViewFragment;
import org.quantumbadger.redreader.reddit.prepared.RedditPreparedPost;
import org.quantumbadger.redreader.reddit.things.RedditPost;
import org.quantumbadger.redreader.views.RedditPostView;

public class WebViewActivity extends Activity implements RedditPostView.PostSelectionListener {

	private WebViewFragment webView;
	public static final int VIEW_IN_BROWSER = 10,
			CLEAR_CACHE = 20;

	public void onCreate(final Bundle savedInstanceState) {

		PrefsUtility.applyTheme(this);

		getSupportActionBar().setHomeButtonEnabled(true);
		getSupportActionBar().setDisplayHomeAsUpEnabled(true);

		super.onCreate(savedInstanceState);

		final Intent intent = getIntent();

        String url = intent.getStringExtra("url");
		final RedditPost post = intent.getParcelableExtra("post");

		if(url == null) {
			BugReportActivity.handleGlobalError(this, "No URL");
		}

		webView = WebViewFragment.newInstance(url, post);

		setContentView(View.inflate(this, R.layout.main_single, null));

		getSupportFragmentManager().beginTransaction().add(R.id.main_single_frame, webView).commit();
	}

	@Override
	public void onBackPressed() {

		if(General.onBackPressed() && !webView.onBackButtonPressed())
			super.onBackPressed();
	}

	public void onPostSelected(final RedditPreparedPost post) {
		LinkHandler.onLinkClicked(this, post.url, false, post.src);
	}

	public void onPostCommentsSelected(final RedditPreparedPost post) {
		final Intent intent = new Intent(this, CommentListingActivity.class);
		intent.putExtra("postId", post.idAlone);
		startActivityForResult(intent, 1);
	}

	@Override
	public boolean onOptionsItemSelected(final MenuItem item) {

		switch(item.getItemId()) {

			case android.R.id.home:
				finish();
				return true;

			case VIEW_IN_BROWSER:
				if(webView.getCurrentUrl() != null) {
					try {
						final Intent intent = new Intent(Intent.ACTION_VIEW);
						intent.setData(Uri.parse(webView.getCurrentUrl()));
						startActivity(intent);
						finish(); //to clear from backstack

					} catch(Exception e) {
						Toast.makeText(this, "Error: could not launch browser.", Toast.LENGTH_LONG).show();
					}

				}
				return true;

			case CLEAR_CACHE:

				webView.clearCache();
				Toast.makeText(this, R.string.web_view_clear_cache_success_toast, Toast.LENGTH_LONG).show();
				return true;

			default:
				return super.onOptionsItemSelected(item);
		}
	}

	@Override
	public boolean onCreateOptionsMenu(Menu menu) {
		menu.add(0, VIEW_IN_BROWSER, 0, R.string.web_view_open_browser);
		menu.add(0, CLEAR_CACHE, 1, R.string.web_view_clear_cache);
		return super.onCreateOptionsMenu(menu);
	}
}