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
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.Toast;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.LinkHandler;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.fragments.WebViewFragment;
import org.quantumbadger.redreader.reddit.prepared.RedditPreparedPost;
import org.quantumbadger.redreader.reddit.things.RedditPost;
import org.quantumbadger.redreader.reddit.url.PostCommentListingURL;
import org.quantumbadger.redreader.views.RedditPostView;

public class WebViewActivity extends BaseActivity
		implements RedditPostView.PostSelectionListener {

	private WebViewFragment webView;
	public static final int VIEW_IN_BROWSER = 10;
	public static final int CLEAR_CACHE = 20;
	public static final int USE_HTTPS = 30;
	public static final int SHARE = 40;

	private RedditPost mPost;

	@Override
	public void onCreate(final Bundle savedInstanceState) {

		PrefsUtility.applyTheme(this);

		super.onCreate(savedInstanceState);

		final Intent intent = getIntent();

		final String url = intent.getStringExtra("url");
		mPost = intent.getParcelableExtra("post");

		if(url == null) {
			BugReportActivity.handleGlobalError(this, "No URL");
		}

		webView = WebViewFragment.newInstance(url, mPost);

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

	@Override
	public void onPostSelected(final RedditPreparedPost post) {
		LinkHandler.onLinkClicked(this, post.src.getUrl(), false, post.src.getSrc());
	}

	@Override
	public void onPostCommentsSelected(final RedditPreparedPost post) {
		LinkHandler.onLinkClicked(
				this,
				PostCommentListingURL.forPostId(post.src.getIdAlone()).toString(),
				false);
	}

	@Override
	public boolean onOptionsItemSelected(final MenuItem item) {

		final String currentUrl = webView.getCurrentUrl();

		switch(item.getItemId()) {

			case VIEW_IN_BROWSER:
				if(currentUrl != null) {
					try {
						final Intent intent = new Intent(Intent.ACTION_VIEW);
						intent.setData(Uri.parse(currentUrl));
						startActivity(intent);
						finish(); //to clear from backstack

					} catch(final Exception e) {
						Toast.makeText(
								this,
								"Error: could not launch browser.",
								Toast.LENGTH_LONG).show();
					}
				}
				return true;

			case CLEAR_CACHE:

				webView.clearCache();
				Toast.makeText(
						this,
						R.string.web_view_clear_cache_success_toast,
						Toast.LENGTH_LONG).show();
				return true;

			case USE_HTTPS:

				if(currentUrl != null) {

					if(currentUrl.startsWith("https://")) {
						General.quickToast(this, R.string.webview_https_already);
						return true;
					}

					if(!currentUrl.startsWith("http://")) {
						General.quickToast(this, R.string.webview_https_unknownprotocol);
						return true;
					}

					LinkHandler.onLinkClicked(
							this,
							currentUrl.replace("http://", "https://"),
							true,
							mPost);
					return true;
				}

			case SHARE:
				if(currentUrl != null) {
					LinkHandler.shareText(
							this,
							mPost != null ? mPost.title : null,
							currentUrl);
				}
				return true;

			default:
				return super.onOptionsItemSelected(item);
		}
	}

	@Override
	public boolean onCreateOptionsMenu(final Menu menu) {
		menu.add(0, VIEW_IN_BROWSER, 0, R.string.web_view_open_browser);
		menu.add(0, CLEAR_CACHE, 1, R.string.web_view_clear_cache);
		menu.add(0, USE_HTTPS, 2, R.string.webview_use_https);
		menu.add(0, SHARE, 3, R.string.action_share);
		return super.onCreateOptionsMenu(menu);
	}

	public String getCurrentUrl() {
		return webView.getCurrentUrl();
	}
}
