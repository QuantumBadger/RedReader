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

import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.Toast;
import org.saiditnet.redreader.R;
import org.saiditnet.redreader.common.General;
import org.saiditnet.redreader.common.LinkHandler;
import org.saiditnet.redreader.common.PrefsUtility;
import org.saiditnet.redreader.fragments.WebViewFragment;
import org.saiditnet.redreader.reddit.prepared.RedditPreparedPost;
import org.saiditnet.redreader.reddit.things.RedditPost;
import org.saiditnet.redreader.reddit.url.PostCommentListingURL;
import org.saiditnet.redreader.views.RedditPostView;

public class WebViewActivity extends BaseActivity implements RedditPostView.PostSelectionListener {

	private WebViewFragment webView;
	public static final int VIEW_IN_BROWSER = 10,
			CLEAR_CACHE = 20,
			USE_HTTPS = 30,
			SHARE = 40;

	private RedditPost mPost;

	public void onCreate(final Bundle savedInstanceState) {

		PrefsUtility.applyTheme(this);

		super.onCreate(savedInstanceState);

		final Intent intent = getIntent();

		String url = intent.getStringExtra("url");
		mPost = intent.getParcelableExtra("post");

		if(url == null) {
			BugReportActivity.handleGlobalError(this, "No URL");
		}

		webView = WebViewFragment.newInstance(url, mPost);

		setBaseActivityContentView(View.inflate(this, R.layout.main_single, null));

		getSupportFragmentManager().beginTransaction().add(R.id.main_single_frame, webView).commit();
	}

	@Override
	public void onBackPressed() {

		if(General.onBackPressed() && !webView.onBackButtonPressed())
			super.onBackPressed();
	}

	public void onPostSelected(final RedditPreparedPost post) {
		LinkHandler.onLinkClicked(this, post.src.getUrl(), false, post.src.getSrc());
	}

	public void onPostCommentsSelected(final RedditPreparedPost post) {
		LinkHandler.onLinkClicked(this, PostCommentListingURL.forPostId(post.src.getIdAlone()).toString(), false);
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

					} catch(Exception e) {
						Toast.makeText(this, "Error: could not launch browser.", Toast.LENGTH_LONG).show();
					}
				}
				return true;

			case CLEAR_CACHE:

				webView.clearCache();
				Toast.makeText(this, R.string.web_view_clear_cache_success_toast, Toast.LENGTH_LONG).show();
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

					LinkHandler.onLinkClicked(this, currentUrl.replace("http://", "https://"), true, mPost);
					return true;
				}

			case SHARE:
				if (currentUrl != null){
					final Intent mailer = new Intent(Intent.ACTION_SEND);
					mailer.setType("text/plain");
					if (mPost != null){
						mailer.putExtra(Intent.EXTRA_SUBJECT, mPost.title);
					}
					mailer.putExtra(Intent.EXTRA_TEXT, currentUrl);
					startActivity(Intent.createChooser(mailer, getString(R.string.action_share)));
				}
				return true;

			default:
				return super.onOptionsItemSelected(item);
		}
	}

	@Override
	public boolean onCreateOptionsMenu(Menu menu) {
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
