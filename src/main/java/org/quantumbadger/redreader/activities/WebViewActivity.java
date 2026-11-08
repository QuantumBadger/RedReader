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
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;

import androidx.core.content.IntentCompat;

import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.common.LinkHandler;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.common.UriString;
import org.quantumbadger.redreader.fragments.WebViewFragment;
import org.quantumbadger.redreader.reddit.kthings.RedditPost;
import org.quantumbadger.redreader.reddit.prepared.RedditPreparedPost;
import org.quantumbadger.redreader.reddit.url.PostCommentListingURL;
import org.quantumbadger.redreader.views.RedditPostView;

public class WebViewActivity extends ViewsBaseActivity
		implements RedditPostView.PostSelectionListener {

	private WebViewFragment webView;
	public static final int VIEW_IN_BROWSER = 1000;
	public static final int CLEAR_CACHE = 1010;
	public static final int USE_HTTPS = 1020;
	public static final int SHARE = 1030;

	@Override
	public void onCreate(final Bundle savedInstanceState) {

		PrefsUtility.applyTheme(this);

		super.onCreate(savedInstanceState);

		final Intent intent = getIntent();

		final UriString url = IntentCompat.getParcelableExtra(intent, "url", UriString.class);
		final RedditPost post = IntentCompat.getParcelableExtra(intent, "post", RedditPost.class);

		if(url == null) {
			BugReportActivity.handleGlobalError(this, "No URL");
		}

		webView = WebViewFragment.newInstance(url, post);

		setBaseActivityListing(View.inflate(this, R.layout.main_single, null));

		getSupportFragmentManager().beginTransaction()
				.add(R.id.main_single_frame, webView)
				.commit();
	}

	@Override
	protected boolean baseActivityMustInterceptBack() {
		// Always intercept, as the WebView may need to navigate back through
		// its history. This is deliberately not conditional on canGoBack(), as
		// that would have to be rechecked on every navigation (including ones
		// the page makes itself via the history API), and missing one would
		// close the browser instead of going back.
		return true;
	}

	@Override
	protected boolean baseActivityOnBackPressed() {
		return webView.onBackButtonPressed();
	}

	@Override
	public void onPostSelected(final RedditPreparedPost post) {
		LinkHandler.onLinkClicked(this, post.src.getUrl(), false, post.src.getSrc());
	}

	@Override
	public void onPostCommentsSelected(final RedditPreparedPost post) {
		LinkHandler.onLinkClicked(
				this,
				PostCommentListingURL.forPostId(post.src.getIdAlone()).toUriString(),
				false);
	}

	@Override
	public boolean onOptionsItemSelected(final MenuItem item) {
		return webView.onBrowserMenuItemSelected(item) || super.onOptionsItemSelected(item);
	}

	@Override
	public boolean onCreateOptionsMenu(final Menu menu) {
		webView.addBrowserMenuItems(menu);
		return super.onCreateOptionsMenu(menu);
	}

	public UriString getCurrentUrl() {
		return webView.getCurrentUrl();
	}
}
