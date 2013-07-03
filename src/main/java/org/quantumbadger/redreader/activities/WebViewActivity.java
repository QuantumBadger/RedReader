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
import android.view.View;
import android.net.Uri;
import com.actionbarsherlock.view.Menu;
import com.actionbarsherlock.view.MenuItem;
import org.holoeverywhere.app.Activity;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.common.LinkHandler;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.fragments.WebViewFragment;
import org.quantumbadger.redreader.reddit.prepared.RedditPreparedPost;
import org.quantumbadger.redreader.reddit.things.RedditPost;
import org.quantumbadger.redreader.views.RedditPostView;

public class WebViewActivity extends Activity implements RedditPostView.PostSelectionListener {

    private WebViewFragment webView;
    private String url;
    public static final int VIEW_IN_BROWSER = 10;

    public void onCreate(final Bundle savedInstanceState) {

        PrefsUtility.applyTheme(this);

        getSupportActionBar().setHomeButtonEnabled(true);
        getSupportActionBar().setDisplayHomeAsUpEnabled(true);

        super.onCreate(savedInstanceState);

        final Intent intent = getIntent();

        url = intent.getStringExtra("url");
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
        if(!webView.onBackButtonPressed()) finish();
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
                if(url != null) {
                    Intent i = new Intent(Intent.ACTION_VIEW);
                    i.setData(Uri.parse(url));
                    finish(); //to clear from backstack
                    startActivity(i);
                }
            default:
                return super.onOptionsItemSelected(item);
        }
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        menu.add(0, VIEW_IN_BROWSER, 0, R.string.web_view_open_browser);
        return super.onCreateOptionsMenu(menu);
    }
}