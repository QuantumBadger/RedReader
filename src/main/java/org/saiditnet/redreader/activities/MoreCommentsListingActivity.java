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
import android.os.Bundle;
import android.support.annotation.Nullable;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.FrameLayout;
import org.saiditnet.redreader.R;
import org.saiditnet.redreader.account.RedditAccountChangeListener;
import org.saiditnet.redreader.account.RedditAccountManager;
import org.saiditnet.redreader.common.DialogUtils;
import org.saiditnet.redreader.common.General;
import org.saiditnet.redreader.common.LinkHandler;
import org.saiditnet.redreader.common.PrefsUtility;
import org.saiditnet.redreader.fragments.CommentListingFragment;
import org.saiditnet.redreader.reddit.prepared.RedditPreparedPost;
import org.saiditnet.redreader.reddit.url.PostCommentListingURL;
import org.saiditnet.redreader.reddit.url.RedditURLParser;
import org.saiditnet.redreader.reddit.url.UserCommentListingURL;
import org.saiditnet.redreader.views.RedditPostView;

import java.util.ArrayList;

public class MoreCommentsListingActivity extends RefreshableActivity
		implements RedditAccountChangeListener,
		OptionsMenuUtility.OptionsMenuCommentsListener,
		RedditPostView.PostSelectionListener {

	private static final String EXTRA_SEARCH_STRING = "mcla_search_string";

	private final ArrayList<RedditURLParser.RedditURL> mUrls = new ArrayList<>(32);

	private CommentListingFragment mFragment;

	private FrameLayout mPane;

	private String mSearchString = null;

	public void onCreate(final Bundle savedInstanceState) {

		PrefsUtility.applyTheme(this);

		super.onCreate(savedInstanceState);

		setTitle(R.string.app_name);

		// TODO load from savedInstanceState

		final View layout = getLayoutInflater().inflate(R.layout.main_single, null);
		setBaseActivityContentView(layout);
		mPane = (FrameLayout)layout.findViewById(R.id.main_single_frame);

		RedditAccountManager.getInstance(this).addUpdateListener(this);

		if(getIntent() != null) {

			final Intent intent = getIntent();
			mSearchString = intent.getStringExtra(EXTRA_SEARCH_STRING);

			final ArrayList<String> commentIds = intent.getStringArrayListExtra("commentIds");
			final String postId = intent.getStringExtra("postId");

			for(final String commentId : commentIds) {
				mUrls.add(PostCommentListingURL.forPostId(postId).commentId(commentId));
			}

			doRefresh(RefreshableFragment.COMMENTS, false, null);

		} else {
			throw new RuntimeException("Nothing to show! (should load from bundle)"); // TODO
		}
	}

	// TODO save instance state
	// @Override
	// protected void onSaveInstanceState(final Bundle outState) {
	// 	super.onSaveInstanceState(outState);
	// }

	@Override
	public boolean onCreateOptionsMenu(final Menu menu) {
		OptionsMenuUtility.prepare(
				this,
				menu,
				false,
				false,
				true,
				false,
				false,
				false,
				false,
				false,
				false,
				null,
				false,
				false,
				null,
				null);

		if(mFragment != null) {
			mFragment.onCreateOptionsMenu(menu);
		}

		return true;
	}

	public void onRedditAccountChanged() {
		requestRefresh(RefreshableFragment.ALL, false);
	}

	@Override
	protected void doRefresh(final RefreshableFragment which, final boolean force, final Bundle savedInstanceState) {

		mFragment = new CommentListingFragment(
				this,
				savedInstanceState,
				mUrls,
				null,
				mSearchString,
				force);

		mPane.removeAllViews();

		final View view = mFragment.getView();
		mPane.addView(view);
		General.setLayoutMatchParent(view);

		setTitle("More Comments");
	}

	public void onRefreshComments() {
		requestRefresh(RefreshableFragment.COMMENTS, true);
	}

	@Override
	public void onPastComments() {}

	@Override
	public void onSortSelected(final PostCommentListingURL.Sort order) {}

	@Override
	public void onSortSelected(final UserCommentListingURL.Sort order) {}

	@Override
	public void onSearchComments() {
		DialogUtils.showSearchDialog(this, new DialogUtils.OnSearchListener() {
			@Override
			public void onSearch(@Nullable String query) {
				Intent searchIntent = getIntent();
				searchIntent.putExtra(EXTRA_SEARCH_STRING, query);
				startActivity(searchIntent);
			}
		});
	}

	@Override
	public boolean onOptionsItemSelected(final MenuItem item) {

		if(mFragment != null) {
			if(mFragment.onOptionsItemSelected(item)) {
				return true;
			}
		}

		return super.onOptionsItemSelected(item);
	}

	public void onPostSelected(final RedditPreparedPost post) {
		LinkHandler.onLinkClicked(this, post.src.getUrl(), false, post.src.getSrc());
	}

	public void onPostCommentsSelected(final RedditPreparedPost post) {
		LinkHandler.onLinkClicked(this, PostCommentListingURL.forPostId(post.src.getIdAlone()).toString(), false);
	}

	@Override
	public void onBackPressed() {
		if(General.onBackPressed()) super.onBackPressed();
	}
}
