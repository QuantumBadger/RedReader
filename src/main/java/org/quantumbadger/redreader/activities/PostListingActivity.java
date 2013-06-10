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

import android.content.DialogInterface;
import android.content.Intent;
import android.graphics.drawable.ColorDrawable;
import android.os.Bundle;
import android.support.v4.app.FragmentTransaction;
import android.view.WindowManager;
import com.actionbarsherlock.view.Menu;
import com.actionbarsherlock.view.MenuItem;
import org.holoeverywhere.app.AlertDialog;
import org.holoeverywhere.preference.PreferenceManager;
import org.holoeverywhere.preference.SharedPreferences;
import org.holoeverywhere.widget.EditText;
import org.holoeverywhere.widget.LinearLayout;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccountChangeListener;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.common.LinkHandler;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.fragments.PostListingFragment;
import org.quantumbadger.redreader.fragments.SessionListDialog;
import org.quantumbadger.redreader.listingcontrollers.PostListingController;
import org.quantumbadger.redreader.listingcontrollers.PostListingControllerSubreddit;
import org.quantumbadger.redreader.reddit.prepared.RedditPreparedPost;
import org.quantumbadger.redreader.reddit.things.RedditSubreddit;
import org.quantumbadger.redreader.views.RedditPostView;

import java.util.UUID;

// TODO handle general URIs
public class PostListingActivity extends RefreshableActivity
		implements RedditAccountChangeListener,
		RedditPostView.PostSelectionListener,
		SharedPreferences.OnSharedPreferenceChangeListener,
		OptionsMenuUtility.OptionsMenuPostsListener,
		SessionChangeListener {

	private PostListingFragment fragment;
	private PostListingController controller;
	private RedditSubreddit subreddit;

	private SharedPreferences sharedPreferences;

	public void onCreate(final Bundle savedInstanceState) {

		PrefsUtility.applyTheme(this);

		// TODO load from savedInstanceState

		getSupportActionBar().setHomeButtonEnabled(true);
		getSupportActionBar().setDisplayHomeAsUpEnabled(true);

        getWindow().setBackgroundDrawable(new ColorDrawable(getSupportActionBarContext().obtainStyledAttributes(new int[] {R.attr.rrListBackgroundCol}).getColor(0,0)));

		sharedPreferences = PreferenceManager.getDefaultSharedPreferences(this);
		sharedPreferences.registerOnSharedPreferenceChangeListener(this);

		RedditAccountManager.getInstance(this).addUpdateListener(this);

		if(getIntent() != null) {

			final Intent intent = getIntent();

			if(intent.hasExtra("subreddit")) {

				subreddit = intent.getParcelableExtra("subreddit");
				controller = new PostListingControllerSubreddit(subreddit);

				if(subreddit.isReal()) {
					OptionsMenuUtility.fixActionBar(this, subreddit.url);
				} else {
					OptionsMenuUtility.fixActionBar(this, subreddit.title);
				}

			} else {
				throw new RuntimeException("No subreddit provided");
			}

			super.onCreate(savedInstanceState);

			setContentView(R.layout.main_single);
			requestRefresh(RefreshableFragment.POSTS, false);

		} else {
			throw new RuntimeException("Nothing to show! (should load from bundle)"); // TODO
		}
	}

	@Override
	protected void onSaveInstanceState(final Bundle outState) {
		super.onSaveInstanceState(outState);
		// TODO save instance state
	}

	@Override
	public boolean onCreateOptionsMenu(final Menu menu) {

		OptionsMenuUtility.prepare(this, menu, false, true, false, subreddit.isSortable(), false);

		return true;
	}

	public void onRedditAccountChanged() {
		requestRefresh(RefreshableFragment.ALL, false);
	}

	@Override
	protected void doRefresh(final RefreshableFragment which, final boolean force) {
		if(fragment != null) fragment.cancel();
		fragment = controller.get(force);
		final FragmentTransaction transaction = getSupportFragmentManager().beginTransaction();
		transaction.replace(R.id.main_single_frame, fragment, "post_listing_fragment");
		transaction.commit();
	}

	public void onPostSelected(final RedditPreparedPost post) {
		LinkHandler.onLinkClicked(this, post.url, false, post.src);
	}

	public void onPostCommentsSelected(final RedditPreparedPost post) {
		final Intent intent = new Intent(this, CommentListingActivity.class);
		intent.putExtra("postId", post.idAlone);
		startActivityForResult(intent, 1);
	}

	public void onRefreshPosts() {
		controller.setSession(null);
		requestRefresh(RefreshableFragment.POSTS, true);
	}

	public void onPastPosts() {
		final SessionListDialog sessionListDialog = SessionListDialog.newInstance(controller.getUri(), controller.getSession(), SessionChangeType.POSTS);
		sessionListDialog.show(this);
	}

	public void onSubmitPost() {
		final Intent intent = new Intent(this, PostSubmitActivity.class);
		intent.putExtra("subreddit", controller.getSubreddit().display_name);
		startActivity(intent);
	}

	public void onSortSelected(final PostListingController.Sort order) {
		controller.setSort(order);
		requestRefresh(RefreshableFragment.POSTS, false);
	}

	public void onSearchPosts() {

		final AlertDialog.Builder alertBuilder = new AlertDialog.Builder(this);
		final LinearLayout layout = (LinearLayout) getLayoutInflater().inflate(R.layout.dialog_editbox);
		final EditText editText = (EditText)layout.findViewById(R.id.dialog_editbox_edittext);

		editText.requestFocus();

		alertBuilder.setView(layout);
		alertBuilder.setTitle(R.string.action_search);

		alertBuilder.setPositiveButton(R.string.action_search, new DialogInterface.OnClickListener() {
			public void onClick(DialogInterface dialog, int which) {

				final String query = editText.getText().toString().toLowerCase().trim();

				final RedditSubreddit sr = controller.getSubreddit();
				final String restrict_sr = sr.isReal() ? "on" : "off";

				final String url;

				if(sr.isReal()) {
					url = sr.url + "/search.json?restrict_sr=on&q=" + query;
				} else {
					url = "/search.json?q=" + query;
				}

				final Intent intent = new Intent(PostListingActivity.this, PostListingActivity.class);
				intent.putExtra("subreddit", new RedditSubreddit(url, "\"" + query + "\" search results", false));
				startActivity(intent);
			}
		});

		alertBuilder.setNegativeButton(R.string.dialog_cancel, null);

		final AlertDialog alertDialog = alertBuilder.create();
		alertDialog.getWindow().setSoftInputMode(WindowManager.LayoutParams.SOFT_INPUT_STATE_VISIBLE);
		alertDialog.show();
	}

	public void onSharedPreferenceChanged(final SharedPreferences prefs, final String key) {

		if(PrefsUtility.isRestartRequired(this, key)) {
			requestRefresh(RefreshableFragment.RESTART, false);
		}

		if(PrefsUtility.isRefreshRequired(this, key)) {
			requestRefresh(RefreshableFragment.ALL, false);
		}
	}

	@Override
	protected void onDestroy() {
		super.onDestroy();
		sharedPreferences.unregisterOnSharedPreferenceChangeListener(this);
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

	public void onSessionSelected(UUID session, SessionChangeType type) {
		controller.setSession(session);
		requestRefresh(RefreshableFragment.POSTS, false);
	}

	public void onSessionRefreshSelected(SessionChangeType type) {
		onRefreshPosts();
	}

	public void onSessionChanged(UUID session, SessionChangeType type, long timestamp) {
		controller.setSession(session);
	}
}
