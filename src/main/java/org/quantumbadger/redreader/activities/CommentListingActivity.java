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
import android.content.SharedPreferences;
import android.graphics.Color;
import android.graphics.drawable.ColorDrawable;
import android.net.Uri;
import android.os.Bundle;
import android.preference.PreferenceManager;
import android.view.ContextMenu;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccountChangeListener;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.LinkHandler;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.fragments.CommentListingFragment;
import org.quantumbadger.redreader.fragments.SessionListDialog;
import org.quantumbadger.redreader.listingcontrollers.CommentListingController;
import org.quantumbadger.redreader.reddit.prepared.RedditPreparedPost;
import org.quantumbadger.redreader.reddit.url.PostCommentListingURL;
import org.quantumbadger.redreader.reddit.url.RedditURLParser;
import org.quantumbadger.redreader.views.RedditPostView;

import java.util.UUID;

public class CommentListingActivity extends RefreshableActivity
		implements RedditAccountChangeListener,
		OptionsMenuUtility.OptionsMenuCommentsListener,
		RedditPostView.PostSelectionListener,
		SessionChangeListener {

	private CommentListingController controller;

	private SharedPreferences sharedPreferences;

	private CommentListingFragment mFragment;

	public void onCreate(final Bundle savedInstanceState) {

        PrefsUtility.applyTheme(this);

		super.onCreate(savedInstanceState);

		getActionBar().setHomeButtonEnabled(true);
		getActionBar().setDisplayHomeAsUpEnabled(true);

		OptionsMenuUtility.fixActionBar(this, getString(R.string.app_name));

		sharedPreferences = PreferenceManager.getDefaultSharedPreferences(this);

		final boolean solidblack = PrefsUtility.appearance_solidblack(this, sharedPreferences)
				&& PrefsUtility.appearance_theme(this, sharedPreferences) == PrefsUtility.AppearanceTheme.NIGHT;

		// TODO load from savedInstanceState

		if(solidblack) getWindow().setBackgroundDrawable(new ColorDrawable(Color.BLACK));

		RedditAccountManager.getInstance(this).addUpdateListener(this);

		if(getIntent() != null) {

			final Intent intent = getIntent();

			final String url = intent.getDataString();
            controller = new CommentListingController(RedditURLParser.parseProbableCommentListing(Uri.parse(url)), this);

			doRefresh(RefreshableFragment.COMMENTS, false);

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
		OptionsMenuUtility.prepare(this, menu, false, false, true, false, false, controller.isSortable(), null, false, true, null, null);

		if(mFragment != null) {
			mFragment.onCreateOptionsMenu(menu);
		}

		return true;
	}

	public void onRedditAccountChanged() {
		requestRefresh(RefreshableFragment.ALL, false);
	}

	@Override
	protected void doRefresh(final RefreshableFragment which, final boolean force) {
		mFragment = controller.get(this, force);
		setContentView(mFragment.onCreateView());
		OptionsMenuUtility.fixActionBar(this, controller.getCommentListingUrl().humanReadableName(this, false));
		invalidateOptionsMenu();
	}

	public void onRefreshComments() {
		controller.setSession(null);
		requestRefresh(RefreshableFragment.COMMENTS, true);
	}

	public void onPastComments() {
		final SessionListDialog sessionListDialog = SessionListDialog.newInstance(controller.getUri(), controller.getSession(), SessionChangeListener.SessionChangeType.COMMENTS);
		sessionListDialog.show(getFragmentManager(), null);
	}

	public void onSortSelected(final PostCommentListingURL.Sort order) {
		controller.setSort(order);
		requestRefresh(RefreshableFragment.COMMENTS, false);
	}

	@Override
	public boolean onOptionsItemSelected(final MenuItem item) {

		if(mFragment != null) {
			if(mFragment.onOptionsItemSelected(item)) {
				return true;
			}
		}

		switch(item.getItemId()) {
			case android.R.id.home:
                finish();
                return true;
			default:
				return super.onOptionsItemSelected(item);
		}
	}

	public void onSessionRefreshSelected(SessionChangeType type) {
		onRefreshComments();
	}

	public void onSessionSelected(UUID session, SessionChangeType type) {
		controller.setSession(session);
		requestRefresh(RefreshableFragment.COMMENTS, false);
	}

	public void onSessionChanged(UUID session, SessionChangeType type, long timestamp) {
		controller.setSession(session);
	}

	public void onPostSelected(final RedditPreparedPost post) {
		LinkHandler.onLinkClicked(this, post.url, false, post.src);
	}

	public void onPostCommentsSelected(final RedditPreparedPost post) {
		LinkHandler.onLinkClicked(this, PostCommentListingURL.forPostId(post.idAlone).toString(), false);
	}

	@Override
	public void onBackPressed() {
		if(General.onBackPressed()) super.onBackPressed();
	}

	@Override
	public void onCreateContextMenu(final ContextMenu menu, final View v, final ContextMenu.ContextMenuInfo menuInfo) {

		if(mFragment != null) {
			mFragment.onCreateContextMenu(menu, v, menuInfo);
		}
	}

	@Override
	public boolean onContextItemSelected(final MenuItem item) {

		if(mFragment != null) {
			mFragment.onContextItemSelected(item);
		}

		return super.onContextItemSelected(item);
	}
}