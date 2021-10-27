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
import android.util.Log;
import android.view.Menu;
import android.view.MenuItem;
import androidx.annotation.NonNull;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccountChangeListener;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.common.DialogUtils;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.LinkHandler;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.fragments.CommentListingFragment;
import org.quantumbadger.redreader.fragments.SessionListDialog;
import org.quantumbadger.redreader.listingcontrollers.CommentListingController;
import org.quantumbadger.redreader.reddit.PostCommentSort;
import org.quantumbadger.redreader.reddit.UserCommentSort;
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

	private static final String TAG = "CommentListingActivity";

	public static final String EXTRA_SEARCH_STRING = "cla_search_string";

	private static final String SAVEDSTATE_SESSION = "cla_session";
	private static final String SAVEDSTATE_SORT = "cla_sort";
	private static final String SAVEDSTATE_SORT_IS_USER = "cla_sort_user";
	private static final String SAVEDSTATE_FRAGMENT = "cla_fragment";

	private CommentListingController controller;

	private CommentListingFragment mFragment;

	@Override
	public void onCreate(final Bundle savedInstanceState) {

		PrefsUtility.applyTheme(this);

		super.onCreate(savedInstanceState);

		setTitle(getString(R.string.app_name));

		RedditAccountManager.getInstance(this).addUpdateListener(this);

		if(getIntent() != null) {

			final Intent intent = getIntent();

			final String url = intent.getDataString();
			final String searchString = intent.getStringExtra(EXTRA_SEARCH_STRING);
			controller = new CommentListingController(
					RedditURLParser.parseProbableCommentListing(Uri.parse(url)));
			controller.setSearchString(searchString);

			Bundle fragmentSavedInstanceState = null;

			if(savedInstanceState != null) {

				if(savedInstanceState.containsKey(SAVEDSTATE_SESSION)) {
					controller.setSession(UUID.fromString(savedInstanceState.getString(
							SAVEDSTATE_SESSION)));
				}

				if(savedInstanceState.containsKey(SAVEDSTATE_SORT)) {
					if(savedInstanceState.getBoolean(SAVEDSTATE_SORT_IS_USER)) {
						controller.setSort(UserCommentSort.valueOf(
								savedInstanceState.getString(SAVEDSTATE_SORT)));
					} else {
						controller.setSort(PostCommentSort.valueOf(
								savedInstanceState.getString(SAVEDSTATE_SORT)));
					}
				}

				if(savedInstanceState.containsKey(SAVEDSTATE_FRAGMENT)) {
					fragmentSavedInstanceState = savedInstanceState.getBundle(
							SAVEDSTATE_FRAGMENT);
				}
			}

			doRefresh(RefreshableFragment.COMMENTS, false, fragmentSavedInstanceState);

		} else {
			throw new RuntimeException("Nothing to show!");
		}
	}

	@Override
	protected void onSaveInstanceState(@NonNull final Bundle outState) {
		super.onSaveInstanceState(outState);

		final UUID session = controller.getSession();
		if(session != null) {
			outState.putString(SAVEDSTATE_SESSION, session.toString());
		}

		final OptionsMenuUtility.Sort sort = controller.getSort();
		if(sort != null) {
			outState.putBoolean(SAVEDSTATE_SORT_IS_USER, controller.isUserCommentListing());
			outState.putString(SAVEDSTATE_SORT, sort.name());
		}

		if(mFragment != null) {
			outState.putBundle(SAVEDSTATE_FRAGMENT, mFragment.onSaveInstanceState());
		}
	}

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
				controller.isUserCommentListing(),
				false,
				controller.isSortable(),
				false,
				null,
				false,
				true,
				null,
				null);

		if(mFragment != null) {
			mFragment.onCreateOptionsMenu(menu);
		}

		return true;
	}

	@Override
	public void onRedditAccountChanged() {
		requestRefresh(RefreshableFragment.ALL, false);
	}

	@Override
	protected void doRefresh(
			final RefreshableFragment which,
			final boolean force,
			final Bundle savedInstanceState) {

		mFragment = controller.get(this, force, savedInstanceState);
		mFragment.setBaseActivityContent(this);

		setTitle(controller.getCommentListingUrl().humanReadableName(this, false));
		invalidateOptionsMenu();
	}

	@Override
	public void onRefreshComments() {
		controller.setSession(null);
		requestRefresh(RefreshableFragment.COMMENTS, true);
	}

	@Override
	public void onPastComments() {
		final SessionListDialog sessionListDialog = SessionListDialog.newInstance(
				controller.getUri(),
				controller.getSession(),
				SessionChangeListener.SessionChangeType.COMMENTS);
		sessionListDialog.show(getSupportFragmentManager(), null);
	}

	@Override
	public void onSortSelected(final PostCommentSort order) {
		controller.setSort(order);
		requestRefresh(RefreshableFragment.COMMENTS, false);
	}

	@Override
	public void onSortSelected(final UserCommentSort order) {
		controller.setSort(order);
		requestRefresh(RefreshableFragment.COMMENTS, false);
	}

	@Override
	public void onSearchComments() {
		DialogUtils.showSearchDialog(this, query -> {
			final Intent searchIntent = getIntent();
			searchIntent.putExtra(EXTRA_SEARCH_STRING, query);
			startActivity(searchIntent);
		});
	}

	@Override
	public boolean onOptionsItemSelected(@NonNull final MenuItem item) {

		if(mFragment != null) {
			if(mFragment.onOptionsItemSelected(item)) {
				return true;
			}
		}

		return super.onOptionsItemSelected(item);
	}

	@Override
	public void onSessionRefreshSelected(final SessionChangeType type) {
		onRefreshComments();
	}

	@Override
	public void onSessionSelected(final UUID session, final SessionChangeType type) {
		controller.setSession(session);
		requestRefresh(RefreshableFragment.COMMENTS, false);
	}

	@Override
	public void onSessionChanged(
			final UUID session,
			final SessionChangeType type,
			final long timestamp) {

		Log.i(
				TAG,
				type.name() + " session changed to " + (session != null
						? session.toString()
						: "<null>"));
		controller.setSession(session);
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
	public void onBackPressed() {
		if(General.onBackPressed()) {
			super.onBackPressed();
		}
	}

	@Override
	protected boolean baseActivityAllowToolbarHideOnScroll() {
		return true;
	}

	@Override
	public OptionsMenuUtility.Sort getCommentSort() {
		return controller.getSort();
	}

	@Override
	public PostCommentSort getSuggestedCommentSort() {
		if(mFragment == null || mFragment.getPost() == null) {
			return null;
		}

		return mFragment.getPost().src.getSuggestedCommentSort();
	}
}
