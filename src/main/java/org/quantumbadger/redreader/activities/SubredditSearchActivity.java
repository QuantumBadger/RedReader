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

import android.annotation.SuppressLint;
import android.os.Bundle;
import android.util.Log;
import android.view.View;
import android.view.ViewGroup;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.widget.SearchView;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.adapters.GroupedRecyclerViewAdapter;
import org.quantumbadger.redreader.adapters.GroupedRecyclerViewItemLoadingSpinner;
import org.quantumbadger.redreader.adapters.GroupedRecyclerViewItemRRError;
import org.quantumbadger.redreader.cache.CacheManager;
import org.quantumbadger.redreader.common.AndroidCommon;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.GenerationalCache;
import org.quantumbadger.redreader.common.Optional;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.common.StringUtils;
import org.quantumbadger.redreader.common.ThreadCheckedVar;
import org.quantumbadger.redreader.http.FailedRequestBody;
import org.quantumbadger.redreader.reddit.APIResponseHandler;
import org.quantumbadger.redreader.reddit.RedditAPI;
import org.quantumbadger.redreader.reddit.api.RedditSubredditSubscriptionManager;
import org.quantumbadger.redreader.reddit.things.RedditSubreddit;
import org.quantumbadger.redreader.viewholders.SubredditItemViewHolder;
import org.quantumbadger.redreader.views.LoadingSpinnerView;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;

public class SubredditSearchActivity extends BaseActivity implements
		RedditSubredditSubscriptionManager.SubredditSubscriptionStateChangeListener {

	private static final String TAG = "SubredditSearchActivity";

	@NonNull private final ThreadCheckedVar<SearchView> mSearchView
			= new ThreadCheckedVar<>(null);

	@NonNull private final ThreadCheckedVar<Optional<ArrayList<RedditSubreddit>>>
			mSubscriptions = new ThreadCheckedVar<>(Optional.empty());

	@NonNull private final ThreadCheckedVar<HashSet<String>> mQueriesPending
			= new ThreadCheckedVar<>(new HashSet<>());

	@NonNull private final ThreadCheckedVar<Boolean> mSubscriptionListPending
			= new ThreadCheckedVar<>(false);

	@NonNull private final ThreadCheckedVar<HashMap<String, ArrayList<RedditSubreddit>>>
			mQueryResults = new ThreadCheckedVar<>(new HashMap<>());

	@NonNull private final GenerationalCache<RedditSubreddit, SubredditItem>
			mSubredditItemCache = new GenerationalCache<>(SubredditItem::new);

	@NonNull private Optional<RedditSubredditSubscriptionManager.ListenerContext>
			mSubredditSubscriptionListenerContext = Optional.empty();

	private LoadingSpinnerView mLoadingSpinner;
	private RecyclerView mRecyclerView;
	private LinearLayoutManager mRecyclerViewLayout;
	private GroupedRecyclerViewItemLoadingSpinner mLoadingItem;

	@NonNull private final ThreadCheckedVar<Optional<GroupedRecyclerViewItemRRError>>
			mSubscriptionsErrorItem = new ThreadCheckedVar<>(Optional.empty());

	@NonNull private final ThreadCheckedVar<Optional<GroupedRecyclerViewItemRRError>>
			mQueryErrorItem = new ThreadCheckedVar<>(Optional.empty());

	private GroupedRecyclerViewAdapter mRecyclerViewAdapter;
	private static final int GROUP_SUBREDDITS = 0;
	private static final int GROUP_LOADING_SPINNER = 1;

	@Override
	public void onSubredditSubscriptionListUpdated(
			final RedditSubredditSubscriptionManager subredditSubscriptionManager) {

		AndroidCommon.runOnUiThread(() -> mSubscriptions.set(Optional.empty()));
	}

	@Override
	public void onSubredditSubscriptionAttempted(
			final RedditSubredditSubscriptionManager subredditSubscriptionManager) {
		// Ignore
	}

	@Override
	public void onSubredditUnsubscriptionAttempted(
			final RedditSubredditSubscriptionManager subredditSubscriptionManager) {
		// Ignore
	}

	private class SubredditItem
			extends GroupedRecyclerViewAdapter.Item<SubredditItemViewHolder> {

		@NonNull private final RedditSubreddit mSubreddit;

		private SubredditItem(@NonNull final RedditSubreddit subreddit) {
			mSubreddit = subreddit;
		}

		@Override
		public Class<RedditSubreddit> getViewType() {
			return RedditSubreddit.class;
		}

		@Override
		public SubredditItemViewHolder onCreateViewHolder(final ViewGroup viewGroup) {
			return new SubredditItemViewHolder(viewGroup, SubredditSearchActivity.this);
		}

		@Override
		public void onBindViewHolder(final SubredditItemViewHolder holder) {
			holder.bind(mSubreddit);
		}

		@Override
		public boolean isHidden() {
			return false;
		}
	}

	@Override
	protected boolean baseActivityIsToolbarSearchBarEnabled() {
		return true;
	}

	@SuppressLint("NotifyDataSetChanged")
	private void updateList() {

		General.checkThisIsUIThread();

		Log.i(TAG, "Updating list");

		if(!mLoadingItem.isHidden()) {
			mLoadingItem.setHidden(true);
			mRecyclerViewAdapter.updateHiddenStatus();
		}

		if(mSubscriptionsErrorItem.get().isPresent()) {

			mRecyclerViewAdapter.removeAllFromGroup(GROUP_SUBREDDITS);
			mRecyclerViewAdapter.appendToGroup(
					GROUP_SUBREDDITS,
					mSubscriptionsErrorItem.get().get());

			mRecyclerView.setVisibility(View.VISIBLE);
			mLoadingSpinner.setVisibility(View.GONE);

			return;
		}

		final String currentQuery = mSearchView.get().getQuery().toString();

		if(mSubscriptions.get().isEmpty()) {

			Log.i(TAG, "Subscriptions not downloaded yet");

			mRecyclerView.setVisibility(View.GONE);
			mLoadingSpinner.setVisibility(View.VISIBLE);

			if(mSubscriptionListPending.get() != Boolean.TRUE) {
				requestSubscriptions();
			}

		} else {

			mRecyclerViewAdapter.removeAllFromGroup(GROUP_SUBREDDITS);

			final HashSet<String> shownSubreddits = new HashSet<>(256);

			final ArrayList<RedditSubreddit> possibleSuggestions
					= new ArrayList<>(mSubscriptions.get().get());

			final String asciiLowercaseQuery = StringUtils.asciiLowercase(currentQuery);

			{
				final Iterator<RedditSubreddit> it = possibleSuggestions.iterator();

				while(it.hasNext()) {
					final RedditSubreddit entry = it.next();

					final String lowercaseName
							= StringUtils.asciiLowercase(entry.display_name);

					if(lowercaseName.startsWith(asciiLowercaseQuery)
							&& shownSubreddits.add(lowercaseName)) {
						mRecyclerViewAdapter.appendToGroup(
								GROUP_SUBREDDITS,
								mSubredditItemCache.get(entry));
						it.remove();
					}
				}
			}

			{
				final Iterator<RedditSubreddit> it = possibleSuggestions.iterator();

				while(it.hasNext()) {
					final RedditSubreddit entry = it.next();

					final String lowercaseName
							= StringUtils.asciiLowercase(entry.display_name);

					if(lowercaseName.contains(asciiLowercaseQuery)
							&& shownSubreddits.add(lowercaseName)) {
						mRecyclerViewAdapter.appendToGroup(
								GROUP_SUBREDDITS,
								mSubredditItemCache.get(entry));
						it.remove();
					}
				}
			}

			final ArrayList<RedditSubreddit> currentQueryResults
					= mQueryResults.get().get(currentQuery);

			if(currentQueryResults != null) {
				for(final RedditSubreddit subreddit : currentQueryResults) {
					final String name = StringUtils.asciiLowercase(subreddit.display_name);
					if(shownSubreddits.add(name)) {
						mRecyclerViewAdapter.appendToGroup(
								GROUP_SUBREDDITS,
								mSubredditItemCache.get(subreddit));
					}
				}
			} else if(!currentQuery.trim().isEmpty()) {

				if(mQueryErrorItem.get().isPresent()) {
					mRecyclerViewAdapter.appendToGroup(
							GROUP_SUBREDDITS,
							mQueryErrorItem.get().get());

				} else {
					mLoadingItem.setHidden(false);
					mRecyclerViewAdapter.updateHiddenStatus();
				}
			}

			mRecyclerViewAdapter.notifyDataSetChanged();
			mSubredditItemCache.nextGeneration();

			mRecyclerView.setVisibility(View.VISIBLE);
			mLoadingSpinner.setVisibility(View.GONE);
		}
	}

	@Override
	protected void onCreate(final Bundle savedInstanceState) {
		PrefsUtility.applyTheme(this);
		super.onCreate(savedInstanceState);

		mLoadingItem = new GroupedRecyclerViewItemLoadingSpinner(this);

		final SearchView searchView = findViewById(R.id.actionbar_search_view);
		mSearchView.set(searchView);
		searchView.setQueryHint(getString(R.string.find_subreddit));
		searchView.requestFocus();

		setBaseActivityListing(R.layout.subreddit_search_listing);

		mLoadingSpinner = findViewById(R.id.subreddit_search_loading_spinner);
		mRecyclerView = findViewById(R.id.subreddit_search_recyclerview);

		mRecyclerViewLayout = new LinearLayoutManager(
				this,
				RecyclerView.VERTICAL,
				false);

		mRecyclerViewAdapter = new GroupedRecyclerViewAdapter(2);
		mRecyclerViewAdapter.appendToGroup(GROUP_LOADING_SPINNER, mLoadingItem);

		mRecyclerView.setLayoutManager(mRecyclerViewLayout);
		mRecyclerView.setAdapter(mRecyclerViewAdapter);

		final RedditAccount user
				= RedditAccountManager.getInstance(this).getDefaultAccount();

		final RedditSubredditSubscriptionManager subscriptionManager
				= RedditSubredditSubscriptionManager.getSingleton(this, user);

		mSubredditSubscriptionListenerContext
				= Optional.of(subscriptionManager.addListener(this));

		requestSubscriptions();

		searchView.setOnQueryTextListener(new SearchView.OnQueryTextListener() {
			@Override
			public boolean onQueryTextSubmit(final String query) {
				handleQueryChanged(query);
				return true;
			}

			@Override
			public boolean onQueryTextChange(final String newText) {
				handleQueryChanged(newText);
				return true;
			}
		});

		updateList();
	}

	private void handleQueryChanged(@NonNull final String text) {

		mSubscriptionsErrorItem.set(Optional.empty());
		mQueryErrorItem.set(Optional.empty());

		updateList();
		mRecyclerViewLayout.scrollToPosition(0);

		if(text.isEmpty()) {
			return;
		}

		if(mQueriesPending.get().contains(text)) {
			// Do nothing, let's just wait for now.
			return;
		}

		if(!mQueryResults.get().containsKey(text)) {

			mQueriesPending.get().add(text);

			// Wait 1 second to avoid sending requests too fast
			AndroidCommon.UI_THREAD_HANDLER.postDelayed(
					() -> {
						if(text.contentEquals(mSearchView.get().getQuery())) {
							doSearchRequest(text);
						} else {
							mQueriesPending.get().remove(text);
						}
					},
					1000
			);
		}
	}

	private void doSearchRequest(@NonNull final String text) {

		Log.i(TAG, "Running search");

		final CacheManager cacheManager = CacheManager.getInstance(this);
		final RedditAccount user
				= RedditAccountManager.getInstance(this).getDefaultAccount();

		RedditAPI.searchSubreddits(
				cacheManager,
				user,
				text,
				this,
				new APIResponseHandler.ValueResponseHandler<
						RedditAPI.SubredditListResponse>(this) {

					@Override
					protected void onSuccess(
							@NonNull final RedditAPI.SubredditListResponse value) {

						Log.i(TAG, "Search results received");

						AndroidCommon.runOnUiThread(() -> {
							mQueryResults.get().put(text, value.subreddits);
							mQueriesPending.get().remove(text);
							updateList();
						});
					}

					@Override
					protected void onCallbackException(final Throwable t) {

						BugReportActivity.handleGlobalError(
								SubredditSearchActivity.this,
								t);

						AndroidCommon.runOnUiThread(() -> {
							mQueriesPending.get().remove(text);
						});
					}

					@Override
					protected void onFailure(
							final int type,
							@Nullable final Throwable t,
							@Nullable final Integer status,
							@Nullable final String readableMessage,
							@NonNull final Optional<FailedRequestBody> response) {

						Log.i(TAG, "Got error receiving search results: " + type + ", " + t);

						AndroidCommon.runOnUiThread(() -> {
							mQueriesPending.get().remove(text);
							mQueryErrorItem.set(Optional.of(
									new GroupedRecyclerViewItemRRError(
											SubredditSearchActivity.this,
											General.getGeneralErrorForFailure(
													SubredditSearchActivity.this,
													type,
													t,
													status,
													null,
													response))));
							updateList();
						});
					}

					@Override
					protected void onFailure(
							@NonNull final APIFailureType type,
							@Nullable final String debuggingContext,
							@NonNull final Optional<FailedRequestBody> response) {

						AndroidCommon.runOnUiThread(() -> {
							mQueriesPending.get().remove(text);
							mQueryErrorItem.set(Optional.of(
									new GroupedRecyclerViewItemRRError(
											SubredditSearchActivity.this,
											General.getGeneralErrorForFailure(
													SubredditSearchActivity.this,
													type,
													debuggingContext,
													response))));
							updateList();
						});
					}
				},
				Optional.empty());
	}

	private void requestSubscriptions() {

		if(mSubscriptionListPending.get() == Boolean.TRUE) {
			Log.i(TAG, "Subscription list already pending");
			return;
		}

		final CacheManager cacheManager = CacheManager.getInstance(this);

		final RedditAccount user
				= RedditAccountManager.getInstance(this).getDefaultAccount();

		mSubscriptionListPending.set(true);

		if(user.isNotAnonymous()) {

			Log.i(TAG, "Requesting subscriptions");

			RedditAPI.subscribedSubreddits(
					cacheManager,
					user,
					this,
					new APIResponseHandler.ValueResponseHandler<
							ArrayList<RedditSubreddit>>(this) {

						@Override
						protected void onSuccess(
								@NonNull final ArrayList<RedditSubreddit> value) {

							Log.i(TAG, "Subscriptions received: " + value.size());
							Collections.sort(value);

							AndroidCommon.runOnUiThread(() -> {
								mSubscriptionListPending.set(false);
								mSubscriptions.set(Optional.of(value));
								updateList();
							});
						}

						@Override
						protected void onCallbackException(
								final Throwable t) {

							AndroidCommon.runOnUiThread(() -> mSubscriptionListPending.set(false));
							BugReportActivity.handleGlobalError(
									SubredditSearchActivity.this,
									t);
						}

						@Override
						protected void onFailure(
								final int type,
								@Nullable final Throwable t,
								@Nullable final Integer status,
								@Nullable final String readableMessage,
								@NonNull final Optional<FailedRequestBody> response) {

							AndroidCommon.runOnUiThread(() -> {
								mSubscriptionListPending.set(false);
								mSubscriptionsErrorItem.set(Optional.of(
										new GroupedRecyclerViewItemRRError(
												SubredditSearchActivity.this,
												General.getGeneralErrorForFailure(
														SubredditSearchActivity.this,
														type,
														t,
														status,
														null,
														response))));
								updateList();
							});
						}

						@Override
						protected void onFailure(
								@NonNull final APIFailureType type,
								@Nullable final String debuggingContext,
								@NonNull final Optional<FailedRequestBody> response) {

							AndroidCommon.runOnUiThread(() -> {
								mSubscriptionListPending.set(false);
								mSubscriptionsErrorItem.set(Optional.of(
										new GroupedRecyclerViewItemRRError(
												SubredditSearchActivity.this,
												General.getGeneralErrorForFailure(
														SubredditSearchActivity.this,
														type,
														debuggingContext,
														response))));
								updateList();
							});
						}
					});
		} else {

			Log.i(TAG, "Requesting popular");

			RedditAPI.popularSubreddits(
					cacheManager,
					user,
					this,
					new APIResponseHandler.ValueResponseHandler<
							RedditAPI.SubredditListResponse>(this) {

						@Override
						protected void onSuccess(
								@NonNull final RedditAPI.SubredditListResponse value) {

							Collections.sort(value.subreddits, (a, b) -> {

								final int subsA = a.subscribers != null
										? a.subscribers
										: 0;

								final int subsB = b.subscribers != null
										? b.subscribers
										: 0;

								return Integer.compare(subsB, subsA);
							});

							AndroidCommon.runOnUiThread(() -> {
								mSubscriptionListPending.set(false);
								mSubscriptions.set(Optional.of(value.subreddits));
								updateList();
							});
						}

						@Override
						protected void onCallbackException(
								final Throwable t) {

							AndroidCommon.runOnUiThread(() -> mSubscriptionListPending.set(false));
							BugReportActivity.handleGlobalError(
									SubredditSearchActivity.this,
									t);
						}

						@Override
						protected void onFailure(
								final int type,
								@Nullable final Throwable t,
								@Nullable final Integer status,
								@Nullable final String readableMessage,
								@NonNull final Optional<FailedRequestBody> response) {

							AndroidCommon.runOnUiThread(() -> {
								mSubscriptionListPending.set(false);
								mSubscriptionsErrorItem.set(Optional.of(
										new GroupedRecyclerViewItemRRError(
												SubredditSearchActivity.this,
												General.getGeneralErrorForFailure(
														SubredditSearchActivity.this,
														type,
														t,
														status,
														null,
														response))));
								updateList();
							});
						}

						@Override
						protected void onFailure(
								@NonNull final APIFailureType type,
								@Nullable final String debuggingContext,
								@NonNull final Optional<FailedRequestBody> response) {

							AndroidCommon.runOnUiThread(() -> {
								mSubscriptionListPending.set(false);
								mSubscriptionsErrorItem.set(Optional.of(
										new GroupedRecyclerViewItemRRError(
												SubredditSearchActivity.this,
												General.getGeneralErrorForFailure(
														SubredditSearchActivity.this,
														type,
														debuggingContext,
														response))));
								updateList();
							});
						}
					},
					Optional.empty());
		}
	}

	@Override
	protected void onDestroy() {
		super.onDestroy();
		mSubredditSubscriptionListenerContext.apply(
				RedditSubredditSubscriptionManager.ListenerContext::removeListener);
	}
}
