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

package org.saiditnet.redreader.fragments;

import android.app.Activity;
import android.content.Context;
import android.content.SharedPreferences;
import android.net.Uri;
import android.os.Bundle;
import android.preference.PreferenceManager;
import android.support.annotation.NonNull;
import android.support.v4.widget.SwipeRefreshLayout;
import android.support.v7.app.AppCompatActivity;
import android.support.v7.widget.LinearLayoutManager;
import android.support.v7.widget.RecyclerView;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.LinearLayout;
import android.widget.TextView;
import android.widget.Toast;
import org.apache.commons.lang3.StringEscapeUtils;
import org.saiditnet.redreader.R;
import org.saiditnet.redreader.account.RedditAccount;
import org.saiditnet.redreader.account.RedditAccountManager;
import org.saiditnet.redreader.activities.BugReportActivity;
import org.saiditnet.redreader.activities.OptionsMenuUtility;
import org.saiditnet.redreader.activities.SessionChangeListener;
import org.saiditnet.redreader.adapters.PostListingManager;
import org.saiditnet.redreader.cache.CacheManager;
import org.saiditnet.redreader.cache.CacheRequest;
import org.saiditnet.redreader.cache.downloadstrategy.DownloadStrategy;
import org.saiditnet.redreader.cache.downloadstrategy.DownloadStrategyAlways;
import org.saiditnet.redreader.cache.downloadstrategy.DownloadStrategyIfNotCached;
import org.saiditnet.redreader.cache.downloadstrategy.DownloadStrategyIfTimestampOutsideBounds;
import org.saiditnet.redreader.cache.downloadstrategy.DownloadStrategyNever;
import org.saiditnet.redreader.common.AndroidCommon;
import org.saiditnet.redreader.common.Constants;
import org.saiditnet.redreader.common.General;
import org.saiditnet.redreader.common.LinkHandler;
import org.saiditnet.redreader.common.PrefsUtility;
import org.saiditnet.redreader.common.RRError;
import org.saiditnet.redreader.common.RRTime;
import org.saiditnet.redreader.common.TimestampBound;
import org.saiditnet.redreader.image.GetImageInfoListener;
import org.saiditnet.redreader.image.ImageInfo;
import org.saiditnet.redreader.io.RequestResponseHandler;
import org.saiditnet.redreader.jsonwrap.JsonBufferedArray;
import org.saiditnet.redreader.jsonwrap.JsonBufferedObject;
import org.saiditnet.redreader.jsonwrap.JsonValue;
import org.saiditnet.redreader.listingcontrollers.CommentListingController;
import org.saiditnet.redreader.reddit.PostSort;
import org.saiditnet.redreader.reddit.RedditPostListItem;
import org.saiditnet.redreader.reddit.RedditSubredditManager;
import org.saiditnet.redreader.reddit.api.RedditSubredditSubscriptionManager;
import org.saiditnet.redreader.reddit.api.SubredditRequestFailure;
import org.saiditnet.redreader.reddit.prepared.RedditParsedPost;
import org.saiditnet.redreader.reddit.prepared.RedditPreparedPost;
import org.saiditnet.redreader.reddit.things.RedditPost;
import org.saiditnet.redreader.reddit.things.RedditSubreddit;
import org.saiditnet.redreader.reddit.things.RedditThing;
import org.saiditnet.redreader.reddit.url.PostCommentListingURL;
import org.saiditnet.redreader.reddit.url.PostListingURL;
import org.saiditnet.redreader.reddit.url.RedditURLParser;
import org.saiditnet.redreader.reddit.url.SearchPostListURL;
import org.saiditnet.redreader.reddit.url.SubredditPostListURL;
import org.saiditnet.redreader.views.PostListingHeader;
import org.saiditnet.redreader.views.RedditPostView;
import org.saiditnet.redreader.views.ScrollbarRecyclerViewManager;
import org.saiditnet.redreader.views.SearchListingHeader;
import org.saiditnet.redreader.views.liststatus.ErrorView;

import java.net.URI;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicInteger;

public class PostListingFragment extends RRFragment
		implements RedditPostView.PostSelectionListener {

	private static final String TAG = "PostListingFragment";

	private static final String SAVEDSTATE_FIRST_VISIBLE_POS = "firstVisiblePosition";

	private PostListingURL mPostListingURL;

	private RedditSubreddit mSubreddit;

	private UUID mSession;
	private final int mPostCountLimit;
	private TextView mLoadMoreView;

	private final SharedPreferences mSharedPreferences;

	private final PostListingManager mPostListingManager;
	private final RecyclerView mRecyclerView;

	private final View mOuter;

	private String mAfter = null, mLastAfter = null;
	private CacheRequest mRequest;
	private boolean mReadyToDownloadMore = false;
	private long mTimestamp;

	private int mPostCount = 0;
	private final AtomicInteger mPostRefreshCount = new AtomicInteger(0);

	private final HashSet<String> mPostIds = new HashSet<>(200);

	private Integer mPreviousFirstVisibleItemPosition;

	// Session may be null
	public PostListingFragment(
			final AppCompatActivity parent,
			final Bundle savedInstanceState,
			final Uri url,
			final UUID session,
			final boolean forceDownload) {

		super(parent, savedInstanceState);

		mPostListingManager = new PostListingManager(parent);

		if(savedInstanceState != null) {
			mPreviousFirstVisibleItemPosition = savedInstanceState.getInt(SAVEDSTATE_FIRST_VISIBLE_POS);
		}

		try {
			mPostListingURL = (PostListingURL) RedditURLParser.parseProbablePostListing(url);
		} catch(ClassCastException e) {
			Toast.makeText(getActivity(), "Invalid post listing URL.", Toast.LENGTH_LONG).show();
			// TODO proper error handling -- show error view
			throw new RuntimeException("Invalid post listing URL");
		}

		mSession = session;

		final Context context = getContext();
		mSharedPreferences = PreferenceManager.getDefaultSharedPreferences(context);

		// TODO output failed URL
		if(mPostListingURL == null) {
			mPostListingManager.addFooterError(
					new ErrorView(getActivity(), new RRError("Invalid post listing URL", "Could not navigate to that URL.")));
			// TODO proper error handling
			throw new RuntimeException("Invalid post listing URL");
		}

		switch(PrefsUtility.pref_behaviour_post_count(context, mSharedPreferences)) {
			case ALL:
				mPostCountLimit = -1;
				break;
			case R25:
				mPostCountLimit = 25;
				break;
			case R50:
				mPostCountLimit = 50;
				break;
			case R100:
				mPostCountLimit = 100;
				break;
			default:
				mPostCountLimit = 0;
				break;
		}

		if(mPostCountLimit > 0) {
			restackRefreshCount();
		}

		final ScrollbarRecyclerViewManager recyclerViewManager
				= new ScrollbarRecyclerViewManager(context, null, false);

		if(parent instanceof OptionsMenuUtility.OptionsMenuPostsListener
				&& PrefsUtility.pref_behaviour_enable_swipe_refresh(context, mSharedPreferences)) {

			recyclerViewManager.enablePullToRefresh(new SwipeRefreshLayout.OnRefreshListener() {
				@Override
				public void onRefresh() {
					((OptionsMenuUtility.OptionsMenuPostsListener)parent).onRefreshPosts();
				}
			});
		}

		mRecyclerView = recyclerViewManager.getRecyclerView();
		mPostListingManager.setLayoutManager((LinearLayoutManager) mRecyclerView.getLayoutManager());

		mRecyclerView.setAdapter(mPostListingManager.getAdapter());

		mOuter = recyclerViewManager.getOuterView();

		mRecyclerView.addOnScrollListener(new RecyclerView.OnScrollListener() {
			@Override
			public void onScrolled(final RecyclerView recyclerView, final int dx, final int dy) {
				onLoadMoreItemsCheck();
			}
		});

		mRecyclerView.getLayoutParams().height = ViewGroup.LayoutParams.MATCH_PARENT;

		int limit = 50;

		if(mPostCountLimit > 0 && limit > mPostCountLimit) {
			limit = mPostCountLimit;
		}

		final DownloadStrategy downloadStrategy;

		if(forceDownload) {
			downloadStrategy = DownloadStrategyAlways.INSTANCE;

		} else if(session == null && savedInstanceState == null && General.isNetworkConnected(context)) {

			final long maxAgeMs = PrefsUtility.pref_cache_rerequest_postlist_age_ms(context, mSharedPreferences);
			downloadStrategy = new DownloadStrategyIfTimestampOutsideBounds(TimestampBound.notOlderThan(maxAgeMs));

		} else {
			downloadStrategy = DownloadStrategyIfNotCached.INSTANCE;
		}

		mRequest = new PostListingRequest(
				mPostListingURL.generateJsonUri(),
				RedditAccountManager.getInstance(context).getDefaultAccount(),
				session,
				downloadStrategy,
				true);

		// The request doesn't go ahead until the header is in place.

		switch(mPostListingURL.pathType()) {

			case RedditURLParser.SEARCH_POST_LISTING_URL:
				setHeader(new SearchListingHeader(getActivity(), (SearchPostListURL) mPostListingURL));
				CacheManager.getInstance(context).makeRequest(mRequest);
				break;

			case RedditURLParser.USER_POST_LISTING_URL:
			case RedditURLParser.MULTIREDDIT_POST_LISTING_URL:
				setHeader(mPostListingURL.humanReadableName(getActivity(), true), mPostListingURL.humanReadableUrl());
				CacheManager.getInstance(context).makeRequest(mRequest);
				break;

			case RedditURLParser.SUBREDDIT_POST_LISTING_URL:

				SubredditPostListURL subredditPostListURL
						= (SubredditPostListURL)mPostListingURL;

				switch(subredditPostListURL.type) {

					case FRONTPAGE:
					case ALL:
					case SUBREDDIT_COMBINATION:
					case ALL_SUBTRACTION:
					case POPULAR:
					case SUBSCRIBED:
						setHeader(mPostListingURL.humanReadableName(getActivity(), true), mPostListingURL.humanReadableUrl());
						CacheManager.getInstance(context).makeRequest(mRequest);
						break;

					case RANDOM:
					case SUBREDDIT: {

						// Request the subreddit data

						final RequestResponseHandler<RedditSubreddit, SubredditRequestFailure> subredditHandler
								= new RequestResponseHandler<RedditSubreddit, SubredditRequestFailure>() {
							@Override
							public void onRequestFailed(SubredditRequestFailure failureReason) {
								// Ignore
								AndroidCommon.UI_THREAD_HANDLER.post(new Runnable() {
									@Override
									public void run() {
										CacheManager.getInstance(context).makeRequest(mRequest);
									}
								});
							}

							@Override
							public void onRequestSuccess(final RedditSubreddit result, final long timeCached) {
								AndroidCommon.UI_THREAD_HANDLER.post(new Runnable() {
									@Override
									public void run() {
										mSubreddit = result;
										onSubredditReceived();
										CacheManager.getInstance(context).makeRequest(mRequest);
									}
								});
							}
						};

						try {
							RedditSubredditManager
									.getInstance(getActivity(), RedditAccountManager.getInstance(getActivity()).getDefaultAccount())
									.getSubreddit(RedditSubreddit.getCanonicalName(subredditPostListURL.subreddit), TimestampBound.NONE, subredditHandler, null);
						} catch(RedditSubreddit.InvalidSubredditNameException e) {
							throw new RuntimeException(e);
						}
						break;
					}
				}

				break;
		}
	}

	private LinearLayout createVerticalLinearLayout(Context context) {
		final LinearLayout result = new LinearLayout(context);
		result.setOrientation(LinearLayout.VERTICAL);
		return result;
	}

	@Override
	public View getView() {
		return mOuter;
	}

	@Override
	public Bundle onSaveInstanceState() {

		final Bundle bundle = new Bundle();

		final LinearLayoutManager layoutManager = (LinearLayoutManager)mRecyclerView.getLayoutManager();
		bundle.putInt(SAVEDSTATE_FIRST_VISIBLE_POS, layoutManager.findFirstVisibleItemPosition());

		return bundle;
	}

	public void cancel() {
		if(mRequest != null) mRequest.cancel();
	}

	public synchronized void restackRefreshCount() {
		while(mPostRefreshCount.get() <= 0) {
			mPostRefreshCount.addAndGet(mPostCountLimit);
		}
	}

	private void onSubredditReceived() {

		if (mPostListingURL.pathType() == RedditURLParser.SUBREDDIT_POST_LISTING_URL
				&& mPostListingURL.asSubredditPostListURL().type == SubredditPostListURL.Type.RANDOM) {
			try {
				mPostListingURL = mPostListingURL.asSubredditPostListURL().changeSubreddit(RedditSubreddit.stripRPrefix(mSubreddit.url));
				mRequest = new PostListingRequest(
						mPostListingURL.generateJsonUri(),
						RedditAccountManager.getInstance(getContext()).getDefaultAccount(),
						mSession,
						mRequest.downloadStrategy,
						true);
			} catch (RedditSubreddit.InvalidSubredditNameException e) {
				throw new RuntimeException(e);
			}
		}
		final String subtitle;

		if(mPostListingURL.getOrder() == null || mPostListingURL.getOrder() == PostSort.HOT) {
			if(mSubreddit.subscribers == null) {
				subtitle = getString(R.string.header_subscriber_count_unknown);
			} else {
				subtitle = getContext().getString(R.string.header_subscriber_count,
					NumberFormat.getNumberInstance(Locale.getDefault()).format(mSubreddit.subscribers));
			}

		} else {
			subtitle = mPostListingURL.humanReadableUrl();
		}

		getActivity().runOnUiThread(new Runnable() {
			@Override
			public void run() {
				setHeader(StringEscapeUtils.unescapeHtml4(mSubreddit.title), subtitle);
				getActivity().invalidateOptionsMenu();
			}
		});

	}

	private void setHeader(final String title, final String subtitle) {
        final PostListingHeader postListingHeader = new PostListingHeader(getActivity(), title, subtitle);
        setHeader(postListingHeader);
	}

	private void setHeader(final View view) {
		getActivity().runOnUiThread(new Runnable() {
			@Override
			public void run() {
				mPostListingManager.addPostListingHeader(view);
			}
		});
	}


	public void onPostSelected(final RedditPreparedPost post) {
		((RedditPostView.PostSelectionListener)getActivity()).onPostSelected(post);

		new Thread() {
			@Override
			public void run() {
				post.markAsRead(getActivity());
			}
		}.start();
	}

	public void onPostCommentsSelected(final RedditPreparedPost post) {

		((RedditPostView.PostSelectionListener)getActivity()).onPostCommentsSelected(post);

		new Thread() {
			@Override
			public void run() {
				post.markAsRead(getActivity());
			}
		}.start();
	}

	private void onLoadMoreItemsCheck() {

		General.checkThisIsUIThread();

		if(mReadyToDownloadMore && mAfter != null && !mAfter.equals(mLastAfter)) {

			final LinearLayoutManager layoutManager = (LinearLayoutManager)mRecyclerView.getLayoutManager();

			if((layoutManager.getItemCount() - layoutManager.findLastVisibleItemPosition() < 20
					&& (mPostCountLimit <= 0 || mPostRefreshCount.get() > 0)
					|| (mPreviousFirstVisibleItemPosition != null
							&& layoutManager.getItemCount() <= mPreviousFirstVisibleItemPosition))) {

				mLastAfter = mAfter;
				mReadyToDownloadMore = false;

				final Uri newUri = mPostListingURL.after(mAfter).generateJsonUri();

				// TODO customise (currently 3 hrs)
				final DownloadStrategy strategy = (RRTime.since(mTimestamp) < 3 * 60 * 60 * 1000)
						? DownloadStrategyIfNotCached.INSTANCE
						: DownloadStrategyNever.INSTANCE;

				int limit = 50;

				if(mPostCountLimit > 0 && limit > mPostRefreshCount.get()) {
					limit = mPostRefreshCount.get();
				}

				mRequest = new PostListingRequest(newUri, RedditAccountManager.getInstance(getActivity()).getDefaultAccount(), mSession, strategy, false);
				mPostListingManager.setLoadingVisible(true);
				CacheManager.getInstance(getActivity()).makeRequest(mRequest);

			} else if(mPostCountLimit > 0 && mPostRefreshCount.get() <= 0) {

				if(mLoadMoreView == null) {

					mLoadMoreView = (TextView)LayoutInflater.from(getContext()).inflate(R.layout.load_more_posts, null);
					mLoadMoreView.setOnClickListener(new View.OnClickListener() {
						@Override
						public void onClick(View view) {
							mPostListingManager.removeLoadMoreButton();
							mLoadMoreView = null;
							restackRefreshCount();
							onLoadMoreItemsCheck();
						}
					});

					mPostListingManager.addLoadMoreButton(mLoadMoreView);
				}
			}
		}
	}

	public void onSubscribe() {

		if(mPostListingURL.pathType() != RedditURLParser.SUBREDDIT_POST_LISTING_URL) return;

		try {
			RedditSubredditSubscriptionManager
					.getSingleton(getActivity(), RedditAccountManager.getInstance(getActivity()).getDefaultAccount())
					.subscribe(RedditSubreddit.getCanonicalName(mPostListingURL.asSubredditPostListURL().subreddit), getActivity());
		} catch(RedditSubreddit.InvalidSubredditNameException e) {
			throw new RuntimeException(e);
		}
	}

	public void onUnsubscribe() {

		if(mSubreddit == null) return;

		try {
			RedditSubredditSubscriptionManager
					.getSingleton(getActivity(), RedditAccountManager.getInstance(getActivity()).getDefaultAccount())
					.unsubscribe(mSubreddit.getCanonicalName(), getActivity());
		} catch(RedditSubreddit.InvalidSubredditNameException e) {
			throw new RuntimeException(e);
		}
	}

	public RedditSubreddit getSubreddit() {
		return mSubreddit;
	}

	private static Uri setUriDownloadCount(final Uri input, final int count) {
		return input.buildUpon().appendQueryParameter("limit", String.valueOf(count)).build();
	}

	public void onPostsAdded() {

		if(mPreviousFirstVisibleItemPosition == null) {
			return;
		}

		final LinearLayoutManager layoutManager = (LinearLayoutManager)mRecyclerView.getLayoutManager();

		if(layoutManager.getItemCount() > mPreviousFirstVisibleItemPosition) {
			layoutManager.scrollToPositionWithOffset(mPreviousFirstVisibleItemPosition, 0);
			mPreviousFirstVisibleItemPosition = null;

		} else {
			layoutManager.scrollToPosition(layoutManager.getItemCount() - 1);
		}
	}

	private class PostListingRequest extends CacheRequest {

		private final boolean firstDownload;

		protected PostListingRequest(Uri url, RedditAccount user, UUID requestSession, DownloadStrategy downloadStrategy, boolean firstDownload) {
			super(General.uriFromString(url.toString()), user, requestSession, Constants.Priority.API_POST_LIST, 0, downloadStrategy, Constants.FileType.POST_LIST, DOWNLOAD_QUEUE_REDDIT_API, true, false, getActivity());
			this.firstDownload = firstDownload;
		}
		@Override
		protected void onDownloadNecessary() {}

		@Override
		protected void onDownloadStarted() {}

		@Override
		protected void onCallbackException(final Throwable t) {
			BugReportActivity.handleGlobalError(context, t);
		}

		@Override
		protected void onFailure(final @CacheRequest.RequestFailureType int type, final Throwable t, final Integer status, final String readableMessage) {

			AndroidCommon.UI_THREAD_HANDLER.post(new Runnable() {
				@Override
				public void run() {

					mPostListingManager.setLoadingVisible(false);

					final RRError error;

					if(type == CacheRequest.REQUEST_FAILURE_CACHE_MISS) {
						error = new RRError(
								context.getString(R.string.error_postlist_cache_title),
								context.getString(R.string.error_postlist_cache_message),
								t,
								status,
								url.toString());

					} else {
						error = General.getGeneralErrorForFailure(context, type, t, status, url.toString());
					}

					mPostListingManager.addFooterError(new ErrorView(getActivity(), error));
				}
			});
		}

		@Override protected void onProgress(final boolean authorizationInProgress, final long bytesRead, final long totalBytes) {}

		@Override protected void onSuccess(final CacheManager.ReadableCacheFile cacheFile, final long timestamp, final UUID session, final boolean fromCache, final String mimetype) {}

		@Override
		public void onJsonParseStarted(final JsonValue value, final long timestamp, final UUID session, final boolean fromCache) {

			final AppCompatActivity activity = getActivity();

			// TODO pref (currently 10 mins)
			if(firstDownload && fromCache && RRTime.since(timestamp) > 10 * 60 * 1000) {
				AndroidCommon.UI_THREAD_HANDLER.post(new Runnable() {
					@Override
					public void run() {

						final TextView cacheNotif = (TextView)LayoutInflater.from(getActivity())
								.inflate(R.layout.cached_header, null, false);

						cacheNotif.setText(getActivity().getString(
								R.string.listing_cached,
								RRTime.formatDateTime(timestamp, getActivity())));

						mPostListingManager.addNotification(cacheNotif);
					}
				});
			} // TODO resuming a copy

			if(firstDownload) {
				((SessionChangeListener)activity).onSessionChanged(session, SessionChangeListener.SessionChangeType.POSTS, timestamp);
				PostListingFragment.this.mSession = session;
				PostListingFragment.this.mTimestamp = timestamp;
			}

			// TODO {"error": 403} is received for unauthorized subreddits

			try {

				final JsonBufferedObject thing = value.asObject();
				final JsonBufferedObject listing = thing.getObject("data");
				final JsonBufferedArray posts = listing.getArray("children");

				final boolean isNsfwAllowed = PrefsUtility.pref_behaviour_nsfw(activity, mSharedPreferences);
				final boolean hideReadPosts = PrefsUtility.pref_behaviour_hide_read_posts(activity, mSharedPreferences);
				final boolean isConnectionWifi = General.isConnectionWifi(activity);

				final PrefsUtility.AppearanceThumbnailsShow thumbnailsPref = PrefsUtility.appearance_thumbnails_show(
						activity, mSharedPreferences);
				final boolean downloadThumbnails = thumbnailsPref == PrefsUtility.AppearanceThumbnailsShow.ALWAYS
						|| (thumbnailsPref == PrefsUtility.AppearanceThumbnailsShow.WIFIONLY && isConnectionWifi);

				final boolean showNsfwThumbnails = PrefsUtility.appearance_thumbnails_nsfw_show(activity, mSharedPreferences);

				final PrefsUtility.CachePrecacheImages imagePrecachePref
						= PrefsUtility.cache_precache_images(activity, mSharedPreferences);

				final PrefsUtility.CachePrecacheComments commentPrecachePref
						= PrefsUtility.cache_precache_comments(activity, mSharedPreferences);

				final boolean precacheImages = (imagePrecachePref == PrefsUtility.CachePrecacheImages.ALWAYS
						|| (imagePrecachePref == PrefsUtility.CachePrecacheImages.WIFIONLY && isConnectionWifi))
						&& !General.isCacheDiskFull(activity);

				final boolean precacheComments = (commentPrecachePref == PrefsUtility.CachePrecacheComments.ALWAYS
						|| (commentPrecachePref == PrefsUtility.CachePrecacheComments.WIFIONLY && isConnectionWifi));

				final PrefsUtility.ImageViewMode imageViewMode
						= PrefsUtility.pref_behaviour_imageview_mode(activity, mSharedPreferences);

				final PrefsUtility.GifViewMode gifViewMode
						= PrefsUtility.pref_behaviour_gifview_mode(activity, mSharedPreferences);

				final PrefsUtility.VideoViewMode videoViewMode
						= PrefsUtility.pref_behaviour_videoview_mode(activity, mSharedPreferences);

				final boolean leftHandedMode
						= PrefsUtility.pref_appearance_left_handed(activity, mSharedPreferences);

				final boolean subredditFilteringEnabled =
						mPostListingURL.pathType() == RedditURLParser.SUBREDDIT_POST_LISTING_URL
						&& (mPostListingURL.asSubredditPostListURL().type == SubredditPostListURL.Type.ALL
								|| mPostListingURL.asSubredditPostListURL().type == SubredditPostListURL.Type.ALL_SUBTRACTION
								|| mPostListingURL.asSubredditPostListURL().type == SubredditPostListURL.Type.POPULAR);

				final List<String> blockedSubreddits = PrefsUtility.pref_blocked_subreddits(activity, mSharedPreferences); // Grab this so we don't have to pull from the prefs every post

				Log.i(TAG, "Precaching images: " + (precacheImages ? "ON" : "OFF"));
				Log.i(TAG, "Precaching comments: " + (precacheComments ? "ON" : "OFF"));

				final CacheManager cm = CacheManager.getInstance(activity);

				final boolean showSubredditName
						= !(mPostListingURL != null
						&& mPostListingURL.pathType() == RedditURLParser.SUBREDDIT_POST_LISTING_URL
						&& mPostListingURL.asSubredditPostListURL().type == SubredditPostListURL.Type.SUBREDDIT);

				final ArrayList<RedditPostListItem> downloadedPosts = new ArrayList<>(25);

				for(final JsonValue postThingValue : posts) {

					final RedditThing postThing = postThingValue.asObject(RedditThing.class);

					if(!postThing.getKind().equals(RedditThing.Kind.POST)) continue;

					final RedditPost post = postThing.asPost();

					mAfter = post.name;

					final boolean isPostBlocked = subredditFilteringEnabled && getIsPostBlocked(blockedSubreddits, post);

					if(!isPostBlocked
							&& (!post.over_18 || isNsfwAllowed)
							&& mPostIds.add(post.getIdAlone())) {

						final boolean downloadThisThumbnail = downloadThumbnails && (!post.over_18 || showNsfwThumbnails);

						final int positionInList = mPostCount;

						final RedditParsedPost parsedPost = new RedditParsedPost(post, false);

						final RedditPreparedPost preparedPost = new RedditPreparedPost(
								activity,
								cm,
								positionInList,
								parsedPost,
								timestamp,
								showSubredditName,
								downloadThisThumbnail);

						// Skip adding this post (go to next iteration) if it has been clicked on AND user preference
						// "hideReadPosts" is true
						if(hideReadPosts && preparedPost.isRead()) continue;

						if(precacheComments) {

							final CommentListingController controller = new CommentListingController(
									PostCommentListingURL.forPostId(preparedPost.src.getIdAlone()),
									activity);

							CacheManager.getInstance(activity).makeRequest(new CacheRequest(
									General.uriFromString(controller.getUri().toString()),
									RedditAccountManager.getInstance(activity).getDefaultAccount(),
									null,
									Constants.Priority.COMMENT_PRECACHE,
									positionInList,
									DownloadStrategyIfNotCached.INSTANCE,
									Constants.FileType.COMMENT_LIST,
									DOWNLOAD_QUEUE_REDDIT_API,
									false, // Don't parse the JSON
									false,
									activity) {

								@Override
								protected void onCallbackException(final Throwable t) {}

								@Override
								protected void onDownloadNecessary() {}

								@Override
								protected void onDownloadStarted() {}

								@Override
								protected void onFailure(final @CacheRequest.RequestFailureType int type, final Throwable t, final Integer status, final String readableMessage) {
									Log.e(TAG, "Failed to precache " + url.toString() + "(RequestFailureType code: " + type + ")");
								}

								@Override
								protected void onProgress(final boolean authorizationInProgress, final long bytesRead, final long totalBytes) {}

								@Override
								protected void onSuccess(final CacheManager.ReadableCacheFile cacheFile, final long timestamp, final UUID session, final boolean fromCache, final String mimetype) {
									Log.i(TAG, "Successfully precached " + url.toString());
								}
							});
						}

						LinkHandler.getImageInfo(activity, parsedPost.getUrl(), Constants.Priority.IMAGE_PRECACHE, positionInList, new GetImageInfoListener() {

							@Override public void onFailure(final @CacheRequest.RequestFailureType int type, final Throwable t, final Integer status, final String readableMessage) {}
							@Override public void onNotAnImage() {}

							@Override
							public void onSuccess(final ImageInfo info) {

								if(!precacheImages) return;

								// Don't precache huge images
								if(info.size != null && info.size > 15 * 1024 * 1024) {
									Log.i(TAG, String.format(
											"Not precaching '%s': too big (%d kB)", post.getUrl(), info.size / 1024));
									return;
								}

								// Don't precache gifs if they're opened externally
								if(ImageInfo.MediaType.GIF.equals(info.mediaType)
										&& !gifViewMode.downloadInApp) {

									Log.i(TAG, String.format(
											"Not precaching '%s': GIFs are opened externally", post.getUrl()));
									return;
								}

								// Don't precache images if they're opened externally
								if(ImageInfo.MediaType.IMAGE.equals(info.mediaType)
										&& !imageViewMode.downloadInApp) {

									Log.i(TAG, String.format(
											"Not precaching '%s': images are opened externally", post.getUrl()));
									return;
								}


								// Don't precache videos if they're opened externally
								if(ImageInfo.MediaType.VIDEO.equals(info.mediaType)
										&& !videoViewMode.downloadInApp) {

									Log.i(TAG, String.format(
											"Not precaching '%s': videos are opened externally", post.getUrl()));
									return;
								}

								precacheImage(activity, info.urlOriginal, positionInList);

								if(info.urlAudioStream != null) {
									precacheImage(activity, info.urlAudioStream, positionInList);
								}
							}
						});

						downloadedPosts.add(new RedditPostListItem(
								preparedPost,
								PostListingFragment.this,
								activity,
								leftHandedMode));

						mPostCount++;
						mPostRefreshCount.decrementAndGet();
					}
				}

				AndroidCommon.UI_THREAD_HANDLER.post(new Runnable() {
					@Override
					public void run() {

						mPostListingManager.addPosts(downloadedPosts);
						mPostListingManager.setLoadingVisible(false);
						onPostsAdded();

						mRequest = null;
						mReadyToDownloadMore = true;
						onLoadMoreItemsCheck();
					}
				});

			} catch (Throwable t) {
				notifyFailure(CacheRequest.REQUEST_FAILURE_PARSE, t, null, "Parse failure");
			}
		}
	}

	private boolean getIsPostBlocked(
			@NonNull final List<String> blockedSubreddits,
			@NonNull final RedditPost post) throws RedditSubreddit.InvalidSubredditNameException {

		final String canonicalName = RedditSubreddit.getCanonicalName(post.subreddit);

		for (String blockedSubredditName : blockedSubreddits) {
			if (blockedSubredditName.equalsIgnoreCase(canonicalName)) {
				return true;
			}
		}

		return false;
	}

	private void precacheImage(
			final Activity activity,
			final String url,
			final int positionInList) {

		final URI uri = General.uriFromString(url);
		if(uri == null) {
			Log.i(TAG, String.format(
					"Not precaching '%s': failed to parse URL", url));
			return;
		}

		CacheManager.getInstance(activity).makeRequest(new CacheRequest(
				uri,
				RedditAccountManager.getAnon(),
				null,
				Constants.Priority.IMAGE_PRECACHE,
				positionInList,
				DownloadStrategyIfNotCached.INSTANCE,
				Constants.FileType.IMAGE,
				CacheRequest.DOWNLOAD_QUEUE_IMAGE_PRECACHE,
				false,
				false,
				activity
		) {
			@Override
			protected void onCallbackException(final Throwable t) {
			}

			@Override
			protected void onDownloadNecessary() {
			}

			@Override
			protected void onDownloadStarted() {
			}

			@Override
			protected void onFailure(final @CacheRequest.RequestFailureType int type, final Throwable t, final Integer status, final String readableMessage) {

				Log.e(TAG, String.format(
						Locale.US,
						"Failed to precache %s (RequestFailureType %d, status %s, readable '%s')",
						url,
						type,
						status == null ? "NULL" : status.toString(),
						readableMessage == null ? "NULL" : readableMessage));
			}

			@Override
			protected void onProgress(final boolean authorizationInProgress, final long bytesRead, final long totalBytes) {
			}

			@Override
			protected void onSuccess(final CacheManager.ReadableCacheFile cacheFile, final long timestamp, final UUID session, final boolean fromCache, final String mimetype) {
				Log.i(TAG, "Successfully precached " + url);
			}
		});
	}
}
