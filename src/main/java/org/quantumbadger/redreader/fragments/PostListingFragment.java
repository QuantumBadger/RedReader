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

package org.quantumbadger.redreader.fragments;

import android.content.Context;
import android.content.SharedPreferences;
import android.net.Uri;
import android.os.Bundle;
import android.os.Handler;
import android.os.Looper;
import android.os.Message;
import android.preference.PreferenceManager;
import android.support.v7.app.AppCompatActivity;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AbsListView;
import android.widget.FrameLayout;
import android.widget.LinearLayout;
import android.widget.ListView;
import android.widget.TextView;
import android.widget.Toast;
import org.apache.commons.lang3.StringEscapeUtils;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.activities.BugReportActivity;
import org.quantumbadger.redreader.activities.SessionChangeListener;
import org.quantumbadger.redreader.adapters.PostListingAdapter;
import org.quantumbadger.redreader.cache.CacheManager;
import org.quantumbadger.redreader.cache.CacheRequest;
import org.quantumbadger.redreader.common.AndroidApi;
import org.quantumbadger.redreader.common.Constants;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.LinkHandler;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.common.RRError;
import org.quantumbadger.redreader.common.RRTime;
import org.quantumbadger.redreader.common.TimestampBound;
import org.quantumbadger.redreader.image.GetImageInfoListener;
import org.quantumbadger.redreader.image.ImageInfo;
import org.quantumbadger.redreader.io.RequestResponseHandler;
import org.quantumbadger.redreader.jsonwrap.JsonBufferedArray;
import org.quantumbadger.redreader.jsonwrap.JsonBufferedObject;
import org.quantumbadger.redreader.jsonwrap.JsonValue;
import org.quantumbadger.redreader.listingcontrollers.CommentListingController;
import org.quantumbadger.redreader.listingcontrollers.PostListingController;
import org.quantumbadger.redreader.reddit.RedditSubredditManager;
import org.quantumbadger.redreader.reddit.api.RedditSubredditSubscriptionManager;
import org.quantumbadger.redreader.reddit.api.SubredditRequestFailure;
import org.quantumbadger.redreader.reddit.prepared.RedditParsedPost;
import org.quantumbadger.redreader.reddit.prepared.RedditPreparedPost;
import org.quantumbadger.redreader.reddit.things.RedditPost;
import org.quantumbadger.redreader.reddit.things.RedditSubreddit;
import org.quantumbadger.redreader.reddit.things.RedditThing;
import org.quantumbadger.redreader.reddit.url.PostCommentListingURL;
import org.quantumbadger.redreader.reddit.url.PostListingURL;
import org.quantumbadger.redreader.reddit.url.RedditURLParser;
import org.quantumbadger.redreader.reddit.url.SubredditPostListURL;
import org.quantumbadger.redreader.views.LoadingSpinnerView;
import org.quantumbadger.redreader.views.PostListingHeader;
import org.quantumbadger.redreader.views.RedditPostView;
import org.quantumbadger.redreader.views.list.ListOverlayView;
import org.quantumbadger.redreader.views.liststatus.ErrorView;

import java.net.URI;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicInteger;

public class PostListingFragment extends RRFragment
		implements RedditPostView.PostSelectionListener,
		AbsListView.OnScrollListener {

	private static final String TAG = "PostListingFragment";

	private static final String SAVEDSTATE_FIRST_VISIBLE_POS = "firstVisiblePosition";

	private final PostListingURL mPostListingURL;

	private RedditSubreddit mSubreddit;

	private UUID mSession;
	private int mPostCountLimit;
	private final PostListingAdapter mAdapter;
	private final ListView mListView;
	private TextView mLoadMoreView;

	private final SharedPreferences mSharedPreferences;

	private final LinearLayout mFragmentHeader, mListHeader, mListHeaderNotifications, mListFooterNotifications;

	private final FrameLayout mOverlayContainer;

	private String mAfter = null, mLastAfter = null;
	private CacheRequest mRequest;
	private boolean mReadyToDownloadMore = false;
	private long mTimestamp;

	private final LoadingSpinnerView mLoadingView;

	private int mPostCount = 0;
	private final AtomicInteger mPostRefreshCount = new AtomicInteger(0);

	private Integer mPreviousFirstVisibleItemPosition;

	private static final int
			NOTIF_AGE = 0,
			NOTIF_ERROR = 1,
			NOTIF_ERROR_FOOTER = 2,
			NOTIF_SHOW_LOADING_SPINNER = 3,
			NOTIF_HIDE_LOADING_SPINNER = 4;

	private final Handler mNotificationHandler = new Handler(Looper.getMainLooper()) {
		@Override
		public void handleMessage(final Message msg) {

			// TODO check if attached? if not, queue, and send on "resume"
			switch(msg.what) {

				case NOTIF_AGE: {
					final TextView cacheNotif = (TextView) LayoutInflater.from(getActivity())
						.inflate(R.layout.cached_header, mListHeaderNotifications, false);
					cacheNotif.setText(getActivity().getString(R.string.listing_cached, RRTime.formatDateTime((Long) msg.obj, getActivity())));

					mListHeaderNotifications.addView(cacheNotif);
					mListHeaderNotifications.requestLayout();
					mAdapter.notifyDataSetChanged();
					break;
				}

				case NOTIF_ERROR: {
					final RRError error = (RRError)msg.obj;
					mFragmentHeader.addView(new ErrorView(getActivity(), error));
					break;
				}

				case NOTIF_ERROR_FOOTER: {
					final RRError error = (RRError)msg.obj;
					mListFooterNotifications.addView(new ErrorView(getActivity(), error));
					mAdapter.notifyDataSetChanged();
					break;
				}

				case NOTIF_HIDE_LOADING_SPINNER: {
					mLoadingView.setVisibility(View.GONE);
					break;
				}

				case NOTIF_SHOW_LOADING_SPINNER: {
					mLoadingView.setVisibility(View.VISIBLE);
					break;
				}
			}
		}
	};

	// Session may be null
	public PostListingFragment(
			final AppCompatActivity parent,
			final Bundle savedInstanceState,
			final Uri url,
			final UUID session,
			final @CacheRequest.DownloadType int downloadType) {

		super(parent, savedInstanceState);

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

		this.mSession = session;

		final Context context = getContext();
		mSharedPreferences = PreferenceManager.getDefaultSharedPreferences(context);

		mOverlayContainer = new FrameLayout(context) {
			@Override
			protected void onAttachedToWindow() {
				super.onAttachedToWindow();
				getLayoutParams().height = ViewGroup.LayoutParams.MATCH_PARENT;
			}
		};

		final LinearLayout outer = new LinearLayout(context);
		outer.setOrientation(android.widget.LinearLayout.VERTICAL);
		mOverlayContainer.addView(outer);

		mLoadingView = new LoadingSpinnerView(context);

		mFragmentHeader = createVerticalLinearLayout(context);

		// TODO output failed URL
		if(mPostListingURL == null) {
			mFragmentHeader.addView(new ErrorView(getActivity(), new RRError("Invalid post listing URL", "Could not navigate to that URL.")));
			// TODO proper error handling
			throw new RuntimeException("Invalid post listing URL");
		}

		mListHeader = createVerticalLinearLayout(context);
		mListHeaderNotifications = createVerticalLinearLayout(context);
		mListFooterNotifications = createVerticalLinearLayout(context);

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
		}

		if(mPostCountLimit > 0) {
			restackRefreshCount();
			mLoadMoreView = (TextView)LayoutInflater.from(context).inflate(R.layout.load_more_posts, null);
			mLoadMoreView.setOnClickListener(new View.OnClickListener() {
				@Override
				public void onClick(View view) {
					mListFooterNotifications.removeView(mLoadMoreView);
					restackRefreshCount();
					onLoadMoreItemsCheck();
				}
			});
		}

		mListHeader.addView(mListHeaderNotifications);

		mListView = (ListView)getActivity().getLayoutInflater().inflate(R.layout.reddit_post_list, null);
		mListView.setOnScrollListener(this);
		mListView.addHeaderView(mListHeader);
		mListView.addFooterView(mListFooterNotifications, null, true);

		mListFooterNotifications.addView(mLoadingView);
		mLoadingView.getLayoutParams().width = ViewGroup.LayoutParams.MATCH_PARENT;
		mLoadingView.getLayoutParams().height = General.dpToPixels(context, 200);

		mListView.setPersistentDrawingCache(ViewGroup.PERSISTENT_ALL_CACHES);
		mListView.setAlwaysDrawnWithCacheEnabled(true);

		mAdapter = new PostListingAdapter(mListView, this, getActivity());
		mListView.setAdapter(mAdapter);

		final ListOverlayView lov = new ListOverlayView(context, mListView);

		outer.addView(mFragmentHeader);
		outer.addView(lov);

		mListView.getLayoutParams().height = ViewGroup.LayoutParams.MATCH_PARENT;

		int limit = 50;

		if(mPostCountLimit > 0 && limit > mPostCountLimit) {
			limit = mPostCountLimit;
		}

		mRequest = new PostListingRequest(
				mPostListingURL.generateJsonUri(),
				RedditAccountManager.getInstance(context).getDefaultAccount(),
				session,
				downloadType,
				true);

		CacheManager.getInstance(context).makeRequest(mRequest);

		switch(mPostListingURL.pathType()) {

			case RedditURLParser.USER_POST_LISTING_URL:
			case RedditURLParser.SEARCH_POST_LISTING_URL:
				setHeader(mPostListingURL.humanReadableName(getActivity(), true), mPostListingURL.humanReadableUrl());
				break;

			case RedditURLParser.SUBREDDIT_POST_LISTING_URL:

				SubredditPostListURL subredditPostListURL
						= (SubredditPostListURL)mPostListingURL;

				switch(subredditPostListURL.type) {

					case FRONTPAGE:
					case ALL:
					case SUBREDDIT_COMBINATION:
					case ALL_SUBTRACTION:
						setHeader(mPostListingURL.humanReadableName(getActivity(), true), mPostListingURL.humanReadableUrl());
						break;

					case SUBREDDIT: {

						// Request the subreddit data

						final RequestResponseHandler<RedditSubreddit, SubredditRequestFailure> subredditHandler
								= new RequestResponseHandler<RedditSubreddit, SubredditRequestFailure>() {
							@Override
							public void onRequestFailed(SubredditRequestFailure failureReason) {
								// Ignore
							}

							@Override
							public void onRequestSuccess(RedditSubreddit result, long timeCached) {
								mSubreddit = result;
								onSubredditReceived();
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
		return mOverlayContainer;
	}

	@Override
	public Bundle onSaveInstanceState() {

		final Bundle bundle = new Bundle();

		bundle.putInt(SAVEDSTATE_FIRST_VISIBLE_POS, mListView.getFirstVisiblePosition());

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

		final String subtitle;

		if(mPostListingURL.getOrder() == null || mPostListingURL.getOrder() == PostListingController.Sort.HOT) {
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
		getActivity().runOnUiThread(new Runnable() {
			@Override
			public void run() {
				final PostListingHeader postListingHeader = new PostListingHeader(getActivity(), title, subtitle);
				mListHeader.addView(postListingHeader, 0);
				mAdapter.notifyDataSetChanged();
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

	public void onScrollStateChanged(AbsListView view, int scrollState) {}

	public void onScroll(AbsListView view, int firstVisibleItem, int visibleItemCount, int totalItemCount) {
		onLoadMoreItemsCheck();
	}

	private synchronized void onLoadMoreItemsCheck() {

		if(mReadyToDownloadMore && mAfter != null && !mAfter.equals(mLastAfter)) {

			if(mAdapter.getDownloadedCount() > 0
					&& (mAdapter.getDownloadedCount() - mListView.getLastVisiblePosition() < 20
					&& (mPostCountLimit <= 0 || mPostRefreshCount.get() > 0)
					|| (mPreviousFirstVisibleItemPosition != null
							&& mListView.getAdapter().getCount() <= mPreviousFirstVisibleItemPosition))) {

				mLastAfter = mAfter;
				mReadyToDownloadMore = false;

				final Uri newUri = mPostListingURL.after(mAfter).generateJsonUri();

				// TODO customise (currently 3 hrs)
				@CacheRequest.DownloadType int type = (RRTime.since(mTimestamp) < 3 * 60 * 60 * 1000) ? CacheRequest.DOWNLOAD_IF_NECESSARY : CacheRequest.DOWNLOAD_NEVER;

				int limit = 50;

				if(mPostCountLimit > 0 && limit > mPostRefreshCount.get()) {
					limit = mPostRefreshCount.get();
				}

				mRequest = new PostListingRequest(newUri, RedditAccountManager.getInstance(getActivity()).getDefaultAccount(), mSession, type, false);
				mNotificationHandler.sendEmptyMessage(NOTIF_SHOW_LOADING_SPINNER);
				CacheManager.getInstance(getActivity()).makeRequest(mRequest);

			} else if(mPostCountLimit > 0 && mPostRefreshCount.get() <= 0) {
				AndroidApi.UI_THREAD_HANDLER.post(new Runnable() {
					@Override
					public void run() {
						if(mLoadMoreView.getParent() == null) {
							mListFooterNotifications.addView(mLoadMoreView);
						}
					}
				});
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

		if(mListView.getAdapter().getCount() > mPreviousFirstVisibleItemPosition) {

			mListView.smoothScrollToPositionFromTop(
					mPreviousFirstVisibleItemPosition,
					0,
					100);

			mPreviousFirstVisibleItemPosition = null;

		} else {
			mListView.smoothScrollToPositionFromTop(
					mListView.getAdapter().getCount() - 1,
					0,
					100);
		}
	}

	private class PostListingRequest extends CacheRequest {

		private final boolean firstDownload;

		protected PostListingRequest(Uri url, RedditAccount user, UUID requestSession, @DownloadType int downloadType, boolean firstDownload) {
			super(General.uriFromString(url.toString()), user, requestSession, Constants.Priority.API_POST_LIST, 0, downloadType, Constants.FileType.POST_LIST, DOWNLOAD_QUEUE_REDDIT_API, true, false, getActivity());
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

			mNotificationHandler.sendEmptyMessage(NOTIF_HIDE_LOADING_SPINNER);

			if(type == CacheRequest.REQUEST_FAILURE_CACHE_MISS) {

				final RRError error = new RRError(
						context.getString(R.string.error_postlist_cache_title),
						context.getString(R.string.error_postlist_cache_message),
						t,
						status,
						url.toString());

				mNotificationHandler.sendMessage(General.handlerMessage(NOTIF_ERROR_FOOTER, error));

			} else {
				final RRError error = General.getGeneralErrorForFailure(context, type, t, status, url.toString());
				mNotificationHandler.sendMessage(General.handlerMessage(NOTIF_ERROR, error));
			}
		}

		@Override protected void onProgress(final boolean authorizationInProgress, final long bytesRead, final long totalBytes) {}

		@Override protected void onSuccess(final CacheManager.ReadableCacheFile cacheFile, final long timestamp, final UUID session, final boolean fromCache, final String mimetype) {}

		@Override
		public void onJsonParseStarted(final JsonValue value, final long timestamp, final UUID session, final boolean fromCache) {

			// TODO pref (currently 10 mins)
			if(firstDownload && fromCache && RRTime.since(timestamp) > 10 * 60 * 1000) {
				mNotificationHandler.sendMessage(General.handlerMessage(NOTIF_AGE, timestamp));
			} // TODO resuming a copy

			if(firstDownload) {
				((SessionChangeListener)getActivity()).onSessionChanged(session, SessionChangeListener.SessionChangeType.POSTS, timestamp);
				PostListingFragment.this.mSession = session;
				PostListingFragment.this.mTimestamp = timestamp;
			}

			// TODO {"error": 403} is received for unauthorized subreddits

			try {

				final Context context = getActivity();
				final JsonBufferedObject thing = value.asObject();
				final JsonBufferedObject listing = thing.getObject("data");
				final JsonBufferedArray posts = listing.getArray("children");

				final boolean isNsfwAllowed = PrefsUtility.pref_behaviour_nsfw(context, mSharedPreferences);
				final boolean isConnectionWifi = General.isConnectionWifi(context);

				final PrefsUtility.AppearanceThumbnailsShow thumbnailsPref = PrefsUtility.appearance_thumbnails_show(context, mSharedPreferences);
				final boolean downloadThumbnails = thumbnailsPref == PrefsUtility.AppearanceThumbnailsShow.ALWAYS
						|| (thumbnailsPref == PrefsUtility.AppearanceThumbnailsShow.WIFIONLY && isConnectionWifi);

				final boolean showNsfwThumbnails = PrefsUtility.appearance_thumbnails_nsfw_show(context, mSharedPreferences);

				final PrefsUtility.CachePrecacheImages imagePrecachePref
						= PrefsUtility.cache_precache_images(context, mSharedPreferences);

				final PrefsUtility.CachePrecacheComments commentPrecachePref
						= PrefsUtility.cache_precache_comments(context, mSharedPreferences);

				final boolean precacheImages = (imagePrecachePref == PrefsUtility.CachePrecacheImages.ALWAYS
						|| (imagePrecachePref == PrefsUtility.CachePrecacheImages.WIFIONLY && isConnectionWifi))
						&& !General.isCacheDiskFull(context);

				final boolean precacheComments = (commentPrecachePref == PrefsUtility.CachePrecacheComments.ALWAYS
						|| (commentPrecachePref == PrefsUtility.CachePrecacheComments.WIFIONLY && isConnectionWifi));

				final PrefsUtility.ImageViewMode imageViewMode
						= PrefsUtility.pref_behaviour_imageview_mode(context, mSharedPreferences);

				final PrefsUtility.GifViewMode gifViewMode
						= PrefsUtility.pref_behaviour_gifview_mode(context, mSharedPreferences);

				final PrefsUtility.VideoViewMode videoViewMode
						= PrefsUtility.pref_behaviour_videoview_mode(context, mSharedPreferences);

				final boolean imagesOpenedInternally = (imageViewMode == PrefsUtility.ImageViewMode.INTERNAL_OPENGL);

				final boolean gifsOpenedInternally
						= (gifViewMode == PrefsUtility.GifViewMode.INTERNAL_MOVIE
								|| gifViewMode == PrefsUtility.GifViewMode.INTERNAL_LEGACY
								|| videoViewMode == PrefsUtility.VideoViewMode.INTERNAL_VIDEOVIEW);

				final boolean isAll =
						mPostListingURL.pathType() == RedditURLParser.SUBREDDIT_POST_LISTING_URL
						&& (mPostListingURL.asSubredditPostListURL().type == SubredditPostListURL.Type.ALL
								|| mPostListingURL.asSubredditPostListURL().type == SubredditPostListURL.Type.ALL_SUBTRACTION);

				final List<String> blockedSubreddits = PrefsUtility.pref_blocked_subreddits(context, mSharedPreferences); // Grab this so we don't have to pull from the prefs every post

				Log.i(TAG, "Precaching images: " + (precacheImages ? "ON" : "OFF"));
				Log.i(TAG, "Precaching comments: " + (precacheComments ? "ON" : "OFF"));

				final CacheManager cm = CacheManager.getInstance(context);

				final boolean showSubredditName
						= !(mPostListingURL != null
						&& mPostListingURL.pathType() == RedditURLParser.SUBREDDIT_POST_LISTING_URL
						&& mPostListingURL.asSubredditPostListURL().type == SubredditPostListURL.Type.SUBREDDIT);

				final ArrayList<RedditPreparedPost> downloadedPosts = new ArrayList<>(25);

				for(final JsonValue postThingValue : posts) {

					final RedditThing postThing = postThingValue.asObject(RedditThing.class);

					if(!postThing.getKind().equals(RedditThing.Kind.POST)) continue;

					final RedditPost post = postThing.asPost();

					mAfter = post.name;

					Boolean isPostBlocked = getIsPostBlocked(isAll, blockedSubreddits, post);

					if(!isPostBlocked && (!post.over_18 || isNsfwAllowed)) {

						final boolean downloadThisThumbnail = downloadThumbnails && (!post.over_18 || showNsfwThumbnails);

						final int positionInList = mPostCount;

						final RedditParsedPost parsedPost = new RedditParsedPost(post, false);

						final RedditPreparedPost preparedPost = new RedditPreparedPost(
								context,
								cm,
								positionInList,
								parsedPost,
								timestamp,
								showSubredditName,
								downloadThisThumbnail);

						if(precacheComments) {

							final CommentListingController controller = new CommentListingController(
									PostCommentListingURL.forPostId(preparedPost.src.getIdAlone()),
									context);

							CacheManager.getInstance(context).makeRequest(new CacheRequest(
									General.uriFromString(controller.getUri().toString()),
									RedditAccountManager.getInstance(context).getDefaultAccount(),
									null,
									Constants.Priority.COMMENT_PRECACHE,
									positionInList,
									DOWNLOAD_IF_NECESSARY,
									Constants.FileType.COMMENT_LIST,
									DOWNLOAD_QUEUE_REDDIT_API,
									false, // Don't parse the JSON
									false,
									context) {

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

						LinkHandler.getImageInfo(context, post.url, Constants.Priority.IMAGE_PRECACHE, positionInList, new GetImageInfoListener() {

							@Override public void onFailure(final @CacheRequest.RequestFailureType int type, final Throwable t, final Integer status, final String readableMessage) {}
							@Override public void onNotAnImage() {}

							@Override
							public void onSuccess(final ImageInfo info) {

								if(!precacheImages) return;

								// Don't precache huge images
								if(info.width != null && info.width > 2500) {
									Log.i(TAG, String.format(
											"Not precaching '%s': too wide (%d px)", post.url, info.width));
									return;
								}

								if(info.height != null && info.height > 2500) {
									Log.i(TAG, String.format(
											"Not precaching '%s': too tall (%d px)", post.url, info.height));
									return;
								}

								if(info.size != null && info.size > 10 * 1024 * 1024) {
									Log.i(TAG, String.format(
											"Not precaching '%s': too big (%d kB)", post.url, info.size / 1024));
									return;
								}

								// Don't precache gifs if they're opened externally
								if(Boolean.TRUE.equals(info.isAnimated) && !gifsOpenedInternally) {
									Log.i(TAG, String.format(
											"Not precaching '%s': GIFs are opened externally", post.url));
									return;
								}

								// Don't precache images if they're opened externally
								if(!Boolean.TRUE.equals(info.isAnimated) && !imagesOpenedInternally) {
									Log.i(TAG, String.format(
											"Not precaching '%s': images are opened externally", post.url));
									return;
								}

								final URI uri = General.uriFromString(info.urlOriginal);
								if(uri == null) return;

								CacheManager.getInstance(context).makeRequest(new CacheRequest(
										uri,
										RedditAccountManager.getAnon(),
										null,
										Constants.Priority.IMAGE_PRECACHE,
										positionInList,
										DOWNLOAD_IF_NECESSARY,
										Constants.FileType.IMAGE,
										DOWNLOAD_QUEUE_IMAGE_PRECACHE,
										false,
										false,
										context
								) {
									@Override protected void onCallbackException(final Throwable t) {}
									@Override protected void onDownloadNecessary() {}
									@Override protected void onDownloadStarted() {}

									@Override protected void onFailure(final @CacheRequest.RequestFailureType int type, final Throwable t, final Integer status, final String readableMessage) {
										Log.e(TAG, "Failed to precache " + info.urlOriginal + "(RequestFailureType code: " + type + ")");
									}
									@Override protected void onProgress(final boolean authorizationInProgress, final long bytesRead, final long totalBytes) {}

									@Override protected void onSuccess(final CacheManager.ReadableCacheFile cacheFile, final long timestamp, final UUID session, final boolean fromCache, final String mimetype) {
										Log.i(TAG, "Successfully precached " + info.urlOriginal);
									}
								});
							}
						});

						downloadedPosts.add(preparedPost);
					}

					mPostCount++;
					mPostRefreshCount.decrementAndGet();
				}

				mAdapter.onPostsDownloaded(downloadedPosts);

				mNotificationHandler.sendEmptyMessage(NOTIF_HIDE_LOADING_SPINNER);

				mRequest = null;
				mReadyToDownloadMore = true;
				onLoadMoreItemsCheck();

			} catch (Throwable t) {
				notifyFailure(CacheRequest.REQUEST_FAILURE_PARSE, t, null, "Parse failure");
			}
		}
	}

	private Boolean getIsPostBlocked(boolean isAll, List<String> blockedSubreddits, RedditPost post) throws RedditSubreddit.InvalidSubredditNameException {
		Boolean isPostBlocked = false;

		if (isAll) {
			for (String blockedSubredditName : blockedSubreddits) {
				if (blockedSubredditName.equalsIgnoreCase(RedditSubreddit.getCanonicalName(post.subreddit))) {
					isPostBlocked = true;
				}
			}
		}
		return isPostBlocked;
	}
}
