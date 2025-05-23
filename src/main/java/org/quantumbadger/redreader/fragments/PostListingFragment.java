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

import android.app.Activity;
import android.content.Context;
import android.net.Uri;
import android.os.Bundle;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.TextView;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.annotation.StringRes;
import androidx.appcompat.app.AppCompatActivity;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import org.apache.commons.text.StringEscapeUtils;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.activities.BaseActivity;
import org.quantumbadger.redreader.activities.BugReportActivity;
import org.quantumbadger.redreader.activities.OptionsMenuUtility;
import org.quantumbadger.redreader.activities.SessionChangeListener;
import org.quantumbadger.redreader.adapters.MainMenuListingManager;
import org.quantumbadger.redreader.adapters.PostListingManager;
import org.quantumbadger.redreader.cache.CacheManager;
import org.quantumbadger.redreader.cache.CacheRequest;
import org.quantumbadger.redreader.cache.CacheRequestCallbacks;
import org.quantumbadger.redreader.cache.downloadstrategy.DownloadStrategy;
import org.quantumbadger.redreader.cache.downloadstrategy.DownloadStrategyAlways;
import org.quantumbadger.redreader.cache.downloadstrategy.DownloadStrategyIfNotCached;
import org.quantumbadger.redreader.cache.downloadstrategy.DownloadStrategyIfTimestampOutsideBounds;
import org.quantumbadger.redreader.cache.downloadstrategy.DownloadStrategyNever;
import org.quantumbadger.redreader.common.AndroidCommon;
import org.quantumbadger.redreader.common.Constants;
import org.quantumbadger.redreader.common.FileUtils;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.GenericFactory;
import org.quantumbadger.redreader.common.LinkHandler;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.common.Priority;
import org.quantumbadger.redreader.common.RRError;
import org.quantumbadger.redreader.common.TimestampBound;
import org.quantumbadger.redreader.common.UriString;
import org.quantumbadger.redreader.common.datastream.SeekableInputStream;
import org.quantumbadger.redreader.common.time.TimeDuration;
import org.quantumbadger.redreader.common.time.TimestampUTC;
import org.quantumbadger.redreader.http.FailedRequestBody;
import org.quantumbadger.redreader.image.GetImageInfoListener;
import org.quantumbadger.redreader.image.ImageInfo;
import org.quantumbadger.redreader.io.RequestResponseHandler;
import org.quantumbadger.redreader.listingcontrollers.CommentListingController;
import org.quantumbadger.redreader.reddit.PostSort;
import org.quantumbadger.redreader.reddit.RedditPostListItem;
import org.quantumbadger.redreader.reddit.RedditSubredditManager;
import org.quantumbadger.redreader.reddit.api.RedditSubredditSubscriptionManager;
import org.quantumbadger.redreader.reddit.kthings.JsonUtils;
import org.quantumbadger.redreader.reddit.kthings.MaybeParseError;
import org.quantumbadger.redreader.reddit.kthings.RedditIdAndType;
import org.quantumbadger.redreader.reddit.kthings.RedditListing;
import org.quantumbadger.redreader.reddit.kthings.RedditPost;
import org.quantumbadger.redreader.reddit.kthings.RedditThing;
import org.quantumbadger.redreader.reddit.prepared.RedditParsedPost;
import org.quantumbadger.redreader.reddit.prepared.RedditPreparedPost;
import org.quantumbadger.redreader.reddit.things.InvalidSubredditNameException;
import org.quantumbadger.redreader.reddit.things.RedditSubreddit;
import org.quantumbadger.redreader.reddit.things.SubredditCanonicalId;
import org.quantumbadger.redreader.reddit.url.PostCommentListingURL;
import org.quantumbadger.redreader.reddit.url.PostListingURL;
import org.quantumbadger.redreader.reddit.url.RedditURLParser;
import org.quantumbadger.redreader.reddit.url.SearchPostListURL;
import org.quantumbadger.redreader.reddit.url.SubredditPostListURL;
import org.quantumbadger.redreader.views.PostListingHeader;
import org.quantumbadger.redreader.views.RedditPostView;
import org.quantumbadger.redreader.views.ScrollbarRecyclerViewManager;
import org.quantumbadger.redreader.views.SearchListingHeader;
import org.quantumbadger.redreader.views.liststatus.ErrorView;

import java.io.IOException;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Locale;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicInteger;

public class PostListingFragment extends RRFragment
		implements RedditPostView.PostSelectionListener {

	private static final String TAG = "PostListingFragment";

	private static final String SAVEDSTATE_FIRST_VISIBLE_POS = "firstVisiblePosition";

	@NonNull private PostListingURL mPostListingURL;

	@Nullable private RedditSubreddit mSubreddit;

	private UUID mSession;
	private final int mPostCountLimit;
	private TextView mLoadMoreView;

	private final PostListingManager mPostListingManager;
	private final RecyclerView mRecyclerView;

	private final View mOuter;

	private RedditIdAndType mAfter = null;
	private RedditIdAndType mLastAfter = null;
	private CacheRequest mRequest;
	private boolean mReadyToDownloadMore = false;
	private TimestampUTC mTimestamp;

	private int mPostCount = 0;
	private boolean mPostsNotShown = false;
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
			mPreviousFirstVisibleItemPosition = savedInstanceState.getInt(
					SAVEDSTATE_FIRST_VISIBLE_POS);
		}

		try {
			mPostListingURL
					= (PostListingURL)RedditURLParser.parseProbablePostListing(url);
		} catch(final ClassCastException e) {
			Toast.makeText(getActivity(), "Invalid post listing URL.", Toast.LENGTH_LONG)
					.show();
			// TODO proper error handling -- show error view
			throw new RuntimeException(e);
		}

		mSession = session;

		final Context context = getContext();

		// TODO output failed URL
		if(mPostListingURL == null) {
			mPostListingManager.addFooterError(
					new ErrorView(
							getActivity(),
							new RRError(
									"Invalid post listing URL",
									"Could not navigate to that URL.",
									true,
									new RuntimeException(),
									null,
									UriString.from(url),
									null)));
			// TODO proper error handling
			throw new RuntimeException("Invalid post listing URL");
		}

		switch(PrefsUtility.pref_behaviour_post_count()) {
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
				&& PrefsUtility.pref_behaviour_enable_swipe_refresh()) {

			recyclerViewManager.enablePullToRefresh(
					((OptionsMenuUtility.OptionsMenuPostsListener)parent)::onRefreshPosts);
		}

		mRecyclerView = recyclerViewManager.getRecyclerView();
		mPostListingManager.setLayoutManager((LinearLayoutManager)mRecyclerView.getLayoutManager());

		mRecyclerView.setAdapter(mPostListingManager.getAdapter());

		mOuter = recyclerViewManager.getOuterView();

		mRecyclerView.addOnScrollListener(new RecyclerView.OnScrollListener() {
			@Override
			public void onScrolled(
					@NonNull final RecyclerView recyclerView,
					final int dx,
					final int dy) {
				onLoadMoreItemsCheck();
			}
		});

		General.setLayoutMatchParent(mRecyclerView);

		final DownloadStrategy downloadStrategy;

		if(forceDownload) {
			downloadStrategy = DownloadStrategyAlways.INSTANCE;

		} else if(session == null
				&& savedInstanceState == null
				&& General.isNetworkConnected(context)) {

			final TimeDuration maxAge
					= PrefsUtility.pref_cache_rerequest_postlist_age();
			downloadStrategy = new DownloadStrategyIfTimestampOutsideBounds(
					TimestampBound.notOlderThan(maxAge));

		} else {
			downloadStrategy = DownloadStrategyIfNotCached.INSTANCE;
		}

		mRequest = createPostListingRequest(
				UriString.from(mPostListingURL.generateJsonUri()),
				RedditAccountManager.getInstance(context).getDefaultAccount(),
				session,
				downloadStrategy,
				true);

		// The request doesn't go ahead until the header is in place.

		switch(mPostListingURL.pathType()) {

			case RedditURLParser.SEARCH_POST_LISTING_URL:
				setHeader(new SearchListingHeader(
						getActivity(),
						(SearchPostListURL)mPostListingURL));
				CacheManager.getInstance(context).makeRequest(mRequest);
				break;

			case RedditURLParser.USER_POST_LISTING_URL:
			case RedditURLParser.MULTIREDDIT_POST_LISTING_URL:
				setHeader(
						mPostListingURL.humanReadableName(getActivity(), true),
						mPostListingURL.humanReadableUrl(),
						null);
				CacheManager.getInstance(context).makeRequest(mRequest);
				break;

			case RedditURLParser.SUBREDDIT_POST_LISTING_URL:

				final SubredditPostListURL subredditPostListURL
						= (SubredditPostListURL)mPostListingURL;

				switch(subredditPostListURL.type) {

					case FRONTPAGE:
					case ALL:
					case SUBREDDIT_COMBINATION:
					case ALL_SUBTRACTION:
					case POPULAR:
						setHeader(
								mPostListingURL.humanReadableName(getActivity(), true),
								mPostListingURL.humanReadableUrl(),
								null);
						CacheManager.getInstance(context).makeRequest(mRequest);
						break;

					case SUBREDDIT: {

						// Request the subreddit data

						final RequestResponseHandler<RedditSubreddit, RRError>
								subredditHandler = new RequestResponseHandler<
								RedditSubreddit,
								RRError>() {
							@Override
							public void onRequestFailed(
									final RRError failureReason) {
								// Ignore
								AndroidCommon.UI_THREAD_HANDLER.post(() ->
										CacheManager.getInstance(context).makeRequest(mRequest));
							}

							@Override
							public void onRequestSuccess(
									final RedditSubreddit result,
									final TimestampUTC timeCached) {
								AndroidCommon.UI_THREAD_HANDLER.post(() -> {
									mSubreddit = result;

									if(mSubreddit.over18
											&& !PrefsUtility.pref_behaviour_nsfw()) {
										mPostListingManager.setLoadingVisible(false);

										final int title
												= R.string.error_nsfw_subreddits_disabled_title;

										final int message
												= R.string.error_nsfw_subreddits_disabled_message;

										mPostListingManager.addFooterError(new ErrorView(
												getActivity(),
												new RRError(
														context.getString(title),
														context.getString(message),
														false)));
									} else {
										onSubredditReceived();
										CacheManager.getInstance(context)
												.makeRequest(mRequest);
									}
								});
							}
						};

						try {
							RedditSubredditManager
									.getInstance(
											getActivity(),
											RedditAccountManager.getInstance(getActivity())
													.getDefaultAccount())
									.getSubreddit(
											new SubredditCanonicalId(
													subredditPostListURL.subreddit),
											TimestampBound.NONE,
											subredditHandler,
											null);
						} catch(final InvalidSubredditNameException e) {
							throw new RuntimeException(e);
						}
						break;
					}
				}

				break;

			case RedditURLParser.POST_COMMENT_LISTING_URL:
			case RedditURLParser.UNKNOWN_COMMENT_LISTING_URL:
			case RedditURLParser.UNKNOWN_POST_LISTING_URL:
			case RedditURLParser.USER_COMMENT_LISTING_URL:
			case RedditURLParser.USER_PROFILE_URL:
			case RedditURLParser.COMPOSE_MESSAGE_URL:
			case RedditURLParser.OPAQUE_SHARED_URL:
				BugReportActivity.handleGlobalError(getActivity(), new RuntimeException(
						"Unknown url type "
								+ mPostListingURL.pathType()
								+ ": "
								+ mPostListingURL.toString()));
		}
	}

	@Override
	public View getListingView() {
		return mOuter;
	}

	@Override
	public Bundle onSaveInstanceState() {

		final Bundle bundle = new Bundle();

		final LinearLayoutManager layoutManager
				= (LinearLayoutManager)mRecyclerView.getLayoutManager();
		bundle.putInt(
				SAVEDSTATE_FIRST_VISIBLE_POS,
				layoutManager.findFirstVisibleItemPosition());

		return bundle;
	}

	public void cancel() {
		if(mRequest != null) {
			mRequest.cancel();
		}
	}

	public synchronized void restackRefreshCount() {
		while(mPostRefreshCount.get() <= 0) {
			mPostRefreshCount.addAndGet(mPostCountLimit);
		}
	}

	private void onSubredditReceived() {
		final String subtitle;

		if(mPostListingURL.getOrder() == null
				|| mPostListingURL.getOrder() == PostSort.HOT) {
			if(mSubreddit.subscribers == null) {
				subtitle = getString(R.string.header_subscriber_count_unknown);
			} else {
				subtitle = getContext().getString(
						R.string.header_subscriber_count,
						NumberFormat.getNumberInstance(Locale.getDefault())
								.format(mSubreddit.subscribers));
			}

		} else {
			subtitle = mPostListingURL.humanReadableUrl();
		}

		getActivity().runOnUiThread(() -> {
			setHeader(
					StringEscapeUtils.unescapeHtml4(mSubreddit.title),
					subtitle,
					mSubreddit);
			getActivity().invalidateOptionsMenu();
		});

	}

	private void setHeader(
			@NonNull final String title,
			@NonNull final String subtitle,
			@Nullable final RedditSubreddit subreddit) {

		final PostListingHeader postListingHeader = new PostListingHeader(
				getActivity(),
				title,
				subtitle,
				mPostListingURL,
				subreddit);

		setHeader(postListingHeader);

		if(subreddit != null) {
			postListingHeader.setOnLongClickListener(view -> {
				try {
					MainMenuListingManager.showActionMenu(
							getActivity(),
							subreddit.getCanonicalId());
				} catch (final InvalidSubredditNameException e) {
					throw new RuntimeException(e);
				}
				return true;
			});
		}
	}

	private void setHeader(final View view) {
		getActivity().runOnUiThread(() -> mPostListingManager.addPostListingHeader(view));
	}


	@Override
	public void onPostSelected(final RedditPreparedPost post) {
		((RedditPostView.PostSelectionListener)getActivity()).onPostSelected(post);

		new Thread() {
			@Override
			public void run() {
				post.markAsRead(getActivity());
			}
		}.start();
	}

	@Override
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

			final LinearLayoutManager layoutManager
					= (LinearLayoutManager)mRecyclerView.getLayoutManager();

			if((layoutManager.getItemCount() - layoutManager.findLastVisibleItemPosition()
					< 20
					&& (mPostCountLimit <= 0 || mPostRefreshCount.get() > 0)
					|| (mPreviousFirstVisibleItemPosition != null
					&& layoutManager.getItemCount()
					<= mPreviousFirstVisibleItemPosition))) {

				mLastAfter = mAfter;
				mReadyToDownloadMore = false;

				final Uri newUri = mPostListingURL.after(mAfter).generateJsonUri();

				// TODO customise (currently 3 hrs)
				final DownloadStrategy strategy = mTimestamp.elapsed()
								.isLessThan(TimeDuration.hours(3))
						? DownloadStrategyIfNotCached.INSTANCE
						: DownloadStrategyNever.INSTANCE;

				mRequest = createPostListingRequest(
						UriString.from(newUri),
						RedditAccountManager.getInstance(getActivity())
								.getDefaultAccount(),
						mSession,
						strategy,
						false);
				mPostListingManager.setLoadingVisible(true);
				CacheManager.getInstance(getActivity()).makeRequest(mRequest);

			} else if(mPostCountLimit > 0 && mPostRefreshCount.get() <= 0) {

				if(mLoadMoreView == null) {

					mLoadMoreView = (TextView)LayoutInflater.from(getContext())
							.inflate(R.layout.load_more_posts, null);
					mLoadMoreView.setOnClickListener(view -> {
						mPostListingManager.removeLoadMoreButton();
						mLoadMoreView = null;
						restackRefreshCount();
						onLoadMoreItemsCheck();
					});

					mPostListingManager.addLoadMoreButton(mLoadMoreView);
				}
			}
		}
	}

	public void onSubscribe() {

		if(mPostListingURL.pathType() != RedditURLParser.SUBREDDIT_POST_LISTING_URL) {
			return;
		}

		try {
			RedditSubredditSubscriptionManager
					.getSingleton(
							getActivity(),
							RedditAccountManager.getInstance(getActivity())
									.getDefaultAccount())
					.subscribe(
							new SubredditCanonicalId(
									mPostListingURL.asSubredditPostListURL().subreddit),
							getActivity());
		} catch(final InvalidSubredditNameException e) {
			throw new RuntimeException(e);
		}
	}

	public void onUnsubscribe() {

		if(mSubreddit == null) {
			return;
		}

		try {
			RedditSubredditSubscriptionManager
					.getSingleton(
							getActivity(),
							RedditAccountManager.getInstance(getActivity())
									.getDefaultAccount())
					.unsubscribe(mSubreddit.getCanonicalId(), getActivity());
		} catch(final InvalidSubredditNameException e) {
			throw new RuntimeException(e);
		}
	}

	@NonNull
	public PostListingURL getPostListingURL() {
		return mPostListingURL;
	}

	@Nullable
	public RedditSubreddit getSubreddit() {
		return mSubreddit;
	}

	public void onPostsAdded() {

		if(mPreviousFirstVisibleItemPosition == null) {
			return;
		}

		final LinearLayoutManager layoutManager
				= (LinearLayoutManager)mRecyclerView.getLayoutManager();

		if(layoutManager.getItemCount() > mPreviousFirstVisibleItemPosition) {
			layoutManager.scrollToPositionWithOffset(
					mPreviousFirstVisibleItemPosition,
					0);
			mPreviousFirstVisibleItemPosition = null;

		} else {
			layoutManager.scrollToPosition(layoutManager.getItemCount() - 1);
		}
	}

	@NonNull
	private CacheRequest createPostListingRequest(
				final UriString url,
				final RedditAccount user,
				final UUID requestSession,
				final DownloadStrategy downloadStrategy,
				final boolean firstDownload) {

		final AppCompatActivity activity = getActivity();

		return new CacheRequest(
				url,
				user,
				requestSession,
				new Priority(Constants.Priority.API_POST_LIST),
				downloadStrategy,
				Constants.FileType.POST_LIST,
				CacheRequest.DownloadQueueType.REDDIT_API,
				activity,
				new CacheRequestCallbacks() {
					@Override
					public void onDataStreamComplete(
							@NonNull final GenericFactory<SeekableInputStream, IOException>
									streamFactory,
							final TimestampUTC timestamp,
							@NonNull final UUID session,
							final boolean fromCache,
							@Nullable final String mimetype) {

						final BaseActivity activity = (BaseActivity)getActivity();

						// One hour (matches default refresh value)
						if(firstDownload && fromCache) {
							if (timestamp.elapsedPeriod()
									.asDuration().isGreaterThan(TimeDuration.hours(1))) {
								AndroidCommon.UI_THREAD_HANDLER.post(() -> {

									final TextView cacheNotif
											= (TextView) LayoutInflater.from(activity).inflate(
											R.layout.cached_header,
											null,
											false);

									cacheNotif.setText(getActivity().getString(
											R.string.listing_cached,
											timestamp.format()));

									mPostListingManager.addNotification(cacheNotif);
								});
							} // TODO resuming a copy
						}

						if(firstDownload) {
							((SessionChangeListener)activity).onSessionChanged(
									session,
									SessionChangeListener.SessionChangeType.POSTS,
									timestamp);
							PostListingFragment.this.mSession = session;
							PostListingFragment.this.mTimestamp = timestamp;
						}

						// TODO {"error": 403} is received for unauthorized subreddits

						try {

							final RedditThing thing = JsonUtils.INSTANCE
									.decodeRedditThingFromStream(streamFactory.create());

							if(!(thing instanceof RedditThing.Listing)) {
								throw new RuntimeException("Expected listing, got "
										+ thing.getClass().getName());
							}

							final RedditListing listing = ((RedditThing.Listing)thing).getData();

							final ArrayList<MaybeParseError<RedditThing>> posts
									= listing.getChildren();

							final boolean isNsfwAllowed = PrefsUtility.pref_behaviour_nsfw();

							final boolean hideReadPosts
									= PrefsUtility.pref_behaviour_hide_read_posts()
									&& mPostListingURL.pathType()
									!= RedditURLParser.USER_POST_LISTING_URL;

							final boolean isConnectionWifi = General.isConnectionWifi(activity);

							final boolean inlinePreviews
									= PrefsUtility.images_inline_image_previews()
									.isEnabled(isConnectionWifi);

							final boolean showNsfwPreviews
									= PrefsUtility.images_inline_image_previews_nsfw();

							final boolean showSpoilerPreviews
									= PrefsUtility.images_inline_image_previews_spoiler();

							final boolean downloadThumbnails
									= PrefsUtility.appearance_thumbnails_show()
									.isEnabled(isConnectionWifi);

							final boolean allowHighResThumbnails = downloadThumbnails
									&& PrefsUtility.images_high_res_thumbnails()
									.isEnabled(isConnectionWifi);

							final boolean showNsfwThumbnails
									= PrefsUtility.appearance_thumbnails_nsfw_show();

							final boolean showSpoilerThumbnails
									= PrefsUtility.appearance_thumbnails_spoiler_show();

							final boolean precacheImages
									= !inlinePreviews
									&& PrefsUtility.cache_precache_images()
									.isEnabled(isConnectionWifi)
									&& !FileUtils.isCacheDiskFull(activity);

							final boolean precacheComments = PrefsUtility.cache_precache_comments()
									.isEnabled(isConnectionWifi);

							final PrefsUtility.ImageViewMode imageViewMode
									= PrefsUtility.pref_behaviour_imageview_mode();

							final PrefsUtility.GifViewMode gifViewMode
									= PrefsUtility.pref_behaviour_gifview_mode();

							final PrefsUtility.VideoViewMode videoViewMode
									= PrefsUtility.pref_behaviour_videoview_mode();

							final boolean leftHandedMode
									= PrefsUtility.pref_appearance_left_handed();

							final boolean subredditFilteringEnabled =
									mPostListingURL.pathType()
											== RedditURLParser.SUBREDDIT_POST_LISTING_URL
											&& (mPostListingURL.asSubredditPostListURL().type
											== SubredditPostListURL.Type.ALL
											|| mPostListingURL.asSubredditPostListURL().type
											== SubredditPostListURL.Type.ALL_SUBTRACTION
											|| mPostListingURL.asSubredditPostListURL().type
											== SubredditPostListURL.Type.POPULAR
											|| mPostListingURL.asSubredditPostListURL().type
											== SubredditPostListURL.Type.FRONTPAGE);

							// Grab this so we don't have to pull from the prefs every post
							final HashSet<SubredditCanonicalId> blockedSubreddits
									= new HashSet<>(PrefsUtility.pref_blocked_subreddits());

							Log.i(TAG, "Inline previews: "
									+ (inlinePreviews ? "ON" : "OFF"));

							Log.i(TAG, "Precaching images: "
									+ (precacheImages ? "ON" : "OFF"));

							Log.i(TAG, "Precaching comments: "
									+ (precacheComments ? "ON" : "OFF"));

							final CacheManager cm = CacheManager.getInstance(activity);

							final boolean showSubredditName = !(mPostListingURL != null
									&& mPostListingURL.pathType()
									== RedditURLParser.SUBREDDIT_POST_LISTING_URL
									&& mPostListingURL.asSubredditPostListURL().type
									== SubredditPostListURL.Type.SUBREDDIT);

							final ArrayList<RedditPostListItem> downloadedPosts
									= new ArrayList<>(25);

							for(final MaybeParseError<RedditThing> postThingValue : posts) {

								if(!(postThingValue instanceof MaybeParseError.Ok)) {
									// TODO handle this
									continue;
								}

								final RedditThing postThing
										= ((MaybeParseError.Ok<RedditThing>)postThingValue)
												.getValue();

								if(!(postThing instanceof RedditThing.Post)) {
									continue;
								}

								final RedditPost post = ((RedditThing.Post)postThing).getData();

								mAfter = post.getName();

								final boolean isPostBlocked = subredditFilteringEnabled
										&& blockedSubreddits.contains(
										new SubredditCanonicalId(post.getSubreddit().getDecoded()));

								if(!isPostBlocked
										&& (!post.getOver_18() || isNsfwAllowed)
										&& mPostIds.add(post.getIdAlone())) {

									final boolean downloadThisThumbnail = downloadThumbnails
											&& (!post.getOver_18() || showNsfwThumbnails)
											&& (!post.getSpoiler() || showSpoilerThumbnails);

									final boolean downloadThisPreview = inlinePreviews
											&& (!post.getOver_18() || showNsfwPreviews)
											&& (!post.getSpoiler() || showSpoilerPreviews);

									final int positionInList = mPostCount;

									final RedditParsedPost parsedPost = new RedditParsedPost(
											activity,
											post,
											false);

									final RedditPreparedPost preparedPost = new RedditPreparedPost(
											activity,
											cm,
											positionInList,
											parsedPost,
											timestamp,
											showSubredditName,
											downloadThisThumbnail,
											allowHighResThumbnails,
											downloadThisPreview);

									// Skip adding this post (go to next iteration) if it
									// has been clicked on AND read posts should be hidden
									if(hideReadPosts && preparedPost.isRead()) {
										mPostsNotShown = true;
										continue;
									}

									if(precacheComments) {
										precacheComments(activity, preparedPost, positionInList);
									}

									LinkHandler.getImageInfo(
											activity,
											parsedPost.getUrl(),
											new Priority(
													Constants.Priority.IMAGE_PRECACHE,
													positionInList),
											new GetImageInfoListener() {

												@Override
												public void onFailure(
														@NonNull final RRError error) {
												}

												@Override
												public void onNotAnImage() {
												}

												@Override
												public void onSuccess(final ImageInfo info) {

													if(!precacheImages) {
														return;
													}

													precacheImage(
															activity,
															info,
															positionInList,
															gifViewMode,
															imageViewMode,
															videoViewMode);
												}
											});

									downloadedPosts.add(new RedditPostListItem(
											preparedPost,
											PostListingFragment.this,
											activity,
											leftHandedMode));

									mPostCount++;
									mPostRefreshCount.decrementAndGet();
								} else {
									mPostsNotShown = true;
								}
							}

							AndroidCommon.runOnUiThread(() -> {

								mPostListingManager.addPosts(downloadedPosts);
								mPostListingManager.setLoadingVisible(false);

								if(mPostCount == 0
										&& (mAfter == null || mAfter.equals(mLastAfter))) {
									@StringRes final int emptyViewText;

									if(mPostsNotShown) {
										if(mPostListingURL.pathType()
												== RedditURLParser.SEARCH_POST_LISTING_URL) {
											emptyViewText = R.string.no_search_results_hidden;
										} else {
											emptyViewText = R.string.no_posts_yet_hidden;
										}
									} else {
										if(mPostListingURL.pathType()
												== RedditURLParser.SEARCH_POST_LISTING_URL) {
											emptyViewText = R.string.no_search_results;
										} else {
											emptyViewText = R.string.no_posts_yet;
										}
									}

									final View emptyView =
											LayoutInflater.from(getContext()).inflate(
													R.layout.no_items_yet,
													mRecyclerView,
													false);

									((TextView)emptyView.findViewById(R.id.empty_view_text))
											.setText(emptyViewText);

									mPostListingManager.addViewToItems(emptyView);
								}

								onPostsAdded();

								mRequest = null;
								mReadyToDownloadMore = true;
								onLoadMoreItemsCheck();
							});

						} catch(final Throwable t) {
							onFailure(General.getGeneralErrorForFailure(
									activity,
									CacheRequest.RequestFailureType.PARSE,
									t,
									null,
									url,
									FailedRequestBody.from(streamFactory)));
						}
					}

					@Override
					public void onFailure(@NonNull final RRError error) {

						AndroidCommon.UI_THREAD_HANDLER.post(() -> {

							mPostListingManager.setLoadingVisible(false);

							mPostListingManager.addFooterError(new ErrorView(
									activity,
									error));
						});
					}
				});
	}

	private void precacheComments(
			final Activity activity,
			@NonNull final RedditPreparedPost preparedPost,
			final int positionInList) {

		final CommentListingController controller = new CommentListingController(
				PostCommentListingURL.forPostId(preparedPost.src.getIdAlone()));

		final UriString url = UriString.from(controller.getUri());

		CacheManager.getInstance(activity)
				.makeRequest(new CacheRequest(
						url,
						RedditAccountManager.getInstance(activity).getDefaultAccount(),
						null,
						new Priority(
								Constants.Priority.COMMENT_PRECACHE,
							positionInList),
						new DownloadStrategyIfTimestampOutsideBounds(
								TimestampBound.notOlderThan(TimeDuration.minutes(15))),
						Constants.FileType.COMMENT_LIST,
						CacheRequest.DownloadQueueType.REDDIT_API,
						// Don't parse the JSON
						activity,
						new CacheRequestCallbacks() {
							@Override
							public void onFailure(@NonNull final RRError error) {

								if(General.isSensitiveDebugLoggingEnabled()) {
									Log.e(
											TAG,
											"Failed to precache "
													+ url
													+ " ("
													+ error
													+ ")");
								}
							}

							@Override
							public void onCacheFileWritten(
									@NonNull final CacheManager.ReadableCacheFile cacheFile,
									final TimestampUTC timestamp,
									@NonNull final UUID session,
									final boolean fromCache,
									@Nullable final String mimetype) {

								// Successfully precached
							}
						}));
	}

	private void precacheImage(
			@NonNull final Activity activity,
			@NonNull final ImageInfo info,
			final int positionInList,
			@NonNull final PrefsUtility.GifViewMode gifViewMode,
			@NonNull final PrefsUtility.ImageViewMode imageViewMode,
			@NonNull final PrefsUtility.VideoViewMode videoViewMode) {

		// Don't precache huge images
		if(info.original.sizeBytes != null
				&& info.original.sizeBytes > 15 * 1024 * 1024) {
			if(General.isSensitiveDebugLoggingEnabled()) {
				Log.i(TAG, String.format(
						"Not precaching '%s': too big (%d kB)",
						info.original.url,
						info.original.sizeBytes / 1024));
			}
			return;
		}

		// Don't precache gifs if they're opened externally
		if(ImageInfo.MediaType.GIF.equals(info.mediaType)
				&& !gifViewMode.downloadInApp) {

			if(General.isSensitiveDebugLoggingEnabled()) {
				Log.i(TAG, String.format(
						"Not precaching '%s': GIFs opened externally",
						info.original.url));
			}
			return;
		}

		// Don't precache images if they're opened externally
		if(ImageInfo.MediaType.IMAGE.equals(info.mediaType)
				&& !imageViewMode.downloadInApp) {

			if(General.isSensitiveDebugLoggingEnabled()) {
				Log.i(TAG, String.format(
						"Not precaching '%s': images opened externally",
						info.original.url));
			}
			return;
		}


		// Don't precache videos if they're opened externally
		if(ImageInfo.MediaType.VIDEO.equals(info.mediaType)
				&& !videoViewMode.downloadInApp) {

			if(General.isSensitiveDebugLoggingEnabled()) {
				Log.i(TAG, String.format(
						"Not precaching '%s': videos opened externally",
						info.original.url));
			}
			return;
		}

		precacheImage(
				activity,
				info.original.url,
				positionInList);

		if(info.urlAudioStream != null) {
			precacheImage(
					activity,
					info.urlAudioStream,
					positionInList);
		}
	}

	private void precacheImage(
			final Activity activity,
			final UriString url,
			final int positionInList) {

		CacheManager.getInstance(activity).makeRequest(new CacheRequest(
				url,
				RedditAccountManager.getAnon(),
				null,
				new Priority(
						Constants.Priority.IMAGE_PRECACHE,
						positionInList),
				DownloadStrategyIfNotCached.INSTANCE,
				Constants.FileType.IMAGE,
				CacheRequest.DownloadQueueType.IMAGE_PRECACHE,
				activity,
				new CacheRequestCallbacks() {
					@Override
					public void onFailure(@NonNull final RRError error) {

						if(General.isSensitiveDebugLoggingEnabled()) {
							Log.e(TAG, String.format(
									Locale.US,
									"Failed to precache %s (%s)",
									url,
									error));
						}
					}

					@Override
					public void onCacheFileWritten(
							@NonNull final CacheManager.ReadableCacheFile cacheFile,
							final TimestampUTC timestamp,
							@NonNull final UUID session,
							final boolean fromCache,
							@Nullable final String mimetype) {

						// Successfully precached
					}
				}));
	}
}
