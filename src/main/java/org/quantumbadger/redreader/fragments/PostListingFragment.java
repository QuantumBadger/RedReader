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
import android.content.SharedPreferences;
import android.net.Uri;
import android.os.Handler;
import android.os.Looper;
import android.os.Message;
import android.preference.PreferenceManager;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.*;
import org.apache.commons.lang3.StringEscapeUtils;
import org.apache.http.StatusLine;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.activities.BugReportActivity;
import org.quantumbadger.redreader.activities.SessionChangeListener;
import org.quantumbadger.redreader.adapters.PostListingAdapter;
import org.quantumbadger.redreader.cache.CacheManager;
import org.quantumbadger.redreader.cache.CacheRequest;
import org.quantumbadger.redreader.cache.RequestFailureType;
import org.quantumbadger.redreader.common.*;
import org.quantumbadger.redreader.image.GetImageInfoListener;
import org.quantumbadger.redreader.image.ImageInfo;
import org.quantumbadger.redreader.io.RequestResponseHandler;
import org.quantumbadger.redreader.jsonwrap.JsonBufferedArray;
import org.quantumbadger.redreader.jsonwrap.JsonBufferedObject;
import org.quantumbadger.redreader.jsonwrap.JsonValue;
import org.quantumbadger.redreader.listingcontrollers.CommentListingController;
import org.quantumbadger.redreader.listingcontrollers.PostListingController;
import org.quantumbadger.redreader.reddit.CommentListingRequest;
import org.quantumbadger.redreader.reddit.RedditSubredditManager;
import org.quantumbadger.redreader.reddit.api.RedditSubredditSubscriptionManager;
import org.quantumbadger.redreader.reddit.api.SubredditRequestFailure;
import org.quantumbadger.redreader.reddit.prepared.RedditChangeDataManager;
import org.quantumbadger.redreader.reddit.prepared.RedditPreparedPost;
import org.quantumbadger.redreader.reddit.things.RedditPost;
import org.quantumbadger.redreader.reddit.things.RedditSubreddit;
import org.quantumbadger.redreader.reddit.things.RedditThing;
import org.quantumbadger.redreader.reddit.url.PostCommentListingURL;
import org.quantumbadger.redreader.reddit.url.PostListingURL;
import org.quantumbadger.redreader.reddit.url.RedditURLParser;
import org.quantumbadger.redreader.reddit.url.SubredditPostListURL;
import org.quantumbadger.redreader.views.CachedHeaderView;
import org.quantumbadger.redreader.views.LoadingSpinnerView;
import org.quantumbadger.redreader.views.PostListingHeader;
import org.quantumbadger.redreader.views.RedditPostView;
import org.quantumbadger.redreader.views.list.ListOverlayView;
import org.quantumbadger.redreader.views.liststatus.ErrorView;

import java.net.URI;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Locale;
import java.util.UUID;

public class PostListingFragment extends RRFragment implements RedditPostView.PostSelectionListener, AbsListView.OnScrollListener {

	private PostListingURL postListingURL;

	private RedditSubreddit subreddit;

	private UUID session = null;
	private CacheRequest.DownloadType downloadType;
	private PrefsUtility.PostCount downloadPostCount;
	private PostListingAdapter adapter;
	private ListView lv;
	private TextView loadMoreView;

	private SharedPreferences sharedPrefs;

	private LinearLayout fragmentHeader, listHeader, listHeaderNotifications, listFooterNotifications;

	private String after = null, lastAfter = null;
	private CacheRequest request;
	private boolean readyToDownloadMore = false;
	private long timestamp;

	private LoadingSpinnerView mLoadingView;

	private int postCount = 0;
	private int postRefreshCount = 0;

	private static final int
			NOTIF_AGE = 0,
			NOTIF_ERROR = 1,
			NOTIF_ERROR_FOOTER = 2,
			NOTIF_SHOW_LOADING_SPINNER = 3,
			NOTIF_HIDE_LOADING_SPINNER = 4;

	private final Activity mActivity;

	private final Handler notificationHandler = new Handler(Looper.getMainLooper()) {
		@Override
		public void handleMessage(final Message msg) {

			final Context context = mActivity;

			if(context == null) {
				Log.e("PLF:notificationHandler", "Context was null");
				return;
			}

			// TODO check if attached? if not, queue, and send on "resume"
			switch(msg.what) {

				case NOTIF_AGE: {
					final CachedHeaderView cacheNotif = new CachedHeaderView(
							context,
							context.getString(R.string.listing_cached) + " " + RRTime.formatDateTime((Long) msg.obj, context),
							null
					);

					listHeaderNotifications.addView(cacheNotif);
					listHeaderNotifications.requestLayout();
					adapter.notifyDataSetChanged();
					break;
				}

				case NOTIF_ERROR: {
					final RRError error = (RRError)msg.obj;
					fragmentHeader.addView(new ErrorView(getActivity(), error));
					break;
				}

				case NOTIF_ERROR_FOOTER: {
					final RRError error = (RRError)msg.obj;
					listFooterNotifications.addView(new ErrorView(getActivity(), error));
					adapter.notifyDataSetChanged();
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
	public PostListingFragment(final Activity parent, final Uri url, final UUID session, final CacheRequest.DownloadType downloadType) {
		super(parent);
		mActivity = parent;

		try {
			postListingURL = (PostListingURL) RedditURLParser.parseProbablePostListing(url);
		} catch(ClassCastException e) {
			Toast.makeText(getActivity(), "Invalid post listing URL.", Toast.LENGTH_LONG).show();
			return;
		}

		this.session = session;
		this.downloadType = downloadType;
	}

	private LinearLayout createVerticalLinearLayout(Context context) {
		final LinearLayout result = new LinearLayout(context);
		result.setOrientation(LinearLayout.VERTICAL);
		return result;
	}

	@Override
	public View onCreateView() {

		final Context context = getContext();
		sharedPrefs = PreferenceManager.getDefaultSharedPreferences(context);

		final FrameLayout overlayContainer = new FrameLayout(context) {
			@Override
			protected void onAttachedToWindow() {
				super.onAttachedToWindow();
				getLayoutParams().height = ViewGroup.LayoutParams.MATCH_PARENT;
			}
		};

		final LinearLayout outer = new LinearLayout(context);
		outer.setOrientation(android.widget.LinearLayout.VERTICAL);
		overlayContainer.addView(outer);

		mLoadingView = new LoadingSpinnerView(context);

		fragmentHeader = createVerticalLinearLayout(context);

		// TODO output failed URL
		if(postListingURL == null) {
			fragmentHeader.addView(new ErrorView(getActivity(), new RRError("Invalid post listing URL", "Could not navigate to that URL.")));
			return outer;
		}

		listHeader = createVerticalLinearLayout(context);
		listHeaderNotifications = createVerticalLinearLayout(context);
		listFooterNotifications = createVerticalLinearLayout(context);

		downloadPostCount = PrefsUtility.pref_behaviour_post_count(context, sharedPrefs);
		restackRefreshCount();
		loadMoreView = (TextView)LayoutInflater.from(context).inflate(R.layout.load_more_posts, null);
		loadMoreView.setOnClickListener(new View.OnClickListener() {
			public void onClick(View view) {
				listFooterNotifications.removeView(loadMoreView);
				restackRefreshCount();
				onLoadMoreItemsCheck();
			}
		});

		listHeader.addView(listHeaderNotifications);

		lv = (ListView)getActivity().getLayoutInflater().inflate(R.layout.reddit_post_list, null);
		lv.setOnScrollListener(this);
		lv.addHeaderView(listHeader);
		lv.addFooterView(listFooterNotifications, null, true);

		listFooterNotifications.addView(mLoadingView);
		mLoadingView.getLayoutParams().width = ViewGroup.LayoutParams.MATCH_PARENT;
		mLoadingView.getLayoutParams().height = General.dpToPixels(context, 200);

		lv.setPersistentDrawingCache(ViewGroup.PERSISTENT_ALL_CACHES);
		lv.setAlwaysDrawnWithCacheEnabled(true);

		adapter = new PostListingAdapter(lv, this, getActivity());
		lv.setAdapter(adapter);

		final ListOverlayView lov = new ListOverlayView(context, lv);

		outer.addView(fragmentHeader);
		outer.addView(lov);

		lv.getLayoutParams().height = ViewGroup.LayoutParams.MATCH_PARENT;

		request = new PostListingRequest(postListingURL.generateJsonUri(), RedditAccountManager.getInstance(context).getDefaultAccount(), session, downloadType, true);

		CacheManager.getInstance(context).makeRequest(request);

		switch(postListingURL.pathType()) {

			case UserPostListingURL:
			case SearchPostListingURL:
				setHeader(postListingURL.humanReadableName(getActivity(), true), postListingURL.humanReadableUrl());
				break;

			case SubredditPostListingURL:

				SubredditPostListURL subredditPostListURL
						= (SubredditPostListURL) postListingURL;

				switch(subredditPostListURL.type) {

					case FRONTPAGE:
					case ALL:
					case SUBREDDIT_COMBINATION:
					case ALL_SUBTRACTION:
						setHeader(postListingURL.humanReadableName(getActivity(), true), postListingURL.humanReadableUrl());
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
								subreddit = result;
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

		return overlayContainer;
	}

	public void cancel() {
		if(request != null) request.cancel();
	}

	public void restackRefreshCount() {
		if(postRefreshCount == 0) {
			switch(downloadPostCount) {
				case R25:
					postRefreshCount = 25;
					break;
				case R50:
					postRefreshCount = 50;
					break;
				case R100:
					postRefreshCount = 100;
				break;
			}
		}
	}

	private void onSubredditReceived() {

		final String subtitle;

		if(postListingURL.getOrder() == null || postListingURL.getOrder() == PostListingController.Sort.HOT) {
			if(subreddit.subscribers == null) {
				subtitle = getString(R.string.header_subscriber_count_unknown);
			} else {
				subtitle = NumberFormat.getNumberInstance(Locale.getDefault()).format(subreddit.subscribers) + " " + getString(R.string.header_subscriber_count);
			}

		} else {
			subtitle = postListingURL.humanReadableUrl();
		}

		getActivity().runOnUiThread(new Runnable() {
			@Override
			public void run() {
				setHeader(StringEscapeUtils.unescapeHtml4(subreddit.title), subtitle);
				getActivity().invalidateOptionsMenu();
			}
		});

	}

	private void setHeader(final String title, final String subtitle) {
		getActivity().runOnUiThread(new Runnable() {
			@Override
			public void run() {
				final PostListingHeader postListingHeader = new PostListingHeader(getActivity(), title, subtitle);
				listHeader.addView(postListingHeader, 0);
				adapter.notifyDataSetChanged();
			}
		});
	}

	public void onPostSelected(final RedditPreparedPost post) {
		((RedditPostView.PostSelectionListener)getActivity()).onPostSelected(post);

		new Thread() {
			public void run() {
				post.markAsRead(getActivity());
			}
		}.start();
	}

	public void onPostCommentsSelected(final RedditPreparedPost post) {
		
		((RedditPostView.PostSelectionListener)getActivity()).onPostCommentsSelected(post);

		new Thread() {
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

		if(readyToDownloadMore
				&& after != null
				&& !after.equals(lastAfter)
				&& adapter.getDownloadedCount() > 0
				&& adapter.getDownloadedCount() - lv.getLastVisiblePosition() < 20
				&& (downloadPostCount == PrefsUtility.PostCount.ALL || postRefreshCount > 0)) {

			lastAfter = after;
			readyToDownloadMore = false;

			final Uri newUri = postListingURL.after(after).generateJsonUri();

			// TODO customise (currently 3 hrs)
			CacheRequest.DownloadType type = (RRTime.since(timestamp) < 3 * 60 * 60 * 1000) ? CacheRequest.DownloadType.IF_NECESSARY : CacheRequest.DownloadType.NEVER;

			request = new PostListingRequest(newUri, RedditAccountManager.getInstance(getActivity()).getDefaultAccount(), session, type, false);
			notificationHandler.sendEmptyMessage(NOTIF_SHOW_LOADING_SPINNER);
			CacheManager.getInstance(getActivity()).makeRequest(request);
		}
		else if((!(downloadPostCount == PrefsUtility.PostCount.ALL) && postRefreshCount == 0) && loadMoreView.getParent() == null) {
			listFooterNotifications.addView(loadMoreView);
		}
	}

	public void onSubscribe() {

		if(postListingURL.pathType() != RedditURLParser.PathType.SubredditPostListingURL) return;

		try {
			RedditSubredditSubscriptionManager
					.getSingleton(getActivity(), RedditAccountManager.getInstance(getActivity()).getDefaultAccount())
					.subscribe(RedditSubreddit.getCanonicalName(postListingURL.asSubredditPostListURL().subreddit), getActivity());
		} catch(RedditSubreddit.InvalidSubredditNameException e) {
			throw new RuntimeException(e);
		}
	}

	public void onUnsubscribe() {

		if(subreddit == null) return;

		try {
			RedditSubredditSubscriptionManager
					.getSingleton(getActivity(), RedditAccountManager.getInstance(getActivity()).getDefaultAccount())
					.unsubscribe(subreddit.getCanonicalName(), getActivity());
		} catch(RedditSubreddit.InvalidSubredditNameException e) {
			throw new RuntimeException(e);
		}
	}

	public RedditSubreddit getSubreddit() {
		return subreddit;
	}

	private class PostListingRequest extends CacheRequest {

		private final boolean firstDownload;

		protected PostListingRequest(Uri url, RedditAccount user, UUID requestSession, DownloadType downloadType, boolean firstDownload) {
			super(General.uriFromString(url.toString()), user, requestSession, Constants.Priority.API_POST_LIST, 0, downloadType, Constants.FileType.POST_LIST, DownloadQueueType.REDDIT_API, true, false, getActivity());
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
		protected void onFailure(final RequestFailureType type, final Throwable t, final Integer status, final String readableMessage) {

			notificationHandler.sendEmptyMessage(NOTIF_HIDE_LOADING_SPINNER);

			if(type == RequestFailureType.CACHE_MISS) {

				final RRError error = new RRError(
						context.getString(R.string.error_postlist_cache_title),
						context.getString(R.string.error_postlist_cache_message),
						t,
						status,
						url.toString());

				notificationHandler.sendMessage(General.handlerMessage(NOTIF_ERROR_FOOTER, error));

			} else {
				final RRError error = General.getGeneralErrorForFailure(context, type, t, status, url.toString());
				notificationHandler.sendMessage(General.handlerMessage(NOTIF_ERROR, error));
			}
		}

		@Override protected void onProgress(final boolean authorizationInProgress, final long bytesRead, final long totalBytes) {}

		@Override protected void onSuccess(final CacheManager.ReadableCacheFile cacheFile, final long timestamp, final UUID session, final boolean fromCache, final String mimetype) {}

		@Override
		public void onJsonParseStarted(final JsonValue value, final long timestamp, final UUID session, final boolean fromCache) {

			// TODO pref (currently 10 mins)
			if(firstDownload && fromCache && RRTime.since(timestamp) > 10 * 60 * 1000) {
				notificationHandler.sendMessage(General.handlerMessage(NOTIF_AGE, timestamp));
			} // TODO resuming a copy

			if(firstDownload) {
				((SessionChangeListener)getActivity()).onSessionChanged(session, SessionChangeListener.SessionChangeType.POSTS, timestamp);
				PostListingFragment.this.session = session;
				PostListingFragment.this.timestamp = timestamp;
			}

			// TODO {"error": 403} is received for unauthorized subreddits

			try {

				final Context context = getActivity();
				final JsonBufferedObject thing = value.asObject();
				final JsonBufferedObject listing = thing.getObject("data");
				final JsonBufferedArray posts = listing.getArray("children");

				final boolean isNsfwAllowed = PrefsUtility.pref_behaviour_nsfw(context, sharedPrefs);
				final boolean isConnectionWifi = General.isConnectionWifi(context);

				final PrefsUtility.AppearanceThumbnailsShow thumbnailsPref = PrefsUtility.appearance_thumbnails_show(context, sharedPrefs);
				final boolean downloadThumbnails = thumbnailsPref == PrefsUtility.AppearanceThumbnailsShow.ALWAYS
						|| (thumbnailsPref == PrefsUtility.AppearanceThumbnailsShow.WIFIONLY && isConnectionWifi);

				final boolean showNsfwThumbnails = PrefsUtility.appearance_thumbnails_nsfw_show(context, sharedPrefs);

				final PrefsUtility.CachePrecacheImages imagePrecachePref = PrefsUtility.cache_precache_images(context, sharedPrefs);
				final PrefsUtility.CachePrecacheComments commentPrecachePref = PrefsUtility.cache_precache_comments(context, sharedPrefs);

				final boolean precacheImages = (imagePrecachePref == PrefsUtility.CachePrecacheImages.ALWAYS
						|| (imagePrecachePref == PrefsUtility.CachePrecacheImages.WIFIONLY && isConnectionWifi))
						&& !General.isCacheDiskFull(context);

				final boolean precacheComments = (commentPrecachePref == PrefsUtility.CachePrecacheComments.ALWAYS
						|| (commentPrecachePref == PrefsUtility.CachePrecacheComments.WIFIONLY && isConnectionWifi));

				Log.i("PostListingFragment", "Precaching images: " + (precacheImages ? "ON" : "OFF"));
				Log.i("PostListingFragment", "Precaching comments: " + (precacheComments ? "ON" : "OFF"));

				final CacheManager cm = CacheManager.getInstance(context);

				// TODO rewrite change data manager
				final HashSet<String> needsChanging = RedditChangeDataManager.getInstance(context).getChangedForParent("posts", user);

				final boolean showSubredditName
						= !(postListingURL != null
						&& postListingURL.pathType() == RedditURLParser.PathType.SubredditPostListingURL
						&& postListingURL.asSubredditPostListURL().type == SubredditPostListURL.Type.SUBREDDIT);

				final ArrayList<RedditPreparedPost> downloadedPosts = new ArrayList<RedditPreparedPost>(25);

				for(final JsonValue postThingValue : posts) {

					final RedditThing postThing = postThingValue.asObject(RedditThing.class);

					if(!postThing.getKind().equals(RedditThing.Kind.POST)) continue;

					final RedditPost post = postThing.asPost();

					after = post.name;

					if(!post.over_18 || isNsfwAllowed) {

						final boolean downloadThisThumbnail = downloadThumbnails && (!post.over_18 || showNsfwThumbnails);

						final int positionInList = postCount;

						final RedditPreparedPost preparedPost = new RedditPreparedPost(context, cm, positionInList, post, timestamp, showSubredditName, needsChanging.contains(post.name), downloadThisThumbnail, precacheImages, user, false);

						if(precacheComments) {

							final CommentListingController controller = new CommentListingController(
									PostCommentListingURL.forPostId(preparedPost.idAlone),
									context);

							CacheManager.getInstance(context).makeRequest(new CacheRequest(
									General.uriFromString(controller.getUri().toString()),
									RedditAccountManager.getInstance(context).getDefaultAccount(),
									null,
									Constants.Priority.COMMENT_PRECACHE,
									positionInList,
									DownloadType.IF_NECESSARY,
									Constants.FileType.COMMENT_LIST,
									DownloadQueueType.REDDIT_API,
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
								protected void onFailure(final RequestFailureType type, final Throwable t, final StatusLine status, final String readableMessage) {
									Log.e("PostListingFragment", "Failed to precache " + url.toString() + "(" + type.toString() + ")");
								}

								@Override
								protected void onProgress(final boolean authorizationInProgress, final long bytesRead, final long totalBytes) {}

								@Override
								protected void onSuccess(final CacheManager.ReadableCacheFile cacheFile, final long timestamp, final UUID session, final boolean fromCache, final String mimetype) {
									Log.i("PostListingFragment", "Successfully precached " + url.toString());
								}
							});
						}
						
						LinkHandler.getImageInfo(context, post.url, Constants.Priority.IMAGE_PRECACHE, positionInList, new GetImageInfoListener() {
							
							@Override public void onFailure(final RequestFailureType type, final Throwable t, final Integer status, final String readableMessage) {}
							@Override public void onNotAnImage() {}

							@Override
							public void onSuccess(final ImageInfo info) {

								if(!precacheImages) return;

								// Don't precache huge images
								if(info.width != null && info.width > 2500) return;
								if(info.height != null && info.height > 2500) return;
								if(info.size != null && info.size > 10 * 1024 * 1024) return;
								
								final URI uri = General.uriFromString(info.urlOriginal);
								if(uri == null) return;
								
								CacheManager.getInstance(context).makeRequest(new CacheRequest(
										uri,
										RedditAccountManager.getAnon(),
										null,
										Constants.Priority.IMAGE_PRECACHE,
										positionInList,
										DownloadType.IF_NECESSARY,
										Constants.FileType.IMAGE,
										DownloadQueueType.QUEUE_IMAGE_PRECACHE,
										false,
										false,
										context
								) {
									@Override protected void onCallbackException(final Throwable t) {}
									@Override protected void onDownloadNecessary() {}
									@Override protected void onDownloadStarted() {}

									@Override protected void onFailure(final RequestFailureType type, final Throwable t, final Integer status, final String readableMessage) {
										Log.e("PostListingFragment", "Failed to precache " + info.urlOriginal + "(" + type.toString() + ")");
									}
									@Override protected void onProgress(final boolean authorizationInProgress, final long bytesRead, final long totalBytes) {}

									@Override protected void onSuccess(final CacheManager.ReadableCacheFile cacheFile, final long timestamp, final UUID session, final boolean fromCache, final String mimetype) {
										Log.i("PostListingFragment", "Successfully precached " + info.urlOriginal);
									}
								});
							}
						});

						downloadedPosts.add(preparedPost);
					}

					postCount++;
					postRefreshCount--;
				}

				adapter.onPostsDownloaded(downloadedPosts);

				notificationHandler.sendEmptyMessage(NOTIF_HIDE_LOADING_SPINNER);

				request = null;
				readyToDownloadMore = true;
				onLoadMoreItemsCheck();

			} catch (Throwable t) {
				notifyFailure(RequestFailureType.PARSE, t, null, "Parse failure");
			}
		}
	}
}
