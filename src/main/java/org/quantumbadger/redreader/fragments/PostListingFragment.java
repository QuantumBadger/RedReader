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
import android.net.Uri;
import android.os.Bundle;
import android.os.Handler;
import android.os.Looper;
import android.os.Message;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AbsListView;
import android.widget.Toast;
import org.apache.commons.lang3.StringEscapeUtils;
import org.apache.http.StatusLine;
import org.holoeverywhere.LayoutInflater;
import org.holoeverywhere.app.Fragment;
import org.holoeverywhere.preference.PreferenceManager;
import org.holoeverywhere.preference.SharedPreferences;
import org.holoeverywhere.widget.LinearLayout;
import org.holoeverywhere.widget.ListView;
import org.holoeverywhere.widget.TextView;
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
import org.quantumbadger.redreader.io.RequestResponseHandler;
import org.quantumbadger.redreader.jsonwrap.JsonBufferedArray;
import org.quantumbadger.redreader.jsonwrap.JsonBufferedObject;
import org.quantumbadger.redreader.jsonwrap.JsonValue;
import org.quantumbadger.redreader.listingcontrollers.PostListingController;
import org.quantumbadger.redreader.reddit.RedditSubredditManager;
import org.quantumbadger.redreader.reddit.api.RedditSubredditSubscriptionManager;
import org.quantumbadger.redreader.reddit.api.SubredditRequestFailure;
import org.quantumbadger.redreader.reddit.prepared.RedditChangeDataManager;
import org.quantumbadger.redreader.reddit.prepared.RedditPreparedPost;
import org.quantumbadger.redreader.reddit.things.RedditPost;
import org.quantumbadger.redreader.reddit.things.RedditSubreddit;
import org.quantumbadger.redreader.reddit.things.RedditThing;
import org.quantumbadger.redreader.reddit.url.PostListingURL;
import org.quantumbadger.redreader.reddit.url.RedditURLParser;
import org.quantumbadger.redreader.reddit.url.SubredditPostListURL;
import org.quantumbadger.redreader.views.PostListingHeader;
import org.quantumbadger.redreader.views.RedditPostView;
import org.quantumbadger.redreader.views.list.ListOverlayView;
import org.quantumbadger.redreader.views.liststatus.ErrorView;
import org.quantumbadger.redreader.views.liststatus.LoadingView;

import java.text.NumberFormat;
import java.util.HashSet;
import java.util.Locale;
import java.util.UUID;

public class PostListingFragment extends Fragment implements RedditPostView.PostSelectionListener, AbsListView.OnScrollListener {

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

	private LoadingView loadingView;

	private int postCount = 0;
	private int postTotalCount = 0;
	private int postRefreshCount = 0;

	private static final int NOTIF_DOWNLOAD_NECESSARY = 1,
			NOTIF_DOWNLOAD_START = 2,
			NOTIF_STARTING = 3,
			NOTIF_AGE = 4,
			NOTIF_ERROR = 5,
			NOTIF_PROGRESS = 6,
			NOTIF_DOWNLOAD_DONE = 7,
			NOTIF_ERROR_FOOTER = 8;

	private final Handler notificationHandler = new Handler(Looper.getMainLooper()) {
		@Override
		public void handleMessage(final Message msg) {

			final Context context = getSupportActivity();

			// TODO check if attached? if not, queue, and send on "resume"
			switch(msg.what) {
				case NOTIF_DOWNLOAD_NECESSARY:
					loadingView = new LoadingView(context, R.string.download_waiting, true, true);
					listFooterNotifications.addView(loadingView);
					adapter.notifyDataSetChanged();
					break;

				case NOTIF_DOWNLOAD_START:
					loadingView.setIndeterminate(R.string.download_connecting);
					break;

				case NOTIF_STARTING:
					if(loadingView != null) loadingView.setIndeterminate(R.string.download_downloadstarting);
					break;

				case NOTIF_AGE:
					final TextView cacheNotif = new TextView(context);
					cacheNotif.setText(context.getString(R.string.listing_cached) + " " + RRTime.formatDateTime((Long) msg.obj, context));
					final int paddingPx = General.dpToPixels(context, 6);
					cacheNotif.setPadding(paddingPx, paddingPx, paddingPx, paddingPx);
					cacheNotif.setTextSize(13f);
					listHeaderNotifications.addView(cacheNotif);
					listHeaderNotifications.requestLayout();
					adapter.notifyDataSetChanged();
					break;

				case NOTIF_ERROR: {
					if(loadingView != null) loadingView.setDone(R.string.download_failed);
					final RRError error = (RRError)msg.obj;
					fragmentHeader.addView(new ErrorView(getSupportActivity(), error));
					break;
				}

				case NOTIF_PROGRESS:
					if(loadingView != null) loadingView.setProgress(R.string.download_loading, (Float) msg.obj);
					break;

				case NOTIF_DOWNLOAD_DONE:
					if(loadingView != null) loadingView.setDoneNoAnim(R.string.download_done);
					break;

				case NOTIF_ERROR_FOOTER: {
					if(loadingView != null) loadingView.setDone(R.string.download_failed);
					final RRError error = (RRError)msg.obj;
					listFooterNotifications.addView(new ErrorView(getSupportActivity(), error));
					adapter.notifyDataSetChanged();
					break;
				}
			}
		}
	};

	public static PostListingFragment newInstance(final Uri url, final UUID session, final CacheRequest.DownloadType downloadType) {

		final PostListingFragment f = new PostListingFragment();

		final Bundle bundle = new Bundle(4);

		bundle.putString("url", url.toString());
		if(session != null) bundle.putString("session", session.toString());
		bundle.putString("downloadType", downloadType.name());

		f.setArguments(bundle);

		return f;
	}

	@Override
	public void onCreate(final Bundle savedInstanceState) {
		// TODO load position/etc?
		super.onCreate(savedInstanceState);

		final Bundle arguments = getArguments();

		final Uri url = Uri.parse(arguments.getString("url"));

		try {
			postListingURL = (PostListingURL) RedditURLParser.parseProbablePostListing(url);
		} catch(ClassCastException e) {
			Toast.makeText(getSupportActivity(), "Invalid post listing URL.", Toast.LENGTH_LONG);
			return;
		}

		if(arguments.containsKey("session")) {
			session = UUID.fromString(arguments.getString("session"));
		}

		downloadType = CacheRequest.DownloadType.valueOf(arguments.getString("downloadType"));
	}

	private LinearLayout createVerticalLinearLayout(Context context) {
		final LinearLayout result = new LinearLayout(context);
		result.setOrientation(LinearLayout.VERTICAL);
		return result;
	}

	@Override
	public View onCreateView(final LayoutInflater inflater, final ViewGroup container, final Bundle savedInstanceState) {

		super.onCreateView(inflater, container, savedInstanceState);
		final Context context = container.getContext();
		sharedPrefs = PreferenceManager.getDefaultSharedPreferences(context);

		final LinearLayout outer = new LinearLayout(context) {
			@Override
			protected void onAttachedToWindow() {
				super.onAttachedToWindow();
				getLayoutParams().height = ViewGroup.LayoutParams.MATCH_PARENT;
			}
		};

		outer.setOrientation(android.widget.LinearLayout.VERTICAL);

		fragmentHeader = createVerticalLinearLayout(context);

		// TODO output failed URL
		if(postListingURL == null) {
			fragmentHeader.addView(new ErrorView(getSupportActivity(), new RRError("Invalid post listing URL", "Could not navigate to that URL.")));
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

		lv = (ListView)inflater.inflate(R.layout.reddit_post_list);
		lv.setOnScrollListener(this);
		lv.addHeaderView(listHeader);
		lv.addFooterView(listFooterNotifications, null, true);

		lv.setPersistentDrawingCache(ViewGroup.PERSISTENT_ALL_CACHES);
		lv.setAlwaysDrawnWithCacheEnabled(true);

		adapter = new PostListingAdapter(lv, this);
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
				setHeader(postListingURL.humanReadableName(getSupportActivity(), true), postListingURL.humanReadableUrl());
				break;

			case SubredditPostListingURL:

				SubredditPostListURL subredditPostListURL
						= (SubredditPostListURL) postListingURL;

				switch(subredditPostListURL.type) {

					case FRONTPAGE:
					case ALL:
					case SUBREDDIT_COMBINATION:
					case ALL_SUBTRACTION:
						setHeader(postListingURL.humanReadableName(getSupportActivity(), true), postListingURL.humanReadableUrl());
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
									.getInstance(getSupportActivity(), RedditAccountManager.getInstance(getSupportActivity()).getDefaultAccount())
									.getSubreddit(RedditSubreddit.getCanonicalName(subredditPostListURL.subreddit), TimestampBound.NONE, subredditHandler, null);
						} catch(RedditSubreddit.InvalidSubredditNameException e) {
							throw new RuntimeException(e);
						}
						break;
					}
				}

				break;
		}

		return outer;
	}

	@Override
	public void onSaveInstanceState(final Bundle outState) {
		// TODO save menu position
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

		getSupportActivity().runOnUiThread(new Runnable() {
			@Override
			public void run() {
				setHeader(StringEscapeUtils.unescapeHtml4(subreddit.title), subtitle);
				getSupportActivity().invalidateOptionsMenu();
			}
		});

	}

	private void setHeader(final String title, final String subtitle) {
		getSupportActivity().runOnUiThread(new Runnable() {
			@Override
			public void run() {
				final PostListingHeader postListingHeader = new PostListingHeader(getSupportActivity(), title, subtitle);
				listHeader.addView(postListingHeader, 0);
				adapter.notifyDataSetChanged();
			}
		});
	}

	public void onPostSelected(final RedditPreparedPost post) {
		((RedditPostView.PostSelectionListener)getSupportActivity()).onPostSelected(post);

		new Thread() {
			public void run() {
				post.markAsRead(getSupportActivity());
			}
		}.start();
	}

	public void onPostCommentsSelected(final RedditPreparedPost post) {
		
		((RedditPostView.PostSelectionListener)getSupportActivity()).onPostCommentsSelected(post);

		new Thread() {
			public void run() {
				post.markAsRead(getSupportActivity());
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

			request = new PostListingRequest(newUri, RedditAccountManager.getInstance(getSupportActivity()).getDefaultAccount(), session, type, false);
			CacheManager.getInstance(getSupportActivity()).makeRequest(request);
		}
		else if((!(downloadPostCount == PrefsUtility.PostCount.ALL) && postRefreshCount == 0) && loadMoreView.getParent() == null) {
			listFooterNotifications.addView(loadMoreView);
		}
	}

	public void onSubscribe() {

		if(postListingURL.pathType() != RedditURLParser.PathType.SubredditPostListingURL) return;

		try {
			RedditSubredditSubscriptionManager
					.getSingleton(getSupportActivity(), RedditAccountManager.getInstance(getSupportActivity()).getDefaultAccount())
					.subscribe(RedditSubreddit.getCanonicalName(postListingURL.asSubredditPostListURL().subreddit), getSupportActivity());
		} catch(RedditSubreddit.InvalidSubredditNameException e) {
			throw new RuntimeException(e);
		}
	}

	public void onUnsubscribe() {

		if(subreddit == null) return;

		try {
			RedditSubredditSubscriptionManager
					.getSingleton(getSupportActivity(), RedditAccountManager.getInstance(getSupportActivity()).getDefaultAccount())
					.unsubscribe(subreddit.getCanonicalName(), getSupportActivity());
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
			super(General.uriFromString(url.toString()), user, requestSession, Constants.Priority.API_POST_LIST, 0, downloadType, Constants.FileType.POST_LIST, true, true, false, getSupportActivity());
			this.firstDownload = firstDownload;
		}

		@Override
		protected void onDownloadNecessary() {
			notificationHandler.sendMessage(General.handlerMessage(NOTIF_DOWNLOAD_NECESSARY, null));
		}

		@Override
		protected void onDownloadStarted() {
			notificationHandler.sendMessage(General.handlerMessage(NOTIF_DOWNLOAD_START, null));
		}

		@Override
		protected void onCallbackException(final Throwable t) {
			BugReportActivity.handleGlobalError(context, t);
		}

		@Override
		protected void onFailure(final RequestFailureType type, final Throwable t, final StatusLine status, final String readableMessage) {
			final RRError error = General.getGeneralErrorForFailure(context, type, t, status, url.toString());
			notificationHandler.sendMessage(General.handlerMessage(NOTIF_ERROR, error));
		}

		@Override protected void onProgress(final long bytesRead, final long totalBytes) {}
		@Override protected void onSuccess(final CacheManager.ReadableCacheFile cacheFile, final long timestamp, final UUID session, final boolean fromCache, final String mimetype) {}

		@Override
		public void onJsonParseStarted(final JsonValue value, final long timestamp, final UUID session, final boolean fromCache) {

			notificationHandler.sendMessage(General.handlerMessage(NOTIF_STARTING, null));

			postTotalCount += 25; // TODO this can vary with the user's reddit settings

			// TODO pref (currently 10 mins)
			if(firstDownload && fromCache && RRTime.since(timestamp) > 10 * 60 * 1000) {
				notificationHandler.sendMessage(General.handlerMessage(NOTIF_AGE, timestamp));
			} // TODO resuming a copy

			if(firstDownload) {
				((SessionChangeListener)getSupportActivity()).onSessionChanged(session, SessionChangeListener.SessionChangeType.POSTS, timestamp);
				PostListingFragment.this.session = session;
				PostListingFragment.this.timestamp = timestamp;
			}

			// TODO {"error": 403} is received for unauthorized subreddits

			try {

				final Context context = getSupportActivity();
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
				final boolean precacheImages = (imagePrecachePref == PrefsUtility.CachePrecacheImages.ALWAYS
						|| (imagePrecachePref == PrefsUtility.CachePrecacheImages.WIFIONLY && isConnectionWifi))
						&& !General.isCacheDiskFull(context);

				final CacheManager cm = CacheManager.getInstance(context);

				// TODO rewrite change data manager
				final HashSet<String> needsChanging = RedditChangeDataManager.getInstance(context).getChangedForParent("posts", user);

				final boolean showSubredditName
						= !(postListingURL != null
						&& postListingURL.pathType() == RedditURLParser.PathType.SubredditPostListingURL
						&& postListingURL.asSubredditPostListURL().type == SubredditPostListURL.Type.SUBREDDIT);

				for(final JsonValue postThingValue : posts) {

					final RedditThing postThing = postThingValue.asObject(RedditThing.class);

					if(!postThing.getKind().equals(RedditThing.Kind.POST)) continue;

					final RedditPost post = postThing.asPost();

					after = post.name;

					if(!post.over_18 || isNsfwAllowed) {

						final boolean downloadThisThumbnail = downloadThumbnails && (!post.over_18 || showNsfwThumbnails);

						final RedditPreparedPost preparedPost = new RedditPreparedPost(context, cm, postCount, post, timestamp, showSubredditName, needsChanging.contains(post.name), downloadThisThumbnail, precacheImages, user);
						adapter.onPostDownloaded(preparedPost);
					}

					postCount++;
					postRefreshCount--;
					// TODO make specific to this download? don't keep global post count
					notificationHandler.sendMessage(General.handlerMessage(NOTIF_PROGRESS, (float) postCount / (float) postTotalCount));
				}

				notificationHandler.sendMessage(General.handlerMessage(NOTIF_DOWNLOAD_DONE, null));

				request = null;
				readyToDownloadMore = true;
				onLoadMoreItemsCheck();

			} catch (Throwable t) {
				notifyFailure(RequestFailureType.PARSE, t, null, "Parse failure");
			}
		}
	}
}
