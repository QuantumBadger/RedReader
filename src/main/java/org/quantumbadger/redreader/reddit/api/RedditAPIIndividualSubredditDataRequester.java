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

package org.quantumbadger.redreader.reddit.api;

import android.content.Context;
import android.util.Log;
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.cache.CacheManager;
import org.quantumbadger.redreader.cache.CacheRequest;
import org.quantumbadger.redreader.cache.downloadstrategy.DownloadStrategyAlways;
import org.quantumbadger.redreader.common.Constants;
import org.quantumbadger.redreader.common.TimestampBound;
import org.quantumbadger.redreader.io.CacheDataSource;
import org.quantumbadger.redreader.io.RequestResponseHandler;
import org.quantumbadger.redreader.jsonwrap.JsonValue;
import org.quantumbadger.redreader.reddit.RedditSubredditHistory;
import org.quantumbadger.redreader.reddit.things.InvalidSubredditNameException;
import org.quantumbadger.redreader.reddit.things.RedditSubreddit;
import org.quantumbadger.redreader.reddit.things.RedditThing;
import org.quantumbadger.redreader.reddit.things.SubredditCanonicalId;

import java.util.Collection;
import java.util.HashMap;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

public class RedditAPIIndividualSubredditDataRequester implements
		CacheDataSource<SubredditCanonicalId, RedditSubreddit, SubredditRequestFailure> {

	private static final String TAG = "IndividualSRDataReq";

	private final Context context;
	private final RedditAccount user;

	public RedditAPIIndividualSubredditDataRequester(
			final Context context,
			final RedditAccount user) {
		this.context = context;
		this.user = user;
	}

	@Override
	public void performRequest(
			final SubredditCanonicalId subredditCanonicalId,
			final TimestampBound timestampBound,
			final RequestResponseHandler<RedditSubreddit, SubredditRequestFailure> handler) {

		final CacheRequest aboutSubredditCacheRequest = new CacheRequest(
				Constants.Reddit.getUri(subredditCanonicalId.toString() + "/about.json"),
				user,
				null,
				Constants.Priority.API_SUBREDDIT_INVIDIVUAL,
				0,
				DownloadStrategyAlways.INSTANCE,
				Constants.FileType.SUBREDDIT_ABOUT,
				CacheRequest.DOWNLOAD_QUEUE_REDDIT_API,
				true,
				false,
				context
		) {

			@Override
			protected void onCallbackException(final Throwable t) {
				handler.onRequestFailed(new SubredditRequestFailure(
						CacheRequest.REQUEST_FAILURE_PARSE,
						t,
						null,
						"Parse error",
						url));
			}

			@Override
			protected void onDownloadNecessary() {
			}

			@Override
			protected void onDownloadStarted() {
			}

			@Override
			protected void onProgress(
					final boolean authorizationInProgress,
					final long bytesRead,
					final long totalBytes) {
			}

			@Override
			protected void onFailure(
					@CacheRequest.RequestFailureType final int type,
					final Throwable t,
					final Integer status,
					final String readableMessage) {
				handler.onRequestFailed(new SubredditRequestFailure(
						type,
						t,
						status,
						readableMessage,
						url));
			}

			@Override
			protected void onSuccess(
					final CacheManager.ReadableCacheFile cacheFile,
					final long timestamp,
					final UUID session,
					final boolean fromCache,
					final String mimetype) {
			}

			@Override
			public void onJsonParseStarted(
					final JsonValue result,
					final long timestamp,
					final UUID session,
					final boolean fromCache) {

				try {
					final RedditThing subredditThing = result.asObject(RedditThing.class);
					final RedditSubreddit subreddit = subredditThing.asSubreddit();
					subreddit.downloadTime = timestamp;
					handler.onRequestSuccess(subreddit, timestamp);

					RedditSubredditHistory.addSubreddit(user, subredditCanonicalId);

				} catch(final Exception e) {
					handler.onRequestFailed(new SubredditRequestFailure(
							CacheRequest.REQUEST_FAILURE_PARSE,
							e,
							null,
							"Parse error",
							url));
				}
			}
		};

		CacheManager.getInstance(context).makeRequest(aboutSubredditCacheRequest);
	}

	@Override
	public void performRequest(
			final Collection<SubredditCanonicalId> subredditCanonicalIds,
			final TimestampBound timestampBound,
			final RequestResponseHandler<
					HashMap<SubredditCanonicalId, RedditSubreddit>,
					SubredditRequestFailure> handler) {

		// TODO if there's a bulk API to do this, that would be good... :)

		final HashMap<SubredditCanonicalId, RedditSubreddit> result = new HashMap<>();
		final AtomicBoolean stillOkay = new AtomicBoolean(true);
		final AtomicInteger requestsToGo
				= new AtomicInteger(subredditCanonicalIds.size());
		final AtomicLong oldestResult = new AtomicLong(Long.MAX_VALUE);

		final RequestResponseHandler<RedditSubreddit, SubredditRequestFailure>
				innerHandler
				= new RequestResponseHandler<RedditSubreddit, SubredditRequestFailure>() {
			@Override
			public void onRequestFailed(final SubredditRequestFailure failureReason) {
				synchronized(result) {
					if(stillOkay.get()) {
						stillOkay.set(false);
						handler.onRequestFailed(failureReason);
					}
				}
			}

			@Override
			public void onRequestSuccess(final RedditSubreddit innerResult, final long timeCached) {
				synchronized(result) {
					if(stillOkay.get()) {

						try {
							final SubredditCanonicalId canonicalId
									= innerResult.getCanonicalId();

							result.put(canonicalId, innerResult);
							oldestResult.set(Math.min(oldestResult.get(), timeCached));
							RedditSubredditHistory.addSubreddit(user, canonicalId);
						} catch(final InvalidSubredditNameException e) {
							Log.e(TAG, "Invalid subreddit name " + innerResult.name, e);
						}

						if(requestsToGo.decrementAndGet() == 0) {
							handler.onRequestSuccess(result, oldestResult.get());
						}
					}
				}
			}
		};

		for(final SubredditCanonicalId subredditCanonicalId : subredditCanonicalIds) {
			performRequest(subredditCanonicalId, timestampBound, innerHandler);
		}
	}

	@Override
	public void performWrite(final RedditSubreddit value) {
		throw new UnsupportedOperationException();
	}

	@Override
	public void performWrite(final Collection<RedditSubreddit> values) {
		throw new UnsupportedOperationException();
	}
}
