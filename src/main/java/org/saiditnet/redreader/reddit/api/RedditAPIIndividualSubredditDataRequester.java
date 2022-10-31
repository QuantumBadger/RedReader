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

package org.saiditnet.redreader.reddit.api;

import android.content.Context;
import android.util.Log;
import org.saiditnet.redreader.account.RedditAccount;
import org.saiditnet.redreader.cache.CacheManager;
import org.saiditnet.redreader.cache.CacheRequest;
import org.saiditnet.redreader.cache.downloadstrategy.DownloadStrategyAlways;
import org.saiditnet.redreader.common.Constants;
import org.saiditnet.redreader.common.TimestampBound;
import org.saiditnet.redreader.io.CacheDataSource;
import org.saiditnet.redreader.io.RequestResponseHandler;
import org.saiditnet.redreader.jsonwrap.JsonValue;
import org.saiditnet.redreader.reddit.RedditSubredditHistory;
import org.saiditnet.redreader.reddit.things.RedditSubreddit;
import org.saiditnet.redreader.reddit.things.RedditThing;

import java.util.Collection;
import java.util.HashMap;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

public class RedditAPIIndividualSubredditDataRequester implements CacheDataSource<String, RedditSubreddit, SubredditRequestFailure> {

	private static final String TAG = "IndividualSRDataReq";

	private final Context context;
	private final RedditAccount user;

	public RedditAPIIndividualSubredditDataRequester(Context context, RedditAccount user) {
		this.context = context;
		this.user = user;
	}

	public void performRequest(final String subredditCanonicalName,
							   final TimestampBound timestampBound,
							   final RequestResponseHandler<RedditSubreddit, SubredditRequestFailure> handler) {

		final CacheRequest aboutSubredditCacheRequest = new CacheRequest(
				Constants.Reddit.getUri("/s/" + subredditCanonicalName + "/about.json"),
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
			protected void onCallbackException(Throwable t) {
				handler.onRequestFailed(new SubredditRequestFailure(CacheRequest.REQUEST_FAILURE_PARSE, t, null, "Parse error", url));
			}

			@Override protected void onDownloadNecessary() {}
			@Override protected void onDownloadStarted() {}
			@Override protected void onProgress(final boolean authorizationInProgress, long bytesRead, long totalBytes) {}

			@Override
			protected void onFailure(@CacheRequest.RequestFailureType int type, Throwable t, Integer status, String readableMessage) {
				handler.onRequestFailed(new SubredditRequestFailure(type, t, status, readableMessage, url));
			}

			@Override
			protected void onSuccess(CacheManager.ReadableCacheFile cacheFile, long timestamp, UUID session,
									 boolean fromCache, String mimetype) {}

			@Override
			public void onJsonParseStarted(JsonValue result, long timestamp, UUID session, boolean fromCache) {

				try {
					final RedditThing subredditThing = result.asObject(RedditThing.class);
					final RedditSubreddit subreddit = subredditThing.asSubreddit();
					subreddit.downloadTime = timestamp;
					handler.onRequestSuccess(subreddit, timestamp);

					RedditSubredditHistory.addSubreddit(user, subredditCanonicalName);

				} catch(Exception e) {
					handler.onRequestFailed(new SubredditRequestFailure(CacheRequest.REQUEST_FAILURE_PARSE, e, null, "Parse error", url));
				}
			}
		};

		CacheManager.getInstance(context).makeRequest(aboutSubredditCacheRequest);
	}

	public void performRequest(final Collection<String> subredditCanonicalIds,
							   final TimestampBound timestampBound,
							   final RequestResponseHandler<HashMap<String, RedditSubreddit>, SubredditRequestFailure> handler) {

		// TODO if there's a bulk API to do this, that would be good... :)

		final HashMap<String, RedditSubreddit> result = new HashMap<>();
		final AtomicBoolean stillOkay = new AtomicBoolean(true);
		final AtomicInteger requestsToGo = new AtomicInteger(subredditCanonicalIds.size());
		final AtomicLong oldestResult = new AtomicLong(Long.MAX_VALUE);

		final RequestResponseHandler <RedditSubreddit, SubredditRequestFailure> innerHandler
				= new RequestResponseHandler<RedditSubreddit, SubredditRequestFailure>() {
			@Override
			public void onRequestFailed(SubredditRequestFailure failureReason) {
				synchronized(result) {
					if(stillOkay.get()) {
						stillOkay.set(false);
						handler.onRequestFailed(failureReason);
					}
				}
			}

			@Override
			public void onRequestSuccess(RedditSubreddit innerResult, long timeCached) {
				synchronized(result) {
					if(stillOkay.get()) {

						result.put(innerResult.getKey(), innerResult);
						oldestResult.set(Math.min(oldestResult.get(), timeCached));

						try
						{
							RedditSubredditHistory.addSubreddit(user, innerResult.getCanonicalName());
						}
						catch(RedditSubreddit.InvalidSubredditNameException e)
						{
							Log.e(TAG, "Invalid subreddit name " + innerResult.name, e);
						}

						if(requestsToGo.decrementAndGet() == 0) {
							handler.onRequestSuccess(result, oldestResult.get());
						}
					}
				}
			}
		};

		for(String subredditCanonicalId : subredditCanonicalIds) {
			performRequest(subredditCanonicalId, timestampBound, innerHandler);
		}
	}

	public void performWrite(RedditSubreddit value) {
		throw new UnsupportedOperationException();
	}

	public void performWrite(Collection<RedditSubreddit> values) {
		throw new UnsupportedOperationException();
	}
}
