/*******************************************************************************
 * This file is part of Scroll.
 *
 * Scroll is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Scroll is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Scroll.  If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/

package com.konneh.scroll.reddit.api;

import android.content.Context;
import android.net.Uri;
import android.util.Log;
import org.apache.http.StatusLine;
import com.konneh.scroll.account.RedditAccount;
import com.konneh.scroll.cache.CacheManager;
import com.konneh.scroll.cache.CacheRequest;
import com.konneh.scroll.cache.RequestFailureType;
import com.konneh.scroll.common.Constants;
import com.konneh.scroll.common.General;
import com.konneh.scroll.common.TimestampBound;
import com.konneh.scroll.common.UnexpectedInternalStateException;
import com.konneh.scroll.io.CacheDataSource;
import com.konneh.scroll.io.RequestResponseHandler;
import com.konneh.scroll.io.WritableHashSet;
import com.konneh.scroll.jsonwrap.JsonBuffered;
import com.konneh.scroll.jsonwrap.JsonBufferedArray;
import com.konneh.scroll.jsonwrap.JsonBufferedObject;
import com.konneh.scroll.jsonwrap.JsonValue;
import com.konneh.scroll.reddit.RedditSubredditManager;
import com.konneh.scroll.reddit.things.RedditSubreddit;
import com.konneh.scroll.reddit.things.RedditThing;

import java.net.URI;
import java.util.*;

public class RedditAPIIndividualSubredditListRequester
		implements CacheDataSource<RedditSubredditManager.SubredditListType, WritableHashSet, SubredditRequestFailure> {

	private final Context context;
	private final RedditAccount user;

	public RedditAPIIndividualSubredditListRequester(Context context, RedditAccount user) {
		this.context = context;
		this.user = user;
	}

	public void performRequest(final RedditSubredditManager.SubredditListType type,
							   final TimestampBound timestampBound,
							   final RequestResponseHandler<WritableHashSet, SubredditRequestFailure> handler) {

		if(type == RedditSubredditManager.SubredditListType.DEFAULTS) {

			final long now = System.currentTimeMillis();

			final HashSet<String> data = new HashSet<String>(Constants.Reddit.DEFAULT_SUBREDDITS.length + 1);

			for(String name : Constants.Reddit.DEFAULT_SUBREDDITS) {
				data.add(name.toLowerCase());
			}

			data.add("/r/redreader");

			final WritableHashSet result = new WritableHashSet(data, now, "DEFAULTS");
			handler.onRequestSuccess(result, now);

			return;
		}

		if(type == RedditSubredditManager.SubredditListType.MOST_POPULAR) {
			doSubredditListRequest(RedditSubredditManager.SubredditListType.MOST_POPULAR, handler, null);

		} else if(user.isAnonymous()) {
			switch(type) {

				case SUBSCRIBED:
					performRequest(RedditSubredditManager.SubredditListType.DEFAULTS, timestampBound, handler);
					return;

				case MODERATED: {
					final long curTime = System.currentTimeMillis();
					handler.onRequestSuccess(new WritableHashSet(
							new HashSet<String>(), curTime, RedditSubredditManager.SubredditListType.MODERATED.name()), curTime);
					return;
				}

				case MULTIREDDITS: {
					final long curTime = System.currentTimeMillis();
					handler.onRequestSuccess(new WritableHashSet(
							new HashSet<String>(), curTime, RedditSubredditManager.SubredditListType.MULTIREDDITS.name()),
							curTime);
					return;
				}

				default:
					throw new RuntimeException("Internal error: unknown subreddit list type '" + type.name() + "'");
			}

		} else {
			doSubredditListRequest(type, handler, null);
		}
	}

	private void doSubredditListRequest(final RedditSubredditManager.SubredditListType type,
										final RequestResponseHandler<WritableHashSet, SubredditRequestFailure> handler,
										final String after) {

		URI uri;

		switch(type) {
			case SUBSCRIBED:
				uri = Constants.Reddit.getUri(Constants.Reddit.PATH_SUBREDDITS_MINE_SUBSCRIBER);
				break;
			case MODERATED:
				uri = Constants.Reddit.getUri(Constants.Reddit.PATH_SUBREDDITS_MINE_MODERATOR);
				break;
			case MOST_POPULAR:
				uri = Constants.Reddit.getUri(Constants.Reddit.PATH_SUBREDDITS_POPULAR);
				break;
			default:
				throw new UnexpectedInternalStateException(type.name());
		}

		if(after != null) {
			// TODO move this logic to General?
			final Uri.Builder builder = Uri.parse(uri.toString()).buildUpon();
			builder.appendQueryParameter("after", after);
			uri = General.uriFromString(builder.toString());
		}

		final CacheRequest aboutSubredditCacheRequest = new CacheRequest(
				uri,
				user,
				null,
				Constants.Priority.API_SUBREDDIT_INVIDIVUAL,
				0,
				CacheRequest.DownloadType.FORCE,
				Constants.FileType.SUBREDDIT_LIST,
				true,
				true,
				false,
				context
		) {

			@Override
			protected void onCallbackException(Throwable t) {
				handler.onRequestFailed(new SubredditRequestFailure(RequestFailureType.PARSE, t, null, "Internal error", url));
			}

			@Override protected void onDownloadNecessary() {}
			@Override protected void onDownloadStarted() {}
			@Override protected void onProgress(final boolean authorizationInProgress, long bytesRead, long totalBytes) {}

			@Override
			protected void onFailure(RequestFailureType type, Throwable t, StatusLine status, String readableMessage) {
				handler.onRequestFailed(new SubredditRequestFailure(type, t, status, readableMessage, url.toString()));
			}

			@Override
			protected void onSuccess(CacheManager.ReadableCacheFile cacheFile, long timestamp, UUID session,
									 boolean fromCache, String mimetype) {}

			@Override
			public void onJsonParseStarted(JsonValue result, long timestamp, UUID session, boolean fromCache) {

				try {

					final HashSet<String> output = new HashSet<String>();
					final ArrayList<RedditSubreddit> toWrite = new ArrayList<RedditSubreddit>();

					final JsonBufferedObject redditListing = result.asObject().getObject("data");

					final JsonBufferedArray subreddits = redditListing.getArray("children");

					final JsonBuffered.Status joinStatus = subreddits.join();
					if(joinStatus == JsonBuffered.Status.FAILED) {
						handler.onRequestFailed(new SubredditRequestFailure(RequestFailureType.PARSE, null, null, "Unknown parse error", url.toString()));
						return;
					}

					if(type == RedditSubredditManager.SubredditListType.SUBSCRIBED
							&& subreddits.getCurrentItemCount() == 0
							&& after == null) {
						doSubredditListRequest(RedditSubredditManager.SubredditListType.DEFAULTS, handler, null);
						return;
					}

					for(final JsonValue v : subreddits) {
						final RedditThing thing = v.asObject(RedditThing.class);
						final RedditSubreddit subreddit = thing.asSubreddit();
						subreddit.downloadTime = timestamp;

						toWrite.add(subreddit);
						output.add(subreddit.getCanonicalName());
					}

					RedditSubredditManager.getInstance(context, user).offerRawSubredditData(toWrite, timestamp);
					final String receivedAfter = redditListing.getString("after");
					if(receivedAfter != null && type != RedditSubredditManager.SubredditListType.MOST_POPULAR) {

						doSubredditListRequest(type, new RequestResponseHandler<WritableHashSet, SubredditRequestFailure>() {
							public void onRequestFailed(SubredditRequestFailure failureReason) {
								handler.onRequestFailed(failureReason);
							}

							public void onRequestSuccess(WritableHashSet result, long timeCached) {
								output.addAll(result.toHashset());
								handler.onRequestSuccess(new WritableHashSet(output, timeCached, type.name()), timeCached);

								if(after == null) {
									Log.i("SubredditListRequester", "Got " + output.size() + " subreddits in multiple requests");
								}
							}
						}, receivedAfter);

					} else {
						handler.onRequestSuccess(new WritableHashSet(output, timestamp, type.name()), timestamp);

						if(after == null) {
							Log.i("SubredditListRequester", "Got " + output.size() + " subreddits in 1 request");
						}
					}

				} catch(Exception e) {
					handler.onRequestFailed(new SubredditRequestFailure(RequestFailureType.PARSE, e, null, "Parse error", url.toString()));
				}
			}
		};

		CacheManager.getInstance(context).makeRequest(aboutSubredditCacheRequest);
	}

	public void performRequest(Collection<RedditSubredditManager.SubredditListType> keys, TimestampBound timestampBound,
							   RequestResponseHandler<HashMap<RedditSubredditManager.SubredditListType, WritableHashSet>,
									   SubredditRequestFailure> handler) {
		// TODO batch API? or just make lots of requests and build up a hash map?
		throw new UnsupportedOperationException();
	}

	public void performWrite(WritableHashSet value) {
		throw new UnsupportedOperationException();
	}

	public void performWrite(Collection<WritableHashSet> values) {
		throw new UnsupportedOperationException();
	}
}
