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
import android.net.Uri;
import org.apache.http.StatusLine;
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.cache.CacheManager;
import org.quantumbadger.redreader.cache.CacheRequest;
import org.quantumbadger.redreader.cache.RequestFailureType;
import org.quantumbadger.redreader.common.Constants;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.TimestampBound;
import org.quantumbadger.redreader.common.UnexpectedInternalStateException;
import org.quantumbadger.redreader.io.CacheDataSource;
import org.quantumbadger.redreader.io.RequestResponseHandler;
import org.quantumbadger.redreader.io.WritableHashSet;
import org.quantumbadger.redreader.jsonwrap.JsonBuffered;
import org.quantumbadger.redreader.jsonwrap.JsonBufferedArray;
import org.quantumbadger.redreader.jsonwrap.JsonBufferedObject;
import org.quantumbadger.redreader.jsonwrap.JsonValue;
import org.quantumbadger.redreader.reddit.RedditSubredditManager;
import org.quantumbadger.redreader.reddit.things.RedditSubreddit;
import org.quantumbadger.redreader.reddit.things.RedditThing;

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

		if(type == RedditSubredditManager.SubredditListType.MOST_POPULAR) {
			doSubredditListRequest(RedditSubredditManager.SubredditListType.MOST_POPULAR, handler, null);

		} else if(user.isAnonymous()) {
			switch(type) {

				case SUBSCRIBED:
					performRequest(RedditSubredditManager.SubredditListType.MOST_POPULAR, timestampBound, handler);
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
			@Override protected void onProgress(long bytesRead, long totalBytes) {}

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

					if(type == RedditSubredditManager.SubredditListType.SUBSCRIBED && subreddits.getCurrentItemCount() == 0) {
						doSubredditListRequest(RedditSubredditManager.SubredditListType.MOST_POPULAR, handler, null);
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
					if(receivedAfter != null) {

						doSubredditListRequest(type, new RequestResponseHandler<WritableHashSet, SubredditRequestFailure>() {
							public void onRequestFailed(SubredditRequestFailure failureReason) {
								handler.onRequestFailed(failureReason);
							}

							public void onRequestSuccess(WritableHashSet result, long timeCached) {
								output.addAll(result.toHashset());
								handler.onRequestSuccess(new WritableHashSet(output, timeCached, type.name()), timeCached);
							}
						}, receivedAfter);

					} else {
						handler.onRequestSuccess(new WritableHashSet(output, timestamp, type.name()), timestamp);
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
