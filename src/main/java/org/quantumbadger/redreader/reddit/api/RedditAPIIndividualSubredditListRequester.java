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
import android.util.Log;

import androidx.annotation.NonNull;

import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.cache.CacheManager;
import org.quantumbadger.redreader.cache.CacheRequest;
import org.quantumbadger.redreader.cache.CacheRequestJSONParser;
import org.quantumbadger.redreader.cache.downloadstrategy.DownloadStrategyAlways;
import org.quantumbadger.redreader.common.Constants;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.Optional;
import org.quantumbadger.redreader.common.Priority;
import org.quantumbadger.redreader.common.RRError;
import org.quantumbadger.redreader.common.TimestampBound;
import org.quantumbadger.redreader.common.UnexpectedInternalStateException;
import org.quantumbadger.redreader.common.UriString;
import org.quantumbadger.redreader.common.time.TimestampUTC;
import org.quantumbadger.redreader.http.FailedRequestBody;
import org.quantumbadger.redreader.io.CacheDataSource;
import org.quantumbadger.redreader.io.RequestResponseHandler;
import org.quantumbadger.redreader.io.WritableHashSet;
import org.quantumbadger.redreader.jsonwrap.JsonArray;
import org.quantumbadger.redreader.jsonwrap.JsonObject;
import org.quantumbadger.redreader.jsonwrap.JsonValue;
import org.quantumbadger.redreader.reddit.RedditSubredditManager;
import org.quantumbadger.redreader.reddit.things.InvalidSubredditNameException;
import org.quantumbadger.redreader.reddit.things.RedditSubreddit;
import org.quantumbadger.redreader.reddit.things.RedditThing;
import org.quantumbadger.redreader.reddit.things.SubredditCanonicalId;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.UUID;

public class RedditAPIIndividualSubredditListRequester implements CacheDataSource<
		RedditSubredditManager.SubredditListType, WritableHashSet, RRError> {

	private final Context context;
	private final RedditAccount user;

	public RedditAPIIndividualSubredditListRequester(
			final Context context,
			final RedditAccount user) {
		this.context = context;
		this.user = user;
	}

	@Override
	public void performRequest(
			final RedditSubredditManager.SubredditListType type,
			final TimestampBound timestampBound,
			final RequestResponseHandler<WritableHashSet, RRError> handler) {

		if(type == RedditSubredditManager.SubredditListType.DEFAULTS) {

			final TimestampUTC now = TimestampUTC.now();

			final HashSet<String> data =
					new HashSet<>(Constants.Reddit.DEFAULT_SUBREDDITS.size() + 1);

			for(final SubredditCanonicalId id : Constants.Reddit.DEFAULT_SUBREDDITS) {
				data.add(id.toString());
			}

			data.add("/r/redreader");

			final WritableHashSet result = new WritableHashSet(data, now, "DEFAULTS");
			handler.onRequestSuccess(result, now);

			return;
		}

		if(type == RedditSubredditManager.SubredditListType.MOST_POPULAR) {
			doSubredditListRequest(
					RedditSubredditManager.SubredditListType.MOST_POPULAR,
					handler,
					null);

		} else if(user.isAnonymous()) {
			switch(type) {

				case SUBSCRIBED:
					performRequest(
							RedditSubredditManager.SubredditListType.DEFAULTS,
							timestampBound,
							handler);
					return;

				case MODERATED: {
					final TimestampUTC curTime = TimestampUTC.now();
					handler.onRequestSuccess(
							new WritableHashSet(
									new HashSet<>(),
									curTime,
									RedditSubredditManager.SubredditListType.MODERATED.name()),
							curTime);
					return;
				}

				case MULTIREDDITS: {
					final TimestampUTC curTime = TimestampUTC.now();
					handler.onRequestSuccess(
							new WritableHashSet(
									new HashSet<>(),
									curTime,
									RedditSubredditManager.SubredditListType.MULTIREDDITS.name()),
							curTime);
					return;
				}

				default:
					throw new RuntimeException(
							"Internal error: unknown subreddit list type '"
									+ type.name()
									+ "'");
			}

		} else {
			doSubredditListRequest(type, handler, null);
		}
	}

	private void doSubredditListRequest(
			final RedditSubredditManager.SubredditListType type,
			final RequestResponseHandler<WritableHashSet, RRError> handler,
			final String after) {

		final UriString uri;

		{
			final UriString baseUri;

			switch(type) {
				case SUBSCRIBED:
					baseUri = Constants.Reddit.getUri(
							Constants.Reddit.PATH_SUBREDDITS_MINE_SUBSCRIBER);
					break;
				case MODERATED:
					baseUri = Constants.Reddit.getUri(
							Constants.Reddit.PATH_SUBREDDITS_MINE_MODERATOR);
					break;
				case MOST_POPULAR:
					baseUri = Constants.Reddit.getUri(
							Constants.Reddit.PATH_SUBREDDITS_POPULAR);
					break;
				default:
					throw new UnexpectedInternalStateException(type.name());
			}

			if(after == null) {
				uri = baseUri;

			} else {
				final Uri.Builder builder = Uri.parse(baseUri.toString()).buildUpon();
				builder.appendQueryParameter("after", after);
				uri = UriString.from(builder);
			}
		}

		final CacheRequest aboutSubredditCacheRequest = new CacheRequest.Builder()
				.setUrl(uri)
				.setUser(user)
				.setPriority(new Priority(Constants.Priority.API_SUBREDDIT_INVIDIVUAL))
				.setDownloadStrategy(DownloadStrategyAlways.INSTANCE)
				.setFileType(Constants.FileType.SUBREDDIT_LIST)
				.setQueueType(CacheRequest.DownloadQueueType.REDDIT_API)
				.setRequestMethod(CacheRequest.RequestMethod.GET)
				.setContext(context)
				.setCache(true)
				.setCallbacks(new CacheRequestJSONParser(
						context, new CacheRequestJSONParser.Listener() {
							@Override
							public void onJsonParsed(
									@NonNull final JsonValue result,
									final TimestampUTC timestamp,
									@NonNull final UUID session, final boolean fromCache) {

								try {

									final HashSet<String> output = new HashSet<>();
									final ArrayList<RedditSubreddit> toWrite = new ArrayList<>();

									final JsonObject redditListing =
											result.asObject().getObject("data");

									final JsonArray subreddits =
											redditListing.getArray("children");

									if(type == RedditSubredditManager.SubredditListType.SUBSCRIBED
											&& subreddits.size() == 0
											&& after == null) {
										performRequest(
												RedditSubredditManager.SubredditListType.DEFAULTS,
												TimestampBound.ANY,
												handler);
										return;
									}

									for(final JsonValue v : subreddits) {
										final RedditThing thing = v.asObject(RedditThing.class);
										final RedditSubreddit subreddit = thing.asSubreddit();

										subreddit.downloadTime = timestamp.toUtcMs();

										try {
											output.add(subreddit.getCanonicalId().toString());
											toWrite.add(subreddit);
										} catch(final InvalidSubredditNameException e) {
											Log.e(
													"SubredditListRequester",
													"Ignoring invalid subreddit",
													e);
										}

									}

									RedditSubredditManager.getInstance(context, user)
											.offerRawSubredditData(toWrite, timestamp);
									final String receivedAfter = redditListing
											.getString("after");
									if(receivedAfter != null && type != RedditSubredditManager
											.SubredditListType.MOST_POPULAR) {

										doSubredditListRequest(
												type,
												new RequestResponseHandler<
														WritableHashSet,
														RRError>() {
													@Override
													public void onRequestFailed(
															final RRError failureReason) {
														handler.onRequestFailed(failureReason);
													}

													@Override
													public void onRequestSuccess(
															final WritableHashSet result,
															final TimestampUTC timeCached) {
														output.addAll(result.toHashset());
														handler.onRequestSuccess(
																new WritableHashSet(
																		output,
																		timeCached,
																		type.name()),
																timeCached);

														if(after == null) {
															Log.i(
																	"SubredditListRequester",
																	"Got " + output.size()
																			+ " subreddits in " +
																			"multiple requests");
														}
													}
												},
												receivedAfter);

									} else {
										handler.onRequestSuccess(new WritableHashSet(
												output,
												timestamp,
												type.name()), timestamp);

										if(after == null) {
											Log.i("SubredditListRequester", "Got "
													+ output.size() + " subreddits in 1 request");
										}
									}

								} catch(final Exception e) {
									handler.onRequestFailed(General.getGeneralErrorForFailure(
											context,
											CacheRequest.RequestFailureType.PARSE,
											e,
											null,
											uri,
											Optional.of(new FailedRequestBody(result))));
								}
							}

							@Override
							public void onFailure(@NonNull final RRError error) {
								handler.onRequestFailed(error);
							}
						}))
				.build();

		CacheManager.getInstance(context).makeRequest(aboutSubredditCacheRequest);
	}

	@Override
	public void performRequest(
			final Collection<RedditSubredditManager.SubredditListType> keys,
			final TimestampBound timestampBound,
			final RequestResponseHandler<
					HashMap<RedditSubredditManager.SubredditListType, WritableHashSet>,
					RRError> handler) {
		// TODO batch API? or just make lots of requests and build up a hash map?
		throw new UnsupportedOperationException();
	}

	@Override
	public void performWrite(final WritableHashSet value) {
		throw new UnsupportedOperationException();
	}

	@Override
	public void performWrite(final Collection<WritableHashSet> values) {
		throw new UnsupportedOperationException();
	}
}
