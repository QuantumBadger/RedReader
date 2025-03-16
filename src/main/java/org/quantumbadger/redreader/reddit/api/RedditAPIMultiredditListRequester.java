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
import org.quantumbadger.redreader.common.UriString;
import org.quantumbadger.redreader.common.time.TimestampUTC;
import org.quantumbadger.redreader.http.FailedRequestBody;
import org.quantumbadger.redreader.io.CacheDataSource;
import org.quantumbadger.redreader.io.RequestResponseHandler;
import org.quantumbadger.redreader.io.WritableHashSet;
import org.quantumbadger.redreader.jsonwrap.JsonArray;
import org.quantumbadger.redreader.jsonwrap.JsonValue;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.UUID;

public class RedditAPIMultiredditListRequester implements CacheDataSource<
		RedditAPIMultiredditListRequester.Key, WritableHashSet, RRError> {

	public static class Key {
		public static final Key INSTANCE = new Key();

		private Key() {
		}
	}

	private final Context context;
	private final RedditAccount user;

	public RedditAPIMultiredditListRequester(final Context context, final RedditAccount user) {
		this.context = context;
		this.user = user;
	}

	@Override
	public void performRequest(
			final Key key,
			final TimestampBound timestampBound,
			final RequestResponseHandler<WritableHashSet, RRError> handler) {

		if(user.isAnonymous()) {

			final TimestampUTC now = TimestampUTC.now();

			handler.onRequestSuccess(
					new WritableHashSet(
							new HashSet<>(),
							now,
							user.getCanonicalUsername()),
					now);

		} else {
			doRequest(handler);
		}
	}

	private void doRequest(
			final RequestResponseHandler<WritableHashSet, RRError> handler) {

		final UriString uri = Constants.Reddit.getUri(Constants.Reddit.PATH_MULTIREDDITS_MINE);

		final CacheRequest request = new CacheRequest(
				uri,
				user,
				null,
				new Priority(Constants.Priority.API_SUBREDDIT_LIST),
				DownloadStrategyAlways.INSTANCE,
				Constants.FileType.MULTIREDDIT_LIST,
				CacheRequest.DownloadQueueType.REDDIT_API,
				CacheRequest.RequestMethod.GET,
				context,
				new CacheRequestJSONParser(context, new CacheRequestJSONParser.Listener() {
					@Override
					public void onJsonParsed(
							@NonNull final JsonValue result,
							final TimestampUTC timestamp,
							@NonNull final UUID session,
							final boolean fromCache) {

						try {
							final HashSet<String> output = new HashSet<>();

							final JsonArray multiredditList = result.asArray();

							for(final JsonValue multireddit : multiredditList) {
								final String name = multireddit.asObject()
										.getObject("data")
										.getString("name");
								output.add(name);
							}

							handler.onRequestSuccess(new WritableHashSet(
									output,
									timestamp,
									user.getCanonicalUsername()), timestamp);

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
				}));

		CacheManager.getInstance(context).makeRequest(request);
	}

	@Override
	public void performRequest(
			final Collection<Key> keys, final TimestampBound timestampBound,
			final RequestResponseHandler<HashMap<Key, WritableHashSet>,
					RRError> handler) {
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
