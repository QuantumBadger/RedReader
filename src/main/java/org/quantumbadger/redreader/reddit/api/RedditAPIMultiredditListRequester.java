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
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.cache.CacheManager;
import org.quantumbadger.redreader.cache.CacheRequest;
import org.quantumbadger.redreader.cache.downloadstrategy.DownloadStrategyAlways;
import org.quantumbadger.redreader.common.Constants;
import org.quantumbadger.redreader.common.RRTime;
import org.quantumbadger.redreader.common.TimestampBound;
import org.quantumbadger.redreader.io.CacheDataSource;
import org.quantumbadger.redreader.io.RequestResponseHandler;
import org.quantumbadger.redreader.io.WritableHashSet;
import org.quantumbadger.redreader.jsonwrap.JsonBufferedArray;
import org.quantumbadger.redreader.jsonwrap.JsonValue;

import java.net.URI;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.UUID;

public class RedditAPIMultiredditListRequester implements CacheDataSource<
		RedditAPIMultiredditListRequester.Key, WritableHashSet, SubredditRequestFailure> {

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
			final RequestResponseHandler<WritableHashSet, SubredditRequestFailure> handler) {

		if(user.isAnonymous()) {

			final long now = RRTime.utcCurrentTimeMillis();

			handler.onRequestSuccess(
					new WritableHashSet(
							new HashSet<String>(),
							now,
							user.getCanonicalUsername()),
					now);

		} else {
			doRequest(handler);
		}
	}

	private void doRequest(
			final RequestResponseHandler<WritableHashSet, SubredditRequestFailure> handler) {

		final URI uri = Constants.Reddit.getUri(Constants.Reddit.PATH_MULTIREDDITS_MINE);

		final CacheRequest request = new CacheRequest(
				uri,
				user,
				null,
				Constants.Priority.API_SUBREDDIT_LIST,
				0,
				DownloadStrategyAlways.INSTANCE,
				Constants.FileType.MULTIREDDIT_LIST,
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
						"Internal error",
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
					@RequestFailureType final int type,
					final Throwable t,
					final Integer status,
					final String readableMessage) {
				handler.onRequestFailed(new SubredditRequestFailure(
						type,
						t,
						status,
						readableMessage,
						url.toString()));
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
					final HashSet<String> output = new HashSet<>();

					final JsonBufferedArray multiredditList = result.asArray();

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
					handler.onRequestFailed(
							new SubredditRequestFailure(
									CacheRequest.REQUEST_FAILURE_PARSE,
									e,
									null,
									"Parse error",
									url.toString()));
				}
			}
		};

		CacheManager.getInstance(context).makeRequest(request);
	}

	@Override
	public void performRequest(
			final Collection<Key> keys, final TimestampBound timestampBound,
			final RequestResponseHandler<HashMap<Key, WritableHashSet>,
					SubredditRequestFailure> handler) {
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
