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

package org.quantumbadger.redreader.image;

import android.content.Context;
import android.os.SystemClock;
import android.util.Log;

import androidx.annotation.NonNull;

import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.cache.CacheManager;
import org.quantumbadger.redreader.cache.CacheRequest;
import org.quantumbadger.redreader.cache.CacheRequestJSONParser;
import org.quantumbadger.redreader.cache.downloadstrategy.DownloadStrategyAlways;
import org.quantumbadger.redreader.cache.downloadstrategy.DownloadStrategyIfTimestampOutsideBounds;
import org.quantumbadger.redreader.common.Constants;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.Optional;
import org.quantumbadger.redreader.common.Priority;
import org.quantumbadger.redreader.common.RRError;
import org.quantumbadger.redreader.common.StringUtils;
import org.quantumbadger.redreader.common.TimestampBound;
import org.quantumbadger.redreader.common.UriString;
import org.quantumbadger.redreader.common.time.TimeDuration;
import org.quantumbadger.redreader.common.time.TimestampUTC;
import org.quantumbadger.redreader.http.FailedRequestBody;
import org.quantumbadger.redreader.http.PostField;
import org.quantumbadger.redreader.http.body.HTTPRequestBody;
import org.quantumbadger.redreader.jsonwrap.JsonValue;

import java.util.UUID;
import java.util.concurrent.atomic.AtomicReference;

public final class RedgifsAPIV2 {

	private static final String TAG = "RedgifsAPIV2";

	private static final AtomicReference<AuthToken> TOKEN = new AtomicReference<>(
			new AuthToken("", 0));

	private static final class AuthToken {
		@NonNull public final String token;
		private final long expireAt;

		private AuthToken(@NonNull final String token, final long expireAt) {
			this.token = token;
			this.expireAt = expireAt;
		}

		public static AuthToken expireIn10Mins(@NonNull final String token) {
			return new AuthToken(token, SystemClock.uptimeMillis() + 10L * 60 * 1000);
		}

		public boolean isValid() {
			return !token.isEmpty() && expireAt > SystemClock.uptimeMillis();
		}
	}

	public static String getLatestToken() {
		return TOKEN.get().token;
	}

	private static void requestMetadata(
			final Context context,
			final String imageId,
			@NonNull final Priority priority,
			final GetImageInfoListener listener) {

		final UriString apiUrl = new UriString("https://api.redgifs.com/v2/gifs/"
				+ StringUtils.asciiLowercase(imageId));

		CacheManager.getInstance(context).makeRequest(new CacheRequest.Builder()
				.setUrl(apiUrl)
				.setUser(RedditAccountManager.getAnon())
				.setPriority(priority)
				// RedGifs V2 links expire after an undocumented period of time
				.setDownloadStrategy(new DownloadStrategyIfTimestampOutsideBounds(
						TimestampBound.notOlderThan(TimeDuration.minutes(10))))
				.setFileType(Constants.FileType.IMAGE_INFO)
				.setQueueType(CacheRequest.DownloadQueueType.REDGIFS_API_V2)
				.setRequestMethod(CacheRequest.RequestMethod.GET)
				.setContext(context)
				.setCache(true)
				.setCallbacks(
					new CacheRequestJSONParser(context, new CacheRequestJSONParser.Listener() {
						@Override
						public void onJsonParsed(
								@NonNull final JsonValue result,
								final TimestampUTC timestamp,
								@NonNull final UUID session,
								final boolean fromCache) {

							try {
								listener.onSuccess(ImageInfo.parseRedgifsV2(result
										.getObjectAtPath("gif")
										.orThrow(() -> new RuntimeException("No element 'gif'"))));

								Log.i(TAG, "Got RedGifs v2 metadata");

							} catch(final Throwable t) {
								listener.onFailure(General.getGeneralErrorForFailure(
										context,
										CacheRequest.RequestFailureType.PARSE,
										t,
										null,
										apiUrl,
										Optional.of(new FailedRequestBody(result))));
							}
						}

						@Override
						public void onFailure(@NonNull final RRError error) {
							listener.onFailure(error);
						}
					}))
				.build());

	}

	public static void getImageInfo(
			final Context context,
			final String imageId,
			@NonNull final Priority priority,
			final GetImageInfoListener listener) {

		if(TOKEN.get().isValid()) {
			Log.i(TAG, "Existing token still valid");
			requestMetadata(context, imageId, priority, listener);
			return;
		}

		Log.i(TAG, "Retrieving new token");

		final UriString apiUrl = new UriString("https://api.redgifs.com/v2/oauth/client");

		CacheManager.getInstance(context).makeRequest(new CacheRequest.Builder()
				.setUrl(apiUrl)
				.setUser(RedditAccountManager.getAnon())
				.setPriority(priority)
				.setDownloadStrategy(DownloadStrategyAlways.INSTANCE)
				.setFileType(Constants.FileType.IMAGE_INFO)
				.setQueueType(CacheRequest.DownloadQueueType.IMMEDIATE)
				.setRequestMethod(CacheRequest.RequestMethod.POST)
				.setRequestBody(new HTTPRequestBody.PostFields(
						new PostField("grant_type", "client_credentials"),
						new PostField(
								Constants.OA_CI,
								"1828d09da4e-1011-a880-0005-d2ecbe8daab3"),
						new PostField(
								Constants.OA_CS,
								"yCarP8TUpIr6J2W8YW+vgSRb8HuBd9koW/nkPtsQaP8=")))
				.setContext(context)
				.setCallbacks(
					new CacheRequestJSONParser(context, new CacheRequestJSONParser.Listener() {
						@Override
						public void onJsonParsed(
								@NonNull final JsonValue result,
								final TimestampUTC timestamp,
								@NonNull final UUID session,
								final boolean fromCache) {

							final Optional<String> accessToken
									= result.getStringAtPath("access_token");

							if(accessToken.isEmpty()) {
								Log.i(TAG, "Failed to get RedGifs v2 token: result not present");
								listener.onFailure(General.getGeneralErrorForFailure(
										context,
										CacheRequest.RequestFailureType.REQUEST,
										null,
										null,
										apiUrl,
										Optional.of(new FailedRequestBody(result))));
								return;
							}

							Log.i(TAG, "Got RedGifs v2 token");

							TOKEN.set(AuthToken.expireIn10Mins(accessToken.get()));

							requestMetadata(context, imageId, priority, listener);
						}

						@Override
						public void onFailure(@NonNull final RRError error) {

							Log.i(TAG, "Failed to get RedGifs v2 token");
							listener.onFailure(error);
						}
					}))
				.build());
	}


}
