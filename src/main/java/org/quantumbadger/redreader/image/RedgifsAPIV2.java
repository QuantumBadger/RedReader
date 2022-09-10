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
import androidx.annotation.Nullable;
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
import org.quantumbadger.redreader.common.RRTime;
import org.quantumbadger.redreader.common.StringUtils;
import org.quantumbadger.redreader.common.TimestampBound;
import org.quantumbadger.redreader.http.FailedRequestBody;
import org.quantumbadger.redreader.http.PostField;
import org.quantumbadger.redreader.http.body.HTTPRequestBodyPostFields;
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
			return new AuthToken(token, SystemClock.uptimeMillis() + RRTime.minsToMs(10));
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

		final String apiUrl = "https://api.redgifs.com/v2/gifs/"
				+ StringUtils.asciiLowercase(imageId);

		CacheManager.getInstance(context).makeRequest(new CacheRequest(
				General.uriFromString(apiUrl),
				RedditAccountManager.getAnon(),
				null,
				priority,
				// RedGifs V2 links expire after an undocumented period of time
				new DownloadStrategyIfTimestampOutsideBounds(
						TimestampBound.notOlderThan(RRTime.minsToMs(10))),
				Constants.FileType.IMAGE_INFO,
				CacheRequest.DOWNLOAD_QUEUE_REDGIFS_API_V2,
				context,
				new CacheRequestJSONParser(context, new CacheRequestJSONParser.Listener() {
					@Override
					public void onJsonParsed(
							@NonNull final JsonValue result,
							final long timestamp,
							@NonNull final UUID session,
							final boolean fromCache) {

						try {
							listener.onSuccess(ImageInfo.parseRedgifsV2(result
									.getObjectAtPath("gif")
									.orThrow(() -> new RuntimeException("No element 'gif'"))));

							Log.i(TAG, "Got RedGifs v2 metadata");

						} catch(final Throwable t) {
							listener.onFailure(
									CacheRequest.REQUEST_FAILURE_PARSE,
									t,
									null,
									"Redgifs V2 data parse failed",
									Optional.of(new FailedRequestBody(result)));
						}
					}

					@Override
					public void onFailure(
							final int type,
							@Nullable final Throwable t,
							@Nullable final Integer httpStatus,
							@Nullable final String readableMessage,
							@NonNull final Optional<FailedRequestBody> body) {

						listener.onFailure(
								type,
								t,
								httpStatus,
								readableMessage,
								body);
					}
				})));

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

		CacheManager.getInstance(context).makeRequest(new CacheRequest(
				General.uriFromString("https://api.redgifs.com/v2/oauth/client"),
				RedditAccountManager.getAnon(),
				null,
				priority,
				DownloadStrategyAlways.INSTANCE,
				Constants.FileType.IMAGE_INFO,
				CacheRequest.DOWNLOAD_QUEUE_IMMEDIATE,
				new HTTPRequestBodyPostFields(
						new PostField("grant_type", "client_credentials"),
						new PostField(
								Constants.OA_CI,
								"1828d09da4e-1011-a880-0005-d2ecbe8daab3"),
						new PostField(
								Constants.OA_CS,
								"yCarP8TUpIr6J2W8YW+vgSRb8HuBd9koW/nkPtsQaP8=")
				),
				context,
				new CacheRequestJSONParser(context, new CacheRequestJSONParser.Listener() {
					@Override
					public void onJsonParsed(
							@NonNull final JsonValue result,
							final long timestamp,
							@NonNull final UUID session,
							final boolean fromCache) {

						final Optional<String> accessToken
								= result.getStringAtPath("access_token");

						if(accessToken.isEmpty()) {
							Log.i(TAG, "Failed to get RedGifs v2 token: result not present");
							listener.onFailure(
									CacheRequest.REQUEST_FAILURE_REQUEST,
									null,
									null,
									"Failed to get RedGifs v2 token: result not present",
									Optional.of(new FailedRequestBody(result)));
							return;
						}

						Log.i(TAG, "Got RedGifs v2 token");

						TOKEN.set(AuthToken.expireIn10Mins(accessToken.get()));

						requestMetadata(context, imageId, priority, listener);
					}

					@Override
					public void onFailure(
							final int type,
							@Nullable final Throwable t,
							@Nullable final Integer httpStatus,
							@Nullable final String readableMessage,
							@NonNull final Optional<FailedRequestBody> body) {

						Log.i(TAG, "Failed to get RedGifs v2 token");
						listener.onFailure(type, t, httpStatus, readableMessage, body);
					}
				})

		));
	}


}
