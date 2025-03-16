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

import androidx.annotation.NonNull;

import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.cache.CacheManager;
import org.quantumbadger.redreader.cache.CacheRequest;
import org.quantumbadger.redreader.cache.CacheRequestJSONParser;
import org.quantumbadger.redreader.cache.downloadstrategy.DownloadStrategyIfNotCached;
import org.quantumbadger.redreader.common.Constants;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.Optional;
import org.quantumbadger.redreader.common.Priority;
import org.quantumbadger.redreader.common.RRError;
import org.quantumbadger.redreader.common.UriString;
import org.quantumbadger.redreader.common.time.TimestampUTC;
import org.quantumbadger.redreader.http.FailedRequestBody;
import org.quantumbadger.redreader.jsonwrap.JsonObject;
import org.quantumbadger.redreader.jsonwrap.JsonValue;

import java.util.UUID;

public final class StreamableAPI {

	public static void getImageInfo(
			final Context context,
			final String imageId,
			@NonNull final Priority priority,
			final GetImageInfoListener listener) {

		final UriString apiUrl = new UriString("https://api.streamable.com/videos/" + imageId);

		CacheManager.getInstance(context).makeRequest(new CacheRequest.Builder()
				.setUrl(apiUrl)
				.setUser(RedditAccountManager.getAnon())
				.setPriority(priority)
				.setDownloadStrategy(DownloadStrategyIfNotCached.INSTANCE)
				.setFileType(Constants.FileType.IMAGE_INFO)
				.setQueueType(CacheRequest.DownloadQueueType.IMMEDIATE)
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
									final JsonObject outer = result.asObject();
									listener.onSuccess(ImageInfo.parseStreamable(outer));

								} catch (final Throwable t) {
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
}
