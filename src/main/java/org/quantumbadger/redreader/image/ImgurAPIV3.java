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
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.activities.BugReportActivity;
import org.quantumbadger.redreader.cache.CacheManager;
import org.quantumbadger.redreader.cache.CacheRequest;
import org.quantumbadger.redreader.common.Constants;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.jsonwrap.JsonBufferedObject;
import org.quantumbadger.redreader.jsonwrap.JsonValue;

import java.util.UUID;

public final class ImgurAPIV3 {

	public static void getAlbumInfo(
			final Context context,
			final String albumId,
			final int priority,
			final int listId,
			final boolean withAuth,
			final GetAlbumInfoListener listener) {

		final String apiUrl = "https://api.imgur.com/3/album/" + albumId;

		CacheManager.getInstance(context).makeRequest(new CacheRequest(
				General.uriFromString(apiUrl),
				RedditAccountManager.getAnon(),
				null,
				priority,
				listId,
				CacheRequest.DOWNLOAD_IF_NECESSARY,
				Constants.FileType.IMAGE_INFO,
				withAuth
						? CacheRequest.DOWNLOAD_QUEUE_IMGUR_API
						: CacheRequest.DOWNLOAD_QUEUE_IMMEDIATE,
				true,
				false,
				context
		) {
			@Override
			protected void onCallbackException(final Throwable t) {
				BugReportActivity.handleGlobalError(context, t);
			}

			@Override
			protected void onDownloadNecessary() {}

			@Override
			protected void onDownloadStarted() {}

			@Override
			protected void onFailure(final @CacheRequest.RequestFailureType int type, final Throwable t, final Integer status, final String readableMessage) {
				listener.onFailure(type, t, status, readableMessage);
			}

			@Override
			protected void onProgress(final boolean authorizationInProgress, final long bytesRead, final long totalBytes) {}

			@Override
			protected void onSuccess(final CacheManager.ReadableCacheFile cacheFile, final long timestamp, final UUID session, final boolean fromCache, final String mimetype) {}

			@Override
			public void onJsonParseStarted(final JsonValue result, final long timestamp, final UUID session, final boolean fromCache) {

				try {
					final JsonBufferedObject outer = result.asObject().getObject("data");
					listener.onSuccess(ImgurAPI.AlbumInfo.parseV3(albumId, outer));

				} catch(Throwable t) {
					listener.onFailure(CacheRequest.REQUEST_FAILURE_PARSE, t, null, "Imgur data parse failed");
				}
			}
		});
	}

	public static void getImageInfo(
			final Context context,
			final String imageId,
			final int priority,
			final int listId,
			final boolean withAuth,
			final GetImageInfoListener listener) {

		final String apiUrl = "https://api.imgur.com/3/image/" + imageId;

		CacheManager.getInstance(context).makeRequest(new CacheRequest(
				General.uriFromString(apiUrl),
				RedditAccountManager.getAnon(),
				null,
				priority,
				listId,
				CacheRequest.DOWNLOAD_IF_NECESSARY,
				Constants.FileType.IMAGE_INFO,
				withAuth
					? CacheRequest.DOWNLOAD_QUEUE_IMGUR_API
					: CacheRequest.DOWNLOAD_QUEUE_IMMEDIATE,
				true,
				false,
				context
		) {
			@Override
			protected void onCallbackException(final Throwable t) {
				BugReportActivity.handleGlobalError(context, t);
			}

			@Override
			protected void onDownloadNecessary() {}

			@Override
			protected void onDownloadStarted() {}

			@Override
			protected void onFailure(final @CacheRequest.RequestFailureType int type, final Throwable t, final Integer status, final String readableMessage) {
				listener.onFailure(type, t, status, readableMessage);
			}

			@Override
			protected void onProgress(final boolean authorizationInProgress, final long bytesRead, final long totalBytes) {}

			@Override
			protected void onSuccess(final CacheManager.ReadableCacheFile cacheFile, final long timestamp, final UUID session, final boolean fromCache, final String mimetype) {}

			@Override
			public void onJsonParseStarted(final JsonValue result, final long timestamp, final UUID session, final boolean fromCache) {

				try {
					final JsonBufferedObject outer = result.asObject().getObject("data");
					listener.onSuccess(ImageInfo.parseImgurV3(outer));

				} catch(Throwable t) {
					listener.onFailure(CacheRequest.REQUEST_FAILURE_PARSE, t, null, "Imgur data parse failed");
				}
			}
		});
	}
}
