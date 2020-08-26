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
import android.net.Uri;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.activities.BugReportActivity;
import org.quantumbadger.redreader.cache.CacheManager;
import org.quantumbadger.redreader.cache.CacheRequest;
import org.quantumbadger.redreader.cache.downloadstrategy.DownloadStrategyIfNotCached;
import org.quantumbadger.redreader.common.Constants;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.jsonwrap.JsonBufferedObject;
import org.quantumbadger.redreader.jsonwrap.JsonValue;
import org.quantumbadger.redreader.reddit.url.PostCommentListingURL;

import java.util.UUID;

public final class RedditGalleryAPI {

	public static void getAlbumInfo(
			final Context context,
			final String albumUrl,
			final String albumId,
			final int priority,
			final int listId,
			final GetAlbumInfoListener listener) {

		final Uri apiUrl = new PostCommentListingURL(
				null,
				albumId,
				null,
				null,
				null,
				null).generateJsonUri();

		CacheManager.getInstance(context).makeRequest(new CacheRequest(
				General.uriFromString(apiUrl.toString()),
				RedditAccountManager.getInstance(context).getDefaultAccount(),
				null,
				priority,
				listId,
				DownloadStrategyIfNotCached.INSTANCE,
				Constants.FileType.IMAGE_INFO,
				CacheRequest.DOWNLOAD_QUEUE_REDDIT_API,
				true,
				false,
				context) {

			@Override
			protected void onCallbackException(final Throwable t) {
				BugReportActivity.handleGlobalError(context, t);
			}

			@Override
			protected void onDownloadNecessary() {
			}

			@Override
			protected void onDownloadStarted() {
			}

			@Override
			protected void onFailure(
					final @RequestFailureType int type,
					final Throwable t,
					final Integer status,
					final String readableMessage) {
				listener.onFailure(type, t, status, readableMessage);
			}

			@Override
			protected void onProgress(
					final boolean authorizationInProgress,
					final long bytesRead,
					final long totalBytes) {
			}

			@Override
			public void onJsonParseStarted(
					final JsonValue result,
					final long timestamp,
					final UUID session,
					final boolean fromCache) {

				try {
					@SuppressWarnings("ConstantConditions") final JsonBufferedObject
							redditPostData = result.asArray()
							.getObject(0)
							.getObject("data")
							.getArray("children")
							.getObject(0)
							.getObject("data");

					final AlbumInfo album
							= AlbumInfo.parseRedditGallery(albumUrl, redditPostData);

					if(album == null) {

						if(redditPostData.getString("removed_by_category") != null) {
							listener.onGalleryRemoved();
						} else {
							listener.onGalleryDataNotPresent();
						}

					} else {
						listener.onSuccess(album);
					}

				} catch(final Exception e) {
					listener.onFailure(
							CacheRequest.REQUEST_FAILURE_PARSE,
							e,
							null,
							"Reddit gallery data parse failed");
				}
			}

			@Override
			protected void onSuccess(
					final CacheManager.ReadableCacheFile cacheFile,
					final long timestamp,
					final UUID session,
					final boolean fromCache,
					final String mimetype) {

				// Nothing to do here
			}
		});
	}
}
