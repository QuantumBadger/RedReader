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

package org.saiditnet.redreader.image;

import android.content.Context;
import org.apache.commons.lang3.StringEscapeUtils;
import org.saiditnet.redreader.account.RedditAccountManager;
import org.saiditnet.redreader.activities.BugReportActivity;
import org.saiditnet.redreader.cache.CacheManager;
import org.saiditnet.redreader.cache.CacheRequest;
import org.saiditnet.redreader.cache.downloadstrategy.DownloadStrategyIfNotCached;
import org.saiditnet.redreader.common.Constants;
import org.saiditnet.redreader.common.General;
import org.saiditnet.redreader.jsonwrap.JsonBufferedArray;
import org.saiditnet.redreader.jsonwrap.JsonBufferedObject;
import org.saiditnet.redreader.jsonwrap.JsonValue;

import java.io.IOException;
import java.util.ArrayList;
import java.util.UUID;

public final class ImgurAPI {

	public static class AlbumInfo {

		public final String id;
		public final String title;
		public final String description;

		public final ArrayList<ImageInfo> images;

		public AlbumInfo(final String id, final String title, final String description, final ArrayList<ImageInfo> images) {
			this.id = id;
			this.title = title;
			this.description = description;
			this.images = new ArrayList<>(images);
		}

		public static AlbumInfo parse(final String id, final JsonBufferedObject object)
				throws IOException, InterruptedException {

			String title = object.getString("title");
			String description = object.getString("description");

			if(title != null) {
				title = StringEscapeUtils.unescapeHtml4(title);
			}

			if(description != null) {
				description = StringEscapeUtils.unescapeHtml4(description);
			}

			final JsonBufferedArray imagesJson = object.getArray("images");
			final ArrayList<ImageInfo> images = new ArrayList<>();

			for(final JsonValue imageJson : imagesJson) {
				images.add(ImageInfo.parseImgur(imageJson.asObject()));
			}

			return new AlbumInfo(id, title, description, images);
		}

		public static AlbumInfo parseV3(final String id, final JsonBufferedObject object)
				throws IOException, InterruptedException {

			String title = object.getString("title");
			String description = object.getString("description");

			if(title != null) {
				title = StringEscapeUtils.unescapeHtml4(title);
			}

			if(description != null) {
				description = StringEscapeUtils.unescapeHtml4(description);
			}

			final JsonBufferedArray imagesJson = object.getArray("images");
			final ArrayList<ImageInfo> images = new ArrayList<>();

			for(final JsonValue imageJson : imagesJson) {
				images.add(ImageInfo.parseImgurV3(imageJson.asObject()));
			}

			return new AlbumInfo(id, title, description, images);
		}
	}

	public static void getAlbumInfo(
			final Context context,
			final String albumId,
			final int priority,
			final int listId,
			final GetAlbumInfoListener listener) {

		final String apiUrl = "https://api.imgur.com/2/album/" + albumId + ".json";

		CacheManager.getInstance(context).makeRequest(new CacheRequest(
				General.uriFromString(apiUrl),
				RedditAccountManager.getAnon(),
				null,
				priority,
				listId,
				DownloadStrategyIfNotCached.INSTANCE,
				Constants.FileType.IMAGE_INFO,
				CacheRequest.DOWNLOAD_QUEUE_IMMEDIATE,
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
					final JsonBufferedObject outer = result.asObject().getObject("album");
					listener.onSuccess(AlbumInfo.parse(albumId, outer));

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
			final GetImageInfoListener listener) {

		final String apiUrl = "https://api.imgur.com/2/image/" + imageId + ".json";

		CacheManager.getInstance(context).makeRequest(new CacheRequest(
				General.uriFromString(apiUrl),
				RedditAccountManager.getAnon(),
				null,
				priority,
				listId,
				DownloadStrategyIfNotCached.INSTANCE,
				Constants.FileType.IMAGE_INFO,
				CacheRequest.DOWNLOAD_QUEUE_IMMEDIATE,
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
					final JsonBufferedObject outer = result.asObject().getObject("image");
					listener.onSuccess(ImageInfo.parseImgur(outer));

				} catch(Throwable t) {
					listener.onFailure(CacheRequest.REQUEST_FAILURE_PARSE, t, null, "Imgur data parse failed");
				}
			}
		});
	}
}
