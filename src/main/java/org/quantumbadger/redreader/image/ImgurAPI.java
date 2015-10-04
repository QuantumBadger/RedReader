package org.quantumbadger.redreader.image;

import android.content.Context;
import org.apache.commons.lang3.StringEscapeUtils;
import org.apache.http.StatusLine;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.activities.BugReportActivity;
import org.quantumbadger.redreader.cache.CacheManager;
import org.quantumbadger.redreader.cache.CacheRequest;
import org.quantumbadger.redreader.cache.RequestFailureType;
import org.quantumbadger.redreader.common.Constants;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.jsonwrap.JsonBufferedArray;
import org.quantumbadger.redreader.jsonwrap.JsonBufferedObject;
import org.quantumbadger.redreader.jsonwrap.JsonValue;

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
			this.images = new ArrayList<ImageInfo>(images);
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
			final ArrayList<ImageInfo> images = new ArrayList<ImageInfo>();

			for(final JsonValue imageJson : imagesJson) {
				images.add(ImageInfo.parse(imageJson.asObject()));
			}

			return new AlbumInfo(id, title, description, images);
		}
	}

	public static class ImageInfo {

		public final String urlOriginal;
		public final String urlBigSquare;

		public final String title;
		public final String caption;

		public final String type;
		public final Boolean isAnimated;

		public final Long width;
		public final Long height;
		public final Long size;

		public ImageInfo(final String urlOriginal) {

			this.urlOriginal = urlOriginal;

			urlBigSquare = null;
			title = null;
			caption = null;
			type = null;
			isAnimated = null;
			width = null;
			height = null;
			size = null;
		}

		public ImageInfo(
				final String urlOriginal,
				final String urlBigSquare,
				final String title,
				final String caption,
				final String type,
				final Boolean isAnimated,
				final Long width,
				final Long height,
				final Long size) {

			this.urlOriginal = urlOriginal;
			this.urlBigSquare = urlBigSquare;
			this.title = title;
			this.caption = caption;
			this.type = type;
			this.isAnimated = isAnimated;
			this.width = width;
			this.height = height;
			this.size = size;
		}

		public static ImageInfo parse(final JsonBufferedObject object)
				throws IOException, InterruptedException {

			final JsonBufferedObject image = object.getObject("image");
			final JsonBufferedObject links = object.getObject("links");

			String urlOriginal = null;
			String urlBigSquare = null;
			String title = null;
			String caption = null;
			String type = null;
			boolean isAnimated = false;
			Long width = null;
			Long height = null;
			Long size = null;

			if(image != null) {
				title = image.getString("title");
				caption = image.getString("caption");
				type = image.getString("type");
				isAnimated = "true".equals(image.getString("animated"));
				width = image.getLong("width");
				height = image.getLong("height");
				size = image.getLong("size");
			}

			if(links != null) {
				urlOriginal = links.getString("original");
				if(urlOriginal != null) urlOriginal = urlOriginal.replace(".gif", ".webm");

				urlBigSquare = links.getString("big_square");
			}
			
			if(title != null) {
				title = StringEscapeUtils.unescapeHtml4(title);
			}

			if(caption != null) {
				caption = StringEscapeUtils.unescapeHtml4(caption);
			}

			return new ImageInfo(
					urlOriginal,
					urlBigSquare,
					title,
					caption,
					type,
					isAnimated,
					width,
					height,
					size);
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
				CacheRequest.DownloadType.IF_NECESSARY,
				Constants.FileType.IMAGE_INFO,
				false,
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
			protected void onFailure(final RequestFailureType type, final Throwable t, final StatusLine status, final String readableMessage) {
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
					listener.onFailure(RequestFailureType.PARSE, t, null, "Imgur data parse failed");
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
				CacheRequest.DownloadType.IF_NECESSARY,
				Constants.FileType.IMAGE_INFO,
				false,
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
			protected void onFailure(final RequestFailureType type, final Throwable t, final StatusLine status, final String readableMessage) {
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
					listener.onSuccess(ImageInfo.parse(outer));

				} catch(Throwable t) {
					listener.onFailure(RequestFailureType.PARSE, t, null, "Imgur data parse failed");
				}
			}
		});
	}
}
