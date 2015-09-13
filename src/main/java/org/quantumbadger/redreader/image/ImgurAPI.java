package org.quantumbadger.redreader.image;

import android.content.Context;
import org.apache.http.StatusLine;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.activities.BugReportActivity;
import org.quantumbadger.redreader.cache.CacheManager;
import org.quantumbadger.redreader.cache.CacheRequest;
import org.quantumbadger.redreader.cache.RequestFailureType;
import org.quantumbadger.redreader.common.Constants;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.jsonwrap.JsonBufferedObject;
import org.quantumbadger.redreader.jsonwrap.JsonValue;

import java.util.UUID;

public final class ImgurAPI {

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

					final JsonBufferedObject image = outer.getObject("image");
					final JsonBufferedObject links = outer.getObject("links");
					
					String url = null;
					String title = null;
					String caption = null;
					boolean isAnimated = false;
					Long width = null;
					Long height = null;
					
					if(image != null) {
						title = image.getString("title");
						caption = image.getString("caption");
						isAnimated = "true".equals(image.getString("animated"));
						width = image.getLong("width");
						height = image.getLong("height");
					}

					if(links != null) {
						url = links.getString("original");
						if(url != null) url = url.replace(".gif", ".webm");
					}

					listener.onSuccess(url, title, caption, isAnimated, width, height);

				} catch(Throwable t) {
					listener.onFailure(RequestFailureType.PARSE, t, null, "Imgur data parse failed");
				}
			}
		});
	}
}
