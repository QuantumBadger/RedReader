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
import android.util.Log;
import org.saiditnet.redreader.account.RedditAccountManager;
import org.saiditnet.redreader.activities.BugReportActivity;
import org.saiditnet.redreader.cache.CacheManager;
import org.saiditnet.redreader.cache.CacheRequest;
import org.saiditnet.redreader.cache.downloadstrategy.DownloadStrategyIfNotCached;
import org.saiditnet.redreader.common.Constants;
import org.saiditnet.redreader.common.General;

import java.io.IOException;
import java.io.InputStream;
import java.util.Locale;
import java.util.UUID;

public final class RedditVideosAPI {

	private static final String[] PREFERRED_VIDEO_FORMATS = {
			"DASH_480",
			"DASH_2_4_M", // 480p
			"DASH_360",
			"DASH_1_2_M", // 360p
			"DASH_720",
			"DASH_4_8_M", // 720p
			"DASH_240",
			"DASH_600_K", // 240p
			"DASH_1080",
			"DASH_9_6_M"  // 1080p
	};

	public static void getImageInfo(
			final Context context,
			final String imageId,
			final int priority,
			final int listId,
			final GetImageInfoListener listener) {

		final String apiUrl = "https://v.redd.it/" + imageId + "/DASHPlaylist.mpd";

		CacheManager.getInstance(context).makeRequest(new CacheRequest(
				General.uriFromString(apiUrl),
				RedditAccountManager.getAnon(),
				null,
				priority,
				listId,
				DownloadStrategyIfNotCached.INSTANCE,
				Constants.FileType.IMAGE_INFO,
				CacheRequest.DOWNLOAD_QUEUE_IMMEDIATE,
				false,
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
			protected void onFailure(final @RequestFailureType int type, final Throwable t, final Integer status, final String readableMessage) {
				listener.onFailure(type, t, status, readableMessage);
			}

			@Override
			protected void onProgress(final boolean authorizationInProgress, final long bytesRead, final long totalBytes) {}

			@Override
			protected void onSuccess(
					final CacheManager.ReadableCacheFile cacheFile,
					final long timestamp,
					final UUID session,
					final boolean fromCache,
					final String mimetype) {

				try {
					final InputStream is = cacheFile.getInputStream();

					try {
						final String mpd = General.readWholeStreamAsUTF8(is);

						String videoUrl = null;
						String audioUrl = null;

						if(mpd.contains("audio")) {
							audioUrl = "https://v.redd.it/" + imageId + "/audio";
						}

						for(final String format : PREFERRED_VIDEO_FORMATS) {
							if(mpd.contains(format)) {
								videoUrl = "https://v.redd.it/" + imageId + "/" + format;
								break;
							}
						}

						if(videoUrl == null) {
							// Fallback
							videoUrl = "https://v.redd.it/" + imageId + "/DASH_480";
						}

						final ImageInfo result;

						if(audioUrl != null) {
							result = new ImageInfo(videoUrl, audioUrl, ImageInfo.MediaType.VIDEO);
						} else {
							result = new ImageInfo(videoUrl, ImageInfo.MediaType.VIDEO);
						}

						Log.i("RedditVideosAPI", String.format(
								Locale.US,
								"For '%s', got video stream '%s' and audio stream '%s'",
								apiUrl,
								videoUrl,
								audioUrl == null ? "null" : audioUrl));

						listener.onSuccess(result);


					} finally {
						General.closeSafely(is);
					}

				} catch(final IOException e) {
					listener.onFailure(REQUEST_FAILURE_STORAGE, e, null, "Failed to read mpd");
				}
			}
		});
	}
}
