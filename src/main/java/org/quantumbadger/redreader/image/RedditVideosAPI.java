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
import android.util.Log;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.cache.CacheManager;
import org.quantumbadger.redreader.cache.CacheRequest;
import org.quantumbadger.redreader.cache.CacheRequestCallbacks;
import org.quantumbadger.redreader.cache.downloadstrategy.DownloadStrategyIfNotCached;
import org.quantumbadger.redreader.common.Constants;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.GenericFactory;
import org.quantumbadger.redreader.common.Optional;
import org.quantumbadger.redreader.common.Priority;
import org.quantumbadger.redreader.common.datastream.SeekableInputStream;
import org.quantumbadger.redreader.http.FailedRequestBody;

import java.io.IOException;
import java.io.InputStream;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicBoolean;

public final class RedditVideosAPI {

	private static final String TAG = "RedditVideosAPI";

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
			@NonNull final Priority priority,
			final GetImageInfoListener listener) {

		final String apiUrl = "https://v.redd.it/" + imageId + "/DASHPlaylist.mpd";

		CacheManager.getInstance(context).makeRequest(new CacheRequest(
				General.uriFromString(apiUrl),
				RedditAccountManager.getAnon(),
				null,
				priority,
				DownloadStrategyIfNotCached.INSTANCE,
				Constants.FileType.IMAGE_INFO,
				CacheRequest.DOWNLOAD_QUEUE_IMMEDIATE,
				context,
				new CacheRequestCallbacks() {

					private final AtomicBoolean mNotifiedFailure
							= new AtomicBoolean(false);

					@Override
					public void onDataStreamComplete(
							@NonNull final GenericFactory<SeekableInputStream, IOException> stream,
							final long timestamp,
							@NonNull final UUID session,
							final boolean fromCache,
							@Nullable final String mimetype) {

						final String mpd;

						try(InputStream is = stream.create()) {
							mpd = new String(General.readWholeStream(is), General.CHARSET_UTF8);
						} catch(final IOException e) {

							Log.e(TAG, "Got exception", e);

							if(!mNotifiedFailure.getAndSet(true)) {
								listener.onFailure(
										CacheRequest.REQUEST_FAILURE_STORAGE,
										e,
										null,
										"Failed to read mpd",
										Optional.empty());
							}

							return;
						}

						try {
							String videoUrl = null;
							String audioUrl = null;

							if(mpd.contains("DASH_audio.mp4")) {
								audioUrl = "https://v.redd.it/" + imageId + "/DASH_audio.mp4";

							} else if(mpd.contains("audio")) {
								audioUrl = "https://v.redd.it/" + imageId + "/audio";
							}

							for(final String format : PREFERRED_VIDEO_FORMATS) {

								if(mpd.contains(format + ".mp4")) {
									videoUrl = "https://v.redd.it/"
											+ imageId
											+ "/"
											+ format
											+ ".mp4";
									break;
								}

								if(mpd.contains(format)) {
									videoUrl = "https://v.redd.it/" + imageId + "/" + format;
									break;
								}
							}

							if(videoUrl == null) {
								// Fallback
								videoUrl = "https://v.redd.it/" + imageId + "/DASH_480.mp4";
							}

							final ImageInfo result;

							if(audioUrl != null) {
								result = new ImageInfo(
										videoUrl,
										audioUrl,
										ImageInfo.MediaType.VIDEO,
										ImageInfo.HasAudio.HAS_AUDIO);
							} else {
								result = new ImageInfo(
										videoUrl,
										ImageInfo.MediaType.VIDEO,
										ImageInfo.HasAudio.NO_AUDIO);
							}

							listener.onSuccess(result);

						} catch(final Exception e) {

							Log.e(TAG, "Got exception", e);

							if(!mNotifiedFailure.getAndSet(true)) {
								listener.onFailure(
										CacheRequest.REQUEST_FAILURE_STORAGE,
										e,
										null,
										"Failed to parse mpd",
										Optional.of(new FailedRequestBody(mpd)));
							}
						}
					}

					@Override
					public void onFailure(
							final int type,
							@Nullable final Throwable t,
							@Nullable final Integer httpStatus,
							@Nullable final String readableMessage,
							@NonNull final Optional<FailedRequestBody> body) {

						if(!mNotifiedFailure.getAndSet(true)) {
							listener.onFailure(
									type,
									t,
									httpStatus,
									readableMessage,
									body);
						}
					}
				}));
	}
}
