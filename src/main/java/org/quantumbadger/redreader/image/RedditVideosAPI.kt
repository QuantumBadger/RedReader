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
package org.quantumbadger.redreader.image

import android.content.Context
import android.util.Log
import org.quantumbadger.redreader.account.RedditAccountManager
import org.quantumbadger.redreader.cache.CacheManager
import org.quantumbadger.redreader.cache.CacheRequest
import org.quantumbadger.redreader.cache.CacheRequestCallbacks
import org.quantumbadger.redreader.cache.downloadstrategy.DownloadStrategyIfNotCached
import org.quantumbadger.redreader.common.Constants
import org.quantumbadger.redreader.common.General.getGeneralErrorForFailure
import org.quantumbadger.redreader.common.General.readWholeStreamAsUTF8
import org.quantumbadger.redreader.common.GenericFactory
import org.quantumbadger.redreader.common.Optional
import org.quantumbadger.redreader.common.Priority
import org.quantumbadger.redreader.common.RRError
import org.quantumbadger.redreader.common.UriString
import org.quantumbadger.redreader.common.datastream.SeekableInputStream
import org.quantumbadger.redreader.common.time.TimestampUTC
import org.quantumbadger.redreader.http.FailedRequestBody
import java.io.IOException
import java.util.UUID
import java.util.concurrent.atomic.AtomicBoolean

object RedditVideosAPI {
    private const val TAG = "RedditVideosAPI"

    private val PREFERRED_VIDEO_FORMATS = arrayOf(
        "DASH_480",
        "DASH_2_4_M",  // 480p
        "DASH_360",
        "DASH_1_2_M",  // 360p
        "DASH_720",
        "DASH_4_8_M",  // 720p
        "DASH_240",
        "DASH_270",
        "DASH_220",
        "DASH_600_K",  // 240p
        "DASH_1080",
        "DASH_9_6_M" // 1080p
    )

    fun getImageInfo(
        context: Context,
        imageId: String,
        priority: Priority,
        listener: GetImageInfoListener
    ) {
        val apiUrl = UriString("https://v.redd.it/$imageId/DASHPlaylist.mpd")

        CacheManager.getInstance(context).makeRequest(
            CacheRequest(
                apiUrl,
                RedditAccountManager.getAnon(),
                null,
                priority,
                DownloadStrategyIfNotCached.INSTANCE,
                Constants.FileType.IMAGE_INFO,
				CacheRequest.DownloadQueueType.IMMEDIATE,
                context,
                object : CacheRequestCallbacks {
                    private val mNotifiedFailure = AtomicBoolean(false)

                    override fun onDataStreamComplete(
                        stream: GenericFactory<SeekableInputStream, IOException>,
                        timestamp: TimestampUTC,
                        session: UUID,
                        fromCache: Boolean,
                        mimetype: String?
                    ) {
                        val mpd = try {
                            stream.create().use(::readWholeStreamAsUTF8)
                        } catch (e: IOException) {
                            Log.e(TAG, "Got exception", e)

                            if (!mNotifiedFailure.getAndSet(true)) {
                                listener.onFailure(
                                    getGeneralErrorForFailure(
                                        context,
										CacheRequest.RequestFailureType.STORAGE,
                                        e,
                                        null,
                                        apiUrl,
                                        FailedRequestBody.from(stream)
                                    )
                                )
                            }

                            return
                        }

                        try {
                            var videoUrl: UriString? = null
                            var audioUrl: UriString? = null

                            // Hacky workaround -- we should parse the MPD
                            val possibleFiles = arrayOf(
                                "DASH_AUDIO_128.mp4",
                                "DASH_AUDIO_64.mp4",
                                "DASH_AUDIO.mp4",
                                "DASH_audio_128.mp4",
                                "DASH_audio_64.mp4",
                                "DASH_audio.mp4",
                                "audio"
                            )

                            for (file in possibleFiles) {
                                if (mpd.contains(file)) {
                                    audioUrl = UriString("https://v.redd.it/$imageId/$file")
                                    break
                                }
                            }

                            for (format in PREFERRED_VIDEO_FORMATS) {
                                if (mpd.contains("$format.mp4")) {
                                    videoUrl = UriString("https://v.redd.it/$imageId/$format.mp4")
                                    break
                                }

                                if (mpd.contains(format)) {
                                    videoUrl = UriString("https://v.redd.it/$imageId/$format")
                                    break
                                }
                            }

                            if (videoUrl == null) {
                                // Fallback
                                videoUrl = UriString("https://v.redd.it/$imageId/DASH_480.mp4")
                            }

                            val result = if (audioUrl != null) {
                                ImageInfo(
                                    original = ImageUrlInfo(videoUrl),
                                    urlAudioStream = audioUrl,
                                    mediaType = ImageInfo.MediaType.VIDEO,
                                    hasAudio = ImageInfo.HasAudio.HAS_AUDIO
                                )
                            } else {
                                ImageInfo(
									original = ImageUrlInfo(videoUrl),
                                    mediaType = ImageInfo.MediaType.VIDEO,
                                    hasAudio = ImageInfo.HasAudio.NO_AUDIO
                                )
                            }

                            listener.onSuccess(result)
                        } catch (e: Exception) {
                            Log.e(TAG, "Got exception", e)

                            if (!mNotifiedFailure.getAndSet(true)) {
                                listener.onFailure(
                                    getGeneralErrorForFailure(
                                        context,
										CacheRequest.RequestFailureType.STORAGE,
                                        e,
                                        null,
                                        apiUrl,
                                        Optional.of(FailedRequestBody(mpd))
                                    )
                                )
                            }
                        }
                    }

                    override fun onFailure(error: RRError) {
                        if (!mNotifiedFailure.getAndSet(true)) {
                            listener.onFailure(error)
                        }
                    }
                })
        )
    }
}
