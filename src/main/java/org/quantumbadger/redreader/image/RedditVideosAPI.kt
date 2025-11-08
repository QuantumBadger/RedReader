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

							val mpdParseResult = parseMPD(mpd)

							if (mpdParseResult.video == null) {
								if (!mNotifiedFailure.getAndSet(true)) {
									listener.onFailure(
										getGeneralErrorForFailure(
											context,
											CacheRequest.RequestFailureType.PARSE,
											null,
											null,
											apiUrl,
											Optional.of(FailedRequestBody(mpd))
										)
									)
								}
								return
							}

							fun fileUrl(filename: String) =
								UriString("https://v.redd.it/$imageId/$filename")

							val result = ImageInfo(
								original = ImageUrlInfo(
									url = fileUrl(mpdParseResult.video.filename),
									size = mpdParseResult.video.let {
										if (it.width != null && it.height != null) {
											ImageSize(width = it.width, height = it.height)
										} else {
											null
										}
									}
								),
								urlAudioStream = mpdParseResult.audio?.filename?.let(::fileUrl),
								mediaType = ImageInfo.MediaType.VIDEO,
								hasAudio = if (mpdParseResult.audio != null) {
									ImageInfo.HasAudio.HAS_AUDIO
								} else {
									ImageInfo.HasAudio.NO_AUDIO
								}
							)

							listener.onSuccess(result)
						} catch (e: Exception) {
							Log.e(TAG, "Got exception", e)

							if (!mNotifiedFailure.getAndSet(true)) {
								listener.onFailure(
									getGeneralErrorForFailure(
										context,
										CacheRequest.RequestFailureType.PARSE,
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
