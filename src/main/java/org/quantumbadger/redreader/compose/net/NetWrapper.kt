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

package org.quantumbadger.redreader.compose.net

import android.graphics.BitmapFactory
import android.util.Log
import androidx.compose.runtime.Composable
import androidx.compose.runtime.DisposableEffect
import androidx.compose.runtime.Immutable
import androidx.compose.runtime.LaunchedEffect
import androidx.compose.runtime.State
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableIntStateOf
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.setValue
import androidx.compose.ui.graphics.ImageBitmap
import androidx.compose.ui.graphics.asImageBitmap
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.res.stringResource
import androidx.core.graphics.scale
import org.quantumbadger.redreader.R
import org.quantumbadger.redreader.account.RedditAccount
import org.quantumbadger.redreader.account.RedditAccountId
import org.quantumbadger.redreader.account.RedditAccountManager
import org.quantumbadger.redreader.cache.CacheManager
import org.quantumbadger.redreader.cache.CacheRequest
import org.quantumbadger.redreader.cache.CacheRequest.DownloadQueueType
import org.quantumbadger.redreader.cache.CacheRequestCallbacks
import org.quantumbadger.redreader.cache.downloadstrategy.DownloadStrategy
import org.quantumbadger.redreader.cache.downloadstrategy.DownloadStrategyIfNotCached
import org.quantumbadger.redreader.common.AndroidCommon
import org.quantumbadger.redreader.common.Constants
import org.quantumbadger.redreader.common.GenericFactory
import org.quantumbadger.redreader.common.LinkHandler
import org.quantumbadger.redreader.common.Priority
import org.quantumbadger.redreader.common.RRError
import org.quantumbadger.redreader.common.UriString
import org.quantumbadger.redreader.common.datastream.SeekableInputStream
import org.quantumbadger.redreader.common.invokeIf
import org.quantumbadger.redreader.common.time.TimestampUTC
import org.quantumbadger.redreader.compose.ctx.GlobalNetworkRetry
import org.quantumbadger.redreader.compose.ctx.LocalRedditUser
import org.quantumbadger.redreader.image.AlbumInfo
import org.quantumbadger.redreader.image.GetAlbumInfoListener
import org.quantumbadger.redreader.image.ImageSize
import java.io.IOException
import java.util.UUID
import kotlin.math.max
import kotlin.math.roundToInt

@Immutable
sealed interface NetRequestStatus<out R> {
	@Immutable
	data object Connecting : NetRequestStatus<Nothing>

	@Immutable
	data class Downloading(val fractionComplete: Float) : NetRequestStatus<Nothing>

	@Immutable
	data class Failed(val error: RRError) : NetRequestStatus<Nothing>

	@Immutable
	data class Success<R>(
		val result: R
	) : NetRequestStatus<R>
}

@Immutable
class FileRequestMetadata(
	val streamFactory: GenericFactory<SeekableInputStream, IOException>,
	val timestamp: TimestampUTC?,
	val session: UUID,
	val fromCache: Boolean,
	val mimetype: String?,
)

@Immutable
class FileRequestResult<R>(
	val metadata: FileRequestMetadata?,
	val data: R
)

@Composable
fun fetchAlbum(
	uri: UriString,
	user: RedditAccountId = LocalRedditUser.current,
): State<NetRequestStatus<AlbumInfo>> {
	val state =
		remember { mutableStateOf<NetRequestStatus<AlbumInfo>>(NetRequestStatus.Connecting) }

	// Prevent conflicting updates to state
	val currentRequest = remember { mutableIntStateOf(0) }

	val context = LocalContext.current.applicationContext

	LaunchedEffect(uri, user) {

		state.value = NetRequestStatus.Connecting

		val thisRequest = ++currentRequest.intValue

		LinkHandler.getAlbumInfo(
			context,
			uri,
			Priority(Constants.Priority.IMAGE_VIEW),
			object : GetAlbumInfoListener {
				override fun onFailure(error: RRError) {
					AndroidCommon.runOnUiThread {
						if (thisRequest == currentRequest.intValue) {
							state.value = NetRequestStatus.Failed(error)
						}
					}
				}

				override fun onSuccess(info: AlbumInfo) {
					AndroidCommon.runOnUiThread {
						if (thisRequest == currentRequest.intValue) {
							state.value = NetRequestStatus.Success(
								info
							)
						}
					}
				}

				override fun onGalleryRemoved() {
					onFailure(
						RRError(
							context.getString(R.string.image_gallery_removed_title),
							context.getString(R.string.image_gallery_removed_message),
							reportable = false,
							url = uri,
						)
					)
				}

				override fun onGalleryDataNotPresent() {
					onFailure(
						RRError(
							context.getString(R.string.image_gallery_no_data_present_title),
							context.getString(R.string.image_gallery_no_data_present_message),
							reportable = true,
							url = uri
						)
					)
				}

			}
		)
	}

	return state
}

@Composable
fun fetchImage(
	uri: UriString,
	user: RedditAccountId = LocalRedditUser.current,
	scaleToMaxAxis: Int = 2048
): State<NetRequestStatus<FileRequestResult<ImageBitmap>>> {

	val TAG = "NetWrapper:fetchImage"

	val context = LocalContext.current.applicationContext

	val filter: (FileRequestMetadata) -> NetRequestStatus<FileRequestResult<ImageBitmap>> =
		remember(uri, user) {
			{
				try {
					val result = BitmapFactory.decodeStream(it.streamFactory.create())
						?: throw RuntimeException("Decoded bitmap was null")

					val maxAxis = max(result.width, result.height)

					val scaledResult = result.invokeIf(maxAxis > scaleToMaxAxis) {

						val newSize = if (result.width > result.height) {
							val scale = scaleToMaxAxis / result.width.toFloat()
							ImageSize(
								scaleToMaxAxis,
								(result.height.toFloat() * scale).roundToInt()
							)
						} else {
							val scale = scaleToMaxAxis / result.height.toFloat()
							ImageSize((result.width.toFloat() * scale).roundToInt(), scaleToMaxAxis)
						}

						Log.i(
							TAG,
							"Scaling image from ${result.width}x${result.height} to $newSize"
						)

						scale(newSize.width, newSize.height)
					}

					NetRequestStatus.Success(
						FileRequestResult(
							metadata = it,
							data = scaledResult.asImageBitmap()
						)
					)
				} catch (e: Exception) {
					NetRequestStatus.Failed(
						RRError(
							title = context.getString(R.string.error_image_decode_failed),
							url = uri,
							t = e
						)
					)
				}
			}
		}

	return fetchFile(
		uri = uri,
		user = user,
		priority = Priority(Constants.Priority.IMAGE_VIEW),
		downloadStrategy = DownloadStrategyIfNotCached.INSTANCE,
		fileType = Constants.FileType.IMAGE,
		queueType = CacheRequest.DownloadQueueType.IMMEDIATE,
		cache = true,
		filter = filter
	)
}

// TODO make this a member of an interface, provided in a CompositionLocal, to allow mocking for previews/etc?
@Composable
private fun <T> fetchFile(
	uri: UriString,
	user: RedditAccountId,
	priority: Priority,
	downloadStrategy: DownloadStrategy,
	fileType: Int,
	queueType: DownloadQueueType,
	cache: Boolean,
	filter: ((FileRequestMetadata) -> NetRequestStatus<FileRequestResult<T>>)
): State<NetRequestStatus<FileRequestResult<T>>> {

	val state =
		remember { mutableStateOf<NetRequestStatus<FileRequestResult<T>>>(NetRequestStatus.Connecting) }

	// Prevent conflicting updates to state
	val currentRequest = remember { mutableIntStateOf(0) }

	val currentGlobalRetry by GlobalNetworkRetry
	var currentLocalRetry by remember { mutableIntStateOf(currentGlobalRetry) }

	val context = LocalContext.current

	val account: RedditAccount =
		RedditAccountManager.getInstance(context).getAccount(user.canonicalUsername)
			?: run {
				state.value = NetRequestStatus.Failed(
					RRError(
						title = stringResource(R.string.error_invalid_account_title),
						message = stringResource(R.string.error_invalid_account_message),
					)
				)

				return state
			}

	LaunchedEffect(
		currentLocalRetry,
		currentGlobalRetry,
		uri, user, priority, downloadStrategy, fileType, queueType, cache, filter
	) {

		if (state.value is NetRequestStatus.Failed && currentLocalRetry != currentGlobalRetry) {
			state.value = NetRequestStatus.Connecting

			// Visual feedback
			AndroidCommon.UI_THREAD_HANDLER.postDelayed({
				currentLocalRetry = currentGlobalRetry
			}, 2000)
		}
	}

	DisposableEffect(
		currentLocalRetry,
		uri,
		user,
		priority,
		downloadStrategy,
		fileType,
		queueType,
		cache,
		filter
	) {

		state.value = NetRequestStatus.Connecting

		val thisRequest = ++currentRequest.intValue

		val req = CacheRequest(
			uri,
			account,
			null,
			priority,
			downloadStrategy,
			fileType,
			queueType,
			cache,
			context,
			object : CacheRequestCallbacks {

				var done = false

				val active
					get() = !done && thisRequest == currentRequest.intValue

				override fun onFailure(error: RRError) {
					AndroidCommon.runOnUiThread {
						if (active) {
							state.value =
								NetRequestStatus.Failed(error.invokeIf(error.resolution == null) {
									error.copy(resolution = RRError.Resolution.RETRY)
								})
							done = true
						}
					}
				}

				override fun onDataStreamComplete(
					streamFactory: GenericFactory<SeekableInputStream, IOException>,
					timestamp: TimestampUTC?,
					session: UUID,
					fromCache: Boolean,
					mimetype: String?
				) {
					val result = filter(
						FileRequestMetadata(
							streamFactory,
							timestamp,
							session,
							fromCache,
							mimetype
						)
					)

					AndroidCommon.runOnUiThread {
						if (active) {
							state.value = result
							done = true
						}
					}
				}

				override fun onProgress(
					authorizationInProgress: Boolean,
					bytesRead: Long,
					totalBytes: Long
				) {
					AndroidCommon.runOnUiThread {
						if (active && totalBytes > 0) {
							state.value =
								NetRequestStatus.Downloading(bytesRead.toFloat() / totalBytes.toFloat())
						}
					}
				}
			}
		)

		CacheManager.getInstance(context).makeRequest(req)

		onDispose {
			req.cancel()
		}
	}

	return state
}
