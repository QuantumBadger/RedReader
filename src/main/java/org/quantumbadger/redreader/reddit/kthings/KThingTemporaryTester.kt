package org.quantumbadger.redreader.reddit.kthings

import android.content.Context
import android.util.Log
import kotlinx.serialization.ExperimentalSerializationApi
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.decodeFromStream
import org.quantumbadger.redreader.account.RedditAccountManager
import org.quantumbadger.redreader.cache.CacheManager
import org.quantumbadger.redreader.cache.CacheRequest
import org.quantumbadger.redreader.cache.CacheRequestCallbacks
import org.quantumbadger.redreader.cache.downloadstrategy.DownloadStrategyAlways
import org.quantumbadger.redreader.common.*
import org.quantumbadger.redreader.common.Optional
import org.quantumbadger.redreader.common.datastream.SeekableInputStream
import org.quantumbadger.redreader.http.FailedRequestBody
import java.io.IOException
import java.util.*

@OptIn(ExperimentalSerializationApi::class)
object KThingTemporaryTester {

	// TODO generic listing manager with recyclerview
	//		Constructor param for item limit, store current item limit in adapter
	//		"End of Listing" text when `after` is null

	// TODO automatic parcelization with Kotlin

	fun run(context: Context) {

		CacheManager.getInstance(context).makeRequest(CacheRequest(
			//General.uriFromString("https://oauth.reddit.com/user/QuantumBadger/saved.json"),
			General.uriFromString("https://oauth.reddit.com/r/all"),
			RedditAccountManager.getInstance(context).getAccount("QuantumBadger")!!,
			null,
			Priority(Constants.Priority.API_POST_LIST),
			DownloadStrategyAlways.INSTANCE,
			Constants.FileType.POST_LIST,
			CacheRequest.DOWNLOAD_QUEUE_REDDIT_API,
			context,
			object : CacheRequestCallbacks {
				override fun onFailure(
					type: Int,
					t: Throwable?,
					httpStatus: Int?,
					readableMessage: String?,
					body: Optional<FailedRequestBody>
				) {
					Log.i("RRDEBUG", "Request failed: $type, $t, $httpStatus, $readableMessage, $body")
				}

				override fun onDataStreamComplete(
					streamFactory: GenericFactory<SeekableInputStream, IOException>,
					timestamp: Long,
					session: UUID,
					fromCache: Boolean,
					mimetype: String?
				) {
					val result = Json {ignoreUnknownKeys = true; isLenient = true }.decodeFromStream<RedditThing>(streamFactory.create())

					Log.i("RRDEBUG", "RRDEBUG result: $result")
				}
			}
		))

	}
}
