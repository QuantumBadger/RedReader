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
import org.quantumbadger.redreader.account.RedditAccountManager
import org.quantumbadger.redreader.cache.CacheManager
import org.quantumbadger.redreader.cache.CacheRequest
import org.quantumbadger.redreader.cache.CacheRequestCallbacks
import org.quantumbadger.redreader.cache.downloadstrategy.DownloadStrategyIfNotCached
import org.quantumbadger.redreader.common.Constants
import org.quantumbadger.redreader.common.General.getGeneralErrorForFailure
import org.quantumbadger.redreader.common.GenericFactory
import org.quantumbadger.redreader.common.Priority
import org.quantumbadger.redreader.common.RRError
import org.quantumbadger.redreader.common.UriString
import org.quantumbadger.redreader.common.datastream.SeekableInputStream
import org.quantumbadger.redreader.common.time.TimestampUTC
import org.quantumbadger.redreader.http.FailedRequestBody
import org.quantumbadger.redreader.reddit.kthings.JsonUtils
import org.quantumbadger.redreader.reddit.kthings.RedditPost
import org.quantumbadger.redreader.reddit.kthings.RedditThing
import org.quantumbadger.redreader.reddit.kthings.RedditThingResponse
import org.quantumbadger.redreader.reddit.url.PostCommentListingURL
import java.io.IOException
import java.util.UUID

class RedditGalleryAPI {

    companion object {
		private val cache = object : LinkedHashMap<String, AlbumInfo>() {
			override fun removeEldestEntry(eldest: MutableMap.MutableEntry<String, AlbumInfo>?): Boolean {
				return this.size > 100
			}
		}

        fun addToCache(post: RedditPost) {
			AlbumInfo.parseRedditGallery(post)?.apply {
				synchronized(cache) {
					cache.remove(post.id)
					cache.put(post.id, this)
				}
			}
        }

        @JvmStatic
		fun getAlbumInfo(
            context: Context,
            albumUrl: UriString,
            albumId: String,
            priority: Priority,
            listener: GetAlbumInfoListener
        ) {
			val cacheEntry = synchronized(cache) {
				cache[albumId]
			}

			cacheEntry?.apply {
				listener.onSuccess(this)
				return
			}

            val apiUrl = PostCommentListingURL(
                null,
                albumId,
                null,
                null,
                null,
                null,
                false
            ).generateJsonUri()

            CacheManager.getInstance(context).makeRequest(
                CacheRequest(
					UriString.from(apiUrl),
                    RedditAccountManager.getInstance(context).defaultAccount,
                    null,
                    priority,
                    DownloadStrategyIfNotCached.INSTANCE,
                    Constants.FileType.IMAGE_INFO,
					CacheRequest.DownloadQueueType.REDDIT_API,
                    context,
					object : CacheRequestCallbacks {

						override fun onDataStreamComplete(
							streamFactory: GenericFactory<SeekableInputStream, IOException>,
							timestamp: TimestampUTC?,
							session: UUID,
							fromCache: Boolean,
							mimetype: String?
						) {
							try {
								val thingResponse = JsonUtils.decodeRedditThingResponseFromStream(streamFactory.create())

								val responseMultiple = (thingResponse as? RedditThingResponse.Multiple) ?:
									throw RuntimeException("Expecting RedditThingResponse.Multiple")

								val listing = (responseMultiple.things.firstOrNull() as? RedditThing.Listing)?.data ?:
									throw RuntimeException("No listing in response")

								val firstItem = listing.children.firstOrNull()?.ok()

								val post = (firstItem as? RedditThing.Post)?.data ?:
									throw RuntimeException("No post found in response")

								val album = AlbumInfo.parseRedditGallery(post)

								if (album == null) {
									if (post.removed_by_category != null) {
										listener.onGalleryRemoved()
									} else {
										listener.onGalleryDataNotPresent()
									}
								} else {
									listener.onSuccess(album)
								}

							} catch(t: Throwable) {
								onFailure(getGeneralErrorForFailure(
									context,
									CacheRequest.RequestFailureType.PARSE,
									t,
									null,
									albumUrl,
									FailedRequestBody.from(streamFactory)));
							}
						}

						override fun onFailure(error: RRError) {
							listener.onFailure(error)
						}
					}
                )
            )
        }
    }
}
