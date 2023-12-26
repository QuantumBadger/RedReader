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

import org.apache.commons.text.StringEscapeUtils
import org.quantumbadger.redreader.jsonwrap.JsonObject
import org.quantumbadger.redreader.reddit.kthings.ImageMetadata
import org.quantumbadger.redreader.reddit.kthings.MaybeParseError
import org.quantumbadger.redreader.reddit.kthings.RedditPost
import kotlin.math.min

class AlbumInfo(
	@JvmField val url: String,
	@JvmField val title: String?,
	val description: String?,
	images: List<ImageInfo>
) {
    @JvmField
	val images = ArrayList(images)

    companion object {
        @JvmStatic
		fun parseImgur(
			url: String,
			obj: JsonObject
        ): AlbumInfo {
            var title = obj.getString("title")
            var description = obj.getString("description")

            if (title != null) {
                title = StringEscapeUtils.unescapeHtml4(title)
            }

            if (description != null) {
                description = StringEscapeUtils.unescapeHtml4(description)
            }

            val imagesJson = obj.getArray("images")
            val images = ArrayList<ImageInfo>()

            for (imageJson in imagesJson!!) {
                images.add(ImageInfo.parseImgur(imageJson.asObject()))
            }

            return AlbumInfo(url, title, description, images)
        }

        @JvmStatic
		fun parseImgurV3(
			url: String,
			obj: JsonObject
        ): AlbumInfo {
            var title = obj.getString("title")
            var description = obj.getString("description")

            if (title != null) {
                title = StringEscapeUtils.unescapeHtml4(title)
            }

            if (description != null) {
                description = StringEscapeUtils.unescapeHtml4(description)
            }

            val imagesJson = obj.getArray("images")
            val images = ArrayList<ImageInfo>()

            for (imageJson in imagesJson!!) {
                images.add(ImageInfo.parseImgurV3(imageJson.asObject()))
            }

            return AlbumInfo(url, title, description, images)
        }

        private fun stringToMediaType(mediaTypeString: String?): ImageInfo.MediaType {
            return if (mediaTypeString == null) {
                ImageInfo.MediaType.IMAGE
            } else {
                when (mediaTypeString) {
                    "AnimatedImage" -> ImageInfo.MediaType.GIF
					// This string doesn't seem to exist yet, but it might do in future
                    "Video" -> ImageInfo.MediaType.VIDEO
                    "Image" -> ImageInfo.MediaType.IMAGE
                    else -> ImageInfo.MediaType.IMAGE
                }
            }
        }

        private fun getThumbnail(images: List<ImageMetadata>): String? {
            val minThumbSize = 200

            var bestSizeMinAxis: Long? = null
            var bestUrl: String? = null

            for (image in images) {

				if (image.u == null) {
					continue
				}

                val x = image.x
                val y = image.y

                val minAxis = min(x, y)

                if (bestSizeMinAxis == null || (bestSizeMinAxis < minThumbSize && minAxis > bestSizeMinAxis)
                    || (minAxis >= minThumbSize && minAxis < bestSizeMinAxis)
                ) {
                    bestSizeMinAxis = minAxis
                    bestUrl = image.u.decoded
                }
            }

            return StringEscapeUtils.unescapeHtml4(bestUrl)
        }

        fun parseRedditGallery(post: RedditPost): AlbumInfo? {

            val galleryItems = post.gallery_data?.items ?: return null

            val images = galleryItems.mapNotNull { (it as? MaybeParseError.Ok)?.value }
				.mapNotNull { item ->

					val mediaMetadataEntry = (post.media_metadata?.get(item.media_id) as? MaybeParseError.Ok)?.value ?:
						return@mapNotNull null

					val standardImage = mediaMetadataEntry.s

					val mediaType = stringToMediaType(mediaMetadataEntry.e)

					val urlEscaped = standardImage.u ?: standardImage.mp4 ?: standardImage.gif

					ImageInfo(
						urlEscaped?.decoded,
						mediaMetadataEntry.p?.let { p -> getThumbnail(p) },
						item.caption?.decoded,
						null,
						item.outbound_url?.decoded,
						mediaMetadataEntry.m,
						mediaType != ImageInfo.MediaType.IMAGE,
						standardImage.x,
						standardImage.y,
						null,
						mediaType,
						ImageInfo.HasAudio.NO_AUDIO,
						null,
						null,
						null
					)
				}.toList()

            val title = post.title?.decoded

            return AlbumInfo(post.findUrl()!!, title, null, images)
        }
    }
}
