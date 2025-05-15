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

import android.os.Parcelable
import androidx.compose.runtime.Immutable
import kotlinx.parcelize.Parcelize
import org.apache.commons.text.StringEscapeUtils
import org.quantumbadger.redreader.common.UriString
import org.quantumbadger.redreader.jsonwrap.JsonObject
import java.io.IOException

private fun Long.isValidPositiveInt() = this < Int.MAX_VALUE && this >= 0

@Immutable
@Parcelize
data class ImageSize(
	val width: Int,
	val height: Int,
) : Parcelable {
	companion object {
		fun from(width: Long?, height: Long?): ImageSize? {
			return if (
				width?.takeIf { it.isValidPositiveInt() } != null &&
				height?.takeIf { it.isValidPositiveInt() } != null
			) {
				ImageSize(width.toInt(), height.toInt())
			} else {
				null
			}
		}

		fun fromJson(
			obj: JsonObject?,
			fieldWidth: String = "width",
			fieldHeight: String = "height"
		) = from(
			obj?.getLong(fieldWidth),
			obj?.getLong(fieldHeight)
		)
	}

	override fun toString() = "${width}x$height"
}

@Immutable
@Parcelize
data class ImageUrlInfo(
	@JvmField val url: UriString,
	@JvmField val size: ImageSize? = null,
	@JvmField val sizeBytes: Long? = null,
) : Parcelable

@Immutable
@Parcelize
data class ImageInfo(

	// TODO include all sizes and select at usage time
	@JvmField val original: ImageUrlInfo,
	@JvmField val bigSquare: ImageUrlInfo? = null,
	@JvmField val preview: ImageUrlInfo? = null,

	@JvmField val urlAudioStream: UriString? = null,

	@JvmField val title: String? = null,
	@JvmField val caption: String? = null,

	@JvmField val outboundUrl: UriString? = null,

	@JvmField val type: String? = null,
	val isAnimated: Boolean? = null,

	@JvmField val mediaType: MediaType? = null,
	@JvmField val hasAudio: HasAudio,
) : Parcelable {

	enum class MediaType {
		IMAGE, VIDEO, GIF
	}

	enum class HasAudio {
		HAS_AUDIO, MAYBE_AUDIO, NO_AUDIO;

		companion object {
			fun fromBoolean(value: Boolean?): HasAudio {
				if (value == null) {
					return MAYBE_AUDIO
				}

				return if (value) {
					HAS_AUDIO
				} else {
					NO_AUDIO
				}
			}
		}
	}

	companion object {
		@JvmStatic
		fun parseGfycat(obj: JsonObject): ImageInfo {

			val title = obj.getString("title")

			val hasAudio = obj.getBoolean("hasAudio")

			val original = obj.getString("mp4Url")?.let { originalUrl ->
				ImageUrlInfo(
					url = UriString(originalUrl),
					size = ImageSize.from(obj.getLong("width"), obj.getLong("height")),
					sizeBytes = obj.getLong("mp4Size")
				)
			}

			if (original == null) {
				throw RuntimeException("mp4Url field missing from response")
			}

			val mobilePoster = obj.getObjectAtPath("content_urls", "mobilePoster").asNullable()

			val preview = mobilePoster?.getString("url")?.let { previewUrl ->
				ImageUrlInfo(
					url = UriString(previewUrl),
					size = ImageSize.fromJson(mobilePoster)
				)
			}

			return ImageInfo(
				original = original,
				preview = preview,
				title = title,
				type = "video/mp4",
				isAnimated = true,
				mediaType = MediaType.VIDEO,
				hasAudio = HasAudio.fromBoolean(hasAudio),
			)
		}

		@JvmStatic
		@Throws(IOException::class)
		fun parseStreamable(obj: JsonObject): ImageInfo {
			var fileObj: JsonObject? = null
			val files = obj.getObject("files")

			val preferredTypes = arrayOf(
				"mp4",
				"webm",
				"mp4-high",
				"webm-high",
				"mp4-mobile",
				"webm-mobile"
			)
			var selectedType: String? = null

			if (files != null) {
				for (type in preferredTypes) {
					fileObj = files.getObject(type)
					selectedType = type
					if (fileObj != null) {
						break
					}
				}
			}

			if (fileObj == null) {
				throw IOException("No suitable Streamable files found")
			}

			val mimeType =
				"video/" + selectedType!!.split("-".toRegex()).dropLastWhile { it.isEmpty() }
					.toTypedArray()[0]

			val original = fileObj.getString("url")?.let { originalUrl ->
				ImageUrlInfo(
					url = UriString(
						if (originalUrl.startsWith("//")) {
							"https:$originalUrl"
						} else {
							originalUrl
						}
					),
					size = ImageSize.fromJson(fileObj)
				)
			}

			if (original == null) {
				throw RuntimeException("url field missing from response")
			}

			return ImageInfo(
				original = original,
				type = mimeType,
				isAnimated = true,
				mediaType = MediaType.VIDEO,
				hasAudio = HasAudio.MAYBE_AUDIO,
			)
		}

		@JvmStatic
		fun parseImgur(obj: JsonObject?): ImageInfo {
			val image = obj?.getObject("image")
			val links = obj?.getObject("links")

			val type = image?.getString("type")
			val isAnimated = "true" == image?.getString("animated")

			val original = links?.getString("original")?.let { originalUrl ->
				ImageUrlInfo(
					url = UriString(
						if (isAnimated) {
							originalUrl.replace(".gif", ".mp4")
						} else {
							originalUrl
						}
					),
					size = ImageSize.fromJson(image),
					sizeBytes = image?.getLong("size")
				)
			}

			if (original == null) {
				throw RuntimeException("original field missing from response")
			}

			val bigSquare = links.getString("big_square")?.let { bigSquareUrl ->
				ImageUrlInfo(url = UriString(bigSquareUrl))
			}

			return ImageInfo(
				original = original,
				bigSquare = bigSquare,
				title = image?.getString("title")?.let(StringEscapeUtils::unescapeHtml4),
				caption = image?.getString("caption")?.let(StringEscapeUtils::unescapeHtml4),
				type = type,
				isAnimated = isAnimated,
				mediaType = if (isAnimated) MediaType.VIDEO else MediaType.IMAGE,
				hasAudio = if (isAnimated) HasAudio.MAYBE_AUDIO else HasAudio.NO_AUDIO,
			)
		}

		@JvmStatic
		fun parseImgurV3(obj: JsonObject?): ImageInfo {
			val type = obj?.getString("type")
			val isAnimated = obj?.getBoolean("animated")

			var mp4 = false
			var hasSound: Boolean? = null

			val originalSize = ImageSize.fromJson(obj)

			val original = obj?.getString("mp4")?.takeIf { it.isNotEmpty() }?.let { urlMp4 ->

				hasSound = obj.getBoolean("has_sound")
				mp4 = true

				ImageUrlInfo(
					url = UriString(urlMp4),
					size = originalSize,
					sizeBytes = obj.getLong("mp4_size"),
				)

			} ?: run {
				hasSound = false
				mp4 = false

				obj?.getString("link")?.takeIf { it.isNotEmpty() }?.let { urlLink ->
					ImageUrlInfo(
						url = UriString(urlLink),
						size = originalSize,
						sizeBytes = obj.getLong("size")
					)
				}
			}

			if (original == null) {
				throw RuntimeException("original field missing from response")
			}

			val bigSquare = obj?.getString("id")?.let {
				ImageUrlInfo(url = UriString("https://i.imgur.com/${it}b.jpg"))
			}

			return ImageInfo(
				original = original,
				bigSquare = bigSquare,
				title = obj?.getString("title")?.let(StringEscapeUtils::unescapeHtml4),
				caption = obj?.getString("caption")?.let(StringEscapeUtils::unescapeHtml4),
				type = type,
				isAnimated = isAnimated,
				mediaType = if (mp4) MediaType.VIDEO else MediaType.IMAGE,
				hasAudio = HasAudio.fromBoolean(hasSound),
			)
		}

		@JvmStatic
		fun parseDeviantArt(obj: JsonObject?): ImageInfo {

			val original = obj?.getString("url")?.let { url ->
				ImageUrlInfo(
					url = UriString(url),
					size = ImageSize.fromJson(obj),
				)
			}

			val bigSquare = obj?.getString("thumbnail_url")?.let { url ->
				ImageUrlInfo(url = UriString(url))
			}

			if (original == null) {
				throw RuntimeException("url field missing from response")
			}

			return ImageInfo(
				original = original,
				bigSquare = bigSquare,
				title = obj.getString("title")?.let(StringEscapeUtils::unescapeHtml4),
				caption = obj.getString("tags")?.let(StringEscapeUtils::unescapeHtml4),
				type = obj.getString("imagetype"),
				isAnimated = false,
				mediaType = MediaType.IMAGE,
				hasAudio = HasAudio.NO_AUDIO,
			)
		}

		@JvmStatic
		fun parseRedgifsV2(obj: JsonObject): ImageInfo {

			val original = obj.getStringAtPath("urls", "hd")
				.orElse(obj.getStringAtPath("urls", "sd"))
				.asNullable()?.let {
					ImageUrlInfo(
						url = UriString(it),
						size = ImageSize.fromJson(obj)
					)
				}

			val hasAudio = obj.getBoolean("hasAudio")

			val preview = obj.getStringAtPath("urls", "poster").orElseNull()?.let { url ->
				ImageUrlInfo(url = UriString(url))
			}

			if (original == null) {
				throw RuntimeException("original field missing from response")
			}

			return ImageInfo(
				original = original,
				preview = preview,
				type = "video/mp4",
				isAnimated = true,
				mediaType = MediaType.VIDEO,
				hasAudio = HasAudio.fromBoolean(hasAudio),
			)
		}
	}
}
