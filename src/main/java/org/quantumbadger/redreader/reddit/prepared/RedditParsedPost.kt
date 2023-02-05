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
 * along with RedReader.  If not, see <http:></http:>//www.gnu.org/licenses/>.
 */
package org.quantumbadger.redreader.reddit.prepared

import androidx.appcompat.app.AppCompatActivity
import org.quantumbadger.redreader.R
import org.quantumbadger.redreader.common.time.TimestampUTC
import org.quantumbadger.redreader.reddit.PostCommentSort
import org.quantumbadger.redreader.reddit.kthings.RedditPost
import org.quantumbadger.redreader.reddit.prepared.bodytext.BodyElement
import org.quantumbadger.redreader.reddit.prepared.html.HtmlReader
import org.quantumbadger.redreader.reddit.things.RedditThingWithIdAndType

class RedditParsedPost(
	activity: AppCompatActivity,
	private val src: RedditPost,
	parseSelfText: Boolean
) : RedditThingWithIdAndType {

    val title: String
	val url: String?
	val selfText: BodyElement?
	val flairText: String?
	val domain: String


	fun findUrl(): String? {

		src.media?.reddit_video?.fallback_url?.decoded?.apply {
			return this
		}

		if (src.url?.decoded?.contains("\\.gif") == true) {
			src.preview?.images?.get(0)?.variants?.mp4?.source?.url?.decoded?.apply {
				return this
			}
		}

		return src.url?.decoded
	}

    init {
		title = src.title?.decoded?.replace('\n', ' ')?.trim() ?: "[null]"

		url = findUrl()

		selfText = if (parseSelfText && isSelfPost && src.selftext_html != null) {
			HtmlReader.parse(src.selftext_html.decoded, activity)
		} else {
			null
		}

		flairText = if (src.link_flair_text != null && src.link_flair_text.decoded.isNotEmpty()) {
			src.link_flair_text.decoded
		} else {
			null
		}

		domain = src.domain?.decoded ?: activity.getString(R.string.post_domain_deleted)
    }

    override fun getIdAlone(): String {
        return src.idAlone
    }

    override fun getIdAndType(): String {
        return src.idAndType
    }

    val isStickied: Boolean
        get() = src.stickied == true

    val thumbnailUrl: String?
        get() = src.thumbnail?.decoded

    data class ImagePreviewDetails(
		@JvmField val url: String,
		@JvmField val width: Int,
		@JvmField val height: Int
	)

    val isPreviewEnabled: Boolean
        get() = src.preview?.enabled == true

    fun getPreview(minWidth: Int, minHeight: Int) = src.preview?.images?.get(0)?.apply {
		getPreviewInternal(this, minWidth, minHeight)
	}

    fun getPreviewMP4(minWidth: Int, minHeight: Int)
		= src.preview?.images?.get(0)?.variants?.mp4?.apply {
			getPreviewInternal(this, minWidth, minHeight)
	}

    private fun getPreviewInternal(
		image: RedditPost.Preview.ImageBase,
		minWidth: Int,
		minHeight: Int
    ): ImagePreviewDetails? {

		val resolutions = image.resolutions

		if (resolutions.isNullOrEmpty()) {
			return null
		}

		var best: RedditPost.Preview.ImageDetails? = null

		val sourceWidth = image.source?.width
		val sourceHeight = image.source?.height

		for (i in -1 until resolutions.size) {

			val resolution = if (i == -1) {
				image.source ?: continue
			} else {
				resolutions[i]
			}

			if (resolution.width < 50 || resolution.height < 50) {
				continue
			}

			if (sourceWidth != null && sourceHeight != null && sourceWidth > 0) {

				val estimatedRealHeight =
					(sourceHeight.toDouble() / sourceWidth.toDouble() * resolution.width).toInt()

				if (estimatedRealHeight > 3000) {
					continue
				}
			}

			val use = if (resolution.height > 3000 || resolution.width > 3000) {
				false
			} else if (best == null) {
				true
			} else if ((best.width < minWidth || best.height < minHeight)
				&& (resolution.width > best.width || resolution.height > best.height)
			) {
				true
			} else if (resolution.width < best.width
					&& resolution.height < best.height
					&& resolution.width >= minWidth
					&& resolution.height >= minHeight) {
				true
			} else {
				false
			}

			if (use) {
				best = resolution
			}
		}

		return best?.run {
			ImagePreviewDetails(url.decoded, width, height)
		}
	}

    val suggestedCommentSort: PostCommentSort?
        get() = if (src.suggested_sort == null) {
            null
        } else PostCommentSort.lookup(src.suggested_sort)

    val isArchived: Boolean
        get() = src.archived == true

    val isLocked: Boolean
        get() = src.locked == true

    fun canModerate(): Boolean {
        return src.can_mod_post == true
    }

    val author: String?
        get() = src.author?.decoded

    val distinguished: String?
        get() = src.distinguished

    val rawSelfTextMarkdown: String?
        get() = src.selftext?.decoded

    val isSpoiler: Boolean
        get() = src.spoiler == true

    val unescapedSelfText: String?
        get() = src.selftext?.decoded

    val subreddit: String
        get() = src.subreddit.decoded

	// TODO do we still need this? I think it's because we add the score at a later point
    val scoreExcludingOwnVote: Int
        get() {
            var score = src.score
			when (src.likes) {
				true -> score--
				false -> score++
				null -> {}
			}
			return score
        }

    val commentCount: Int
        get() = src.num_comments

    val goldAmount: Int
        get() = src.gilded

    val isNsfw: Boolean
        get() = src.over_18 == true

    val createdTimeSecsUTC: TimestampUTC
        get() = src.created_utc.value

    val isSelfPost: Boolean
        get() = src.is_self == true

    val upvotePercentage: Int?
        get() = src.upvote_ratio?.times(100.0)?.toInt()
}
