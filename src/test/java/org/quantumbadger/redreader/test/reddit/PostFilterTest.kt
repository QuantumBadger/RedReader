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
package org.quantumbadger.redreader.test.reddit

import org.junit.Assert.assertEquals
import org.junit.Test
import org.quantumbadger.redreader.common.time.TimestampUTC
import org.quantumbadger.redreader.reddit.PostFilter
import org.quantumbadger.redreader.reddit.kthings.ImageMetadata
import org.quantumbadger.redreader.reddit.kthings.MaybeParseError
import org.quantumbadger.redreader.reddit.kthings.RedditIdAndType
import org.quantumbadger.redreader.reddit.kthings.RedditMediaMetadata
import org.quantumbadger.redreader.reddit.kthings.RedditPost
import org.quantumbadger.redreader.reddit.kthings.RedditTimestampUTC
import org.quantumbadger.redreader.reddit.kthings.UrlEncodedString

class PostFilterTest {

	@Test
	fun selfPostIsText() {
		assertEquals(
			PostFilter.TEXT,
			PostFilter.classify(post(isSelf = true, url = "https://example.com")))
	}

	@Test
	fun imagePostIsImage() {
		assertEquals(
			PostFilter.IMAGE,
			PostFilter.classify(post(url = "https://i.redd.it/image.jpg")))
	}

	@Test
	fun redditVideoIsVideo() {
		assertEquals(
			PostFilter.VIDEO,
			PostFilter.classify(post(isVideo = true)))
	}

	@Test
	fun videoLinkIsVideo() {
		assertEquals(
			PostFilter.VIDEO,
			PostFilter.classify(post(url = "https://www.youtube.com/watch?v=video")))
	}

	@Test
	fun animatedGifIsVideo() {
		assertEquals(
			PostFilter.VIDEO,
			PostFilter.classify(
					post(url = "https://i.redd.it/animation.gif", postHint = "image")))
	}

	@Test
	fun staticGalleryIsImage() {
		assertEquals(PostFilter.IMAGE, PostFilter.classify(gallery("Image")))
	}

	@Test
	fun animatedGalleryIsVideo() {
		assertEquals(PostFilter.VIDEO, PostFilter.classify(gallery("AnimatedImage")))
	}

	@Test
	fun articleIsLink() {
		assertEquals(
			PostFilter.LINK,
			PostFilter.classify(post(url = "https://example.com/article")))
	}

	private fun post(
		url: String? = null,
		isSelf: Boolean = false,
		isVideo: Boolean = false,
		postHint: String? = null,
		galleryData: RedditPost.GalleryData? = null,
		mediaMetadata: Map<UrlEncodedString, MaybeParseError<RedditMediaMetadata>>? = null
	) = RedditPost(
		id = "id",
		name = RedditIdAndType("t3_id"),
		subreddit = UrlEncodedString("test"),
		num_comments = 0,
		score = 0,
		permalink = UrlEncodedString("/r/test/comments/id"),
		created_utc = RedditTimestampUTC(TimestampUTC.ZERO),
		url = url?.let(::UrlEncodedString),
		is_self = isSelf,
		is_video = isVideo,
		post_hint = postHint,
		gallery_data = galleryData,
		media_metadata = mediaMetadata
	)

	private fun gallery(mediaType: String): RedditPost {
		val mediaId = UrlEncodedString("media")
		val metadata = RedditMediaMetadata(
			status = "valid",
			e = mediaType,
			m = "image/jpeg",
			s = ImageMetadata(
				x = 100,
				y = 100,
				u = UrlEncodedString("https://i.redd.it/image.jpg")),
			id = "media")
		val galleryItem: MaybeParseError<RedditPost.GalleryData.GalleryItem>
				= MaybeParseError.Ok(RedditPost.GalleryData.GalleryItem(mediaId))
		val metadataEntry: MaybeParseError<RedditMediaMetadata> = MaybeParseError.Ok(metadata)

		return post(
			url = "https://www.reddit.com/gallery/id",
			galleryData = RedditPost.GalleryData(listOf(galleryItem)),
			mediaMetadata = mapOf(mediaId to metadataEntry))
	}
}
