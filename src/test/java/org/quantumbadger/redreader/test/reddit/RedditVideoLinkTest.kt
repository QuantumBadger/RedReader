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
import org.junit.Assert.assertNotNull
import org.junit.Assert.assertNull
import org.junit.Assert.assertTrue
import org.junit.Test
import org.junit.runner.RunWith
import org.quantumbadger.redreader.common.LinkHandler
import org.quantumbadger.redreader.common.UriString
import org.quantumbadger.redreader.image.parseMPD
import org.robolectric.RobolectricTestRunner
import java.io.File

@RunWith(RobolectricTestRunner::class)
class RedditVideoLinkTest {

	companion object {
		// As linked to from the body of a real comment, captured on 2026-10-29
		private const val COMMENT_VIDEO_URL =
			"https://reddit.com/link/pfe543d/video/46b22pf54zuh1/player"
	}

	private fun videoId(url: String) = LinkHandler.getRedditVideoId(UriString(url))

	@Test
	fun videoEmbeddedInComment() {

		assertEquals("46b22pf54zuh1", videoId(COMMENT_VIDEO_URL))
		assertTrue(LinkHandler.isProbablyAnImage(UriString(COMMENT_VIDEO_URL)))

		assertEquals(
			"46b22pf54zuh1",
			videoId("https://www.reddit.com/link/pfe543d/video/46b22pf54zuh1/player/")
		)
		assertEquals(
			"46b22pf54zuh1",
			videoId("https://old.reddit.com/link/pfe543d/video/46b22pf54zuh1/player?a=b")
		)
	}

	@Test
	fun videoOnVReddIt() {
		assertEquals("46b22pf54zuh1", videoId("https://v.redd.it/46b22pf54zuh1"))
		assertEquals("46b22pf54zuh1", videoId("https://v.redd.it/46b22pf54zuh1/CMAF_220.mp4"))
	}

	@Test
	fun otherRedditLinks() {
		assertNull(videoId("https://www.reddit.com/r/redreader/comments/abc123/title/"))
		assertNull(videoId("https://reddit.com/link/pfe543d"))
		assertNull(videoId("https://www.reddit.com/r/subreddit"))
	}

	@Test
	fun playlistOfVideoEmbeddedInComment() {

		// The playlist for the video above, which has no audio
		val result = parseMPD(
			File("src/test/resources/mpd/comment_video_46b22pf54zuh1.mpd").readText()
		)

		assertNotNull(result.video)
		assertEquals(480, result.video?.width)
		assertEquals(270, result.video?.height)
		assertNull(result.audio)
	}
}
