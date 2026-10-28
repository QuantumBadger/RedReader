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

import kotlinx.serialization.builtins.MapSerializer
import kotlinx.serialization.json.Json
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertNotNull
import org.junit.Assert.assertNull
import org.junit.Assert.assertTrue
import org.junit.Test
import org.quantumbadger.redreader.reddit.kthings.MaybeParseError
import org.quantumbadger.redreader.reddit.kthings.MaybeParseErrorSerializer
import org.quantumbadger.redreader.reddit.kthings.RedditMediaMetadata
import org.quantumbadger.redreader.reddit.kthings.UrlEncodedString
import org.quantumbadger.redreader.reddit.prepared.InlineMedia
import org.quantumbadger.redreader.reddit.prepared.InlineMediaResolver
import java.io.File

class InlineMediaResolverTest {

	companion object {
		private val json = Json {
			ignoreUnknownKeys = true
			isLenient = true
		}

		private val serializer = MapSerializer(
			UrlEncodedString.serializer(),
			MaybeParseErrorSerializer(RedditMediaMetadata.serializer())
		)

		fun decodeMediaMetadata(
			text: String
		): Map<UrlEncodedString, MaybeParseError<RedditMediaMetadata>> =
			json.decodeFromString(serializer, text)

		// Real "media_metadata" entries from comments: an uploaded image, GIFs from Giphy
		// (one "downsized", and one newer entry with no previews), and an entry which Reddit
		// marked as invalid
		fun loadTestData() = decodeMediaMetadata(
			File("src/test/resources/reddit/comment_media_metadata.json").readText()
		)

		const val IMAGE_URL = "https://preview.redd.it/13ukkwdf324e1.jpeg" +
				"?width=828&format=pjpg&auto=webp&s=223c05d014c62128899605b9cfb2fb4975128b9b"

		const val GIPHY_GIF_URL = "https://external-preview.redd.it/" +
				"JIOcmGvocKjXObYt7f5_bx2TpbyDPsmYkBhPDxCHFE8.gif" +
				"?width=200&height=200&s=f7929cacaaaa2ca7ae993bda59e59b070774838c"
	}

	@Test
	fun uploadedImage() {

		val media = InlineMediaResolver(loadTestData()).resolve(IMAGE_URL)

		assertNotNull(media)
		media!!

		assertEquals(InlineMedia.Type.IMAGE, media.type)
		assertEquals(828, media.width)
		assertEquals(1090, media.height)

		// The smaller previews, followed by the full size image
		assertEquals(listOf(108, 216, 320, 640, 828), media.stills.map { it.width })
		assertEquals(IMAGE_URL, media.stills.last().url.value)
	}

	@Test
	fun uploadedImageFromOtherHost() {
		assertNotNull(
			InlineMediaResolver(loadTestData())
				.resolve("https://i.redd.it/13ukkwdf324e1.jpeg")
		)
	}

	@Test
	fun chooseStill() {

		val media = InlineMediaResolver(loadTestData()).resolve(IMAGE_URL)!!

		assertEquals(108, media.chooseStill(1).width)
		assertEquals(320, media.chooseStill(300).width)
		assertEquals(640, media.chooseStill(640).width)

		// Nothing is wide enough, so the largest is used
		assertEquals(828, media.chooseStill(1080).width)
	}

	@Test
	fun giphy() {

		val media = InlineMediaResolver(loadTestData())
			.resolve("https://giphy.com/gifs/fqfPOiwKagbluB8Ors")

		assertNotNull(media)
		media!!

		assertEquals(InlineMedia.Type.ANIMATED, media.type)
		assertEquals(200, media.width)
		assertEquals(200, media.height)

		// A small thumbnail, then the GIF itself, of which only the first frame is decoded
		assertEquals(listOf(140, 200), media.stills.map { it.width })
		assertEquals(GIPHY_GIF_URL, media.chooseStill(141).url.value)
	}

	@Test
	fun giphyDownsized() {

		val media = InlineMediaResolver(loadTestData())
			.resolve("https://giphy.com/gifs/J0IZzqRxvrcmdZ4LgD")

		assertNotNull(media)
		assertEquals(384, media!!.width)
		assertEquals(480, media.height)
	}

	@Test
	fun giphyWithoutDetailsFromReddit() {

		// Captured from a comment's JSON on 2026-10-29: Reddit no longer sends any details of
		// GIFs from Giphy
		val metadata = decodeMediaMetadata(
			"""{"giphy|NA7ANK2VDczE12jLwv": {"status": "invalid"}}"""
		)

		val href = "https://giphy.com/gifs/NA7ANK2VDczE12jLwv"

		assertTrue(InlineMediaResolver(metadata).isEmpty())
		assertNull(InlineMediaResolver(metadata).resolve(href))

		val resolver = InlineMediaResolver(metadata, loadGiphyDirectly = true)
		assertFalse(resolver.isEmpty())

		val media = resolver.resolve(href)

		assertNotNull(media)
		media!!

		assertEquals(InlineMedia.Type.ANIMATED, media.type)
		assertFalse(media.isSizeKnown)
		assertEquals(
			"https://media.giphy.com/media/NA7ANK2VDczE12jLwv/200w_s.gif",
			media.chooseStill(1080).url.value
		)
	}

	@Test
	fun giphyDetailsFromRedditArePreferred() {

		val media = InlineMediaResolver(loadTestData(), loadGiphyDirectly = true)
			.resolve("https://giphy.com/gifs/fqfPOiwKagbluB8Ors")

		assertNotNull(media)
		assertTrue(media!!.isSizeKnown)
		assertEquals(GIPHY_GIF_URL, media.chooseStill(141).url.value)
	}

	@Test
	fun giphyLinksNotAttachedToCommentAreNotLoadedDirectly() {
		assertNull(
			InlineMediaResolver(loadTestData(), loadGiphyDirectly = true)
				.resolve("https://giphy.com/gifs/aaaaaaaaaaaaa")
		)
	}

	@Test
	fun giphyWithoutPreviews() {

		// Newer entries have "p": null, leaving only the GIF itself to display
		val media = InlineMediaResolver(loadTestData())
			.resolve("https://giphy.com/gifs/89x4osEodHEoo")

		assertNotNull(media)
		assertEquals(listOf(148), media!!.stills.map { it.width })
	}

	@Test
	fun uploadedGif() {

		// Captured from a comment's JSON on 2026-10-29: the body links to the GIF on i.redd.it
		val metadata = decodeMediaMetadata(
			"""
			{
				"tgwaot33ovxh1": {
					"status": "valid",
					"e": "AnimatedImage",
					"m": "image/gif",
					"p": [{"y": 108, "x": 108, "u": "https://preview.redd.it/tgwaot33ovxh1.gif?width=108&amp;crop=smart&amp;format=png8&amp;s=fdb6ff72eefa7d9dcd3f8f323d5603d2fc1123a0"}],
					"s": {
						"y": 108,
						"gif": "https://i.redd.it/tgwaot33ovxh1.gif",
						"mp4": "https://preview.redd.it/tgwaot33ovxh1.gif?format=mp4&amp;s=1e7ae3a33151f204060ea1e971f7018b7262478d",
						"x": 108
					},
					"id": "tgwaot33ovxh1"
				}
			}
			""".trimIndent()
		)

		val media = InlineMediaResolver(metadata).resolve("https://i.redd.it/tgwaot33ovxh1.gif")

		assertNotNull(media)
		assertEquals(InlineMedia.Type.ANIMATED, media!!.type)
		assertEquals(108, media.width)
	}

	@Test
	fun giphyWithDescriptionInUrl() {
		assertNotNull(
			InlineMediaResolver(loadTestData())
				.resolve("https://giphy.com/gifs/funny-cat-fqfPOiwKagbluB8Ors")
		)
	}

	@Test
	fun invalidEntry() {

		val metadata = loadTestData()

		// Reddit only sends the status for these, so they fail to parse
		assertTrue(
			metadata[UrlEncodedString("giphy|11C6Hp7Pzlp6qQ")] is MaybeParseError.Err
		)

		assertNull(
			InlineMediaResolver(metadata).resolve("https://giphy.com/gifs/11C6Hp7Pzlp6qQ")
		)
	}

	@Test
	fun linksToMediaNotAttachedToComment() {

		val resolver = InlineMediaResolver(loadTestData())

		assertNull(resolver.resolve("https://preview.redd.it/aaaaaaaaaaaaa.jpeg?width=640"))
		assertNull(resolver.resolve("https://giphy.com/gifs/aaaaaaaaaaaaa"))
		assertNull(resolver.resolve("https://example.com/13ukkwdf324e1.jpeg"))
		assertNull(resolver.resolve("https://www.reddit.com/r/redreader"))
	}

	@Test
	fun noMetadata() {
		assertTrue(InlineMediaResolver(null).isEmpty())
		assertTrue(InlineMediaResolver(emptyMap()).isEmpty())
		assertFalse(InlineMediaResolver(loadTestData()).isEmpty())
	}

	@Test
	fun unsupportedMediaTypes() {

		// Shape from other clients' type definitions: comment videos have no "s" field,
		// and emotes aren't linked to, so neither should be resolved
		val metadata = decodeMediaMetadata(
			"""
			{
				"lxk3mgydr9ff1": {
					"status": "valid",
					"e": "RedditVideo",
					"dashUrl": "https://v.redd.it/link/abc/asset/lxk3mgydr9ff1/DASHPlaylist.mpd",
					"x": 1280,
					"y": 720,
					"isGif": false,
					"id": "lxk3mgydr9ff1"
				},
				"emote|t5_2qh1i|1234": {
					"status": "valid",
					"e": "Image",
					"m": "image/png",
					"s": {"x": 60, "y": 60, "u": "https://reddit-econ-prod-assets/e.png"},
					"t": "sticker",
					"id": "emote|t5_2qh1i|1234"
				}
			}
			""".trimIndent()
		)

		val resolver = InlineMediaResolver(metadata)

		assertNull(resolver.resolve("https://reddit.com/link/abc/video/lxk3mgydr9ff1/player"))
		assertNull(resolver.resolve("https://preview.redd.it/lxk3mgydr9ff1.mp4"))
	}
}
