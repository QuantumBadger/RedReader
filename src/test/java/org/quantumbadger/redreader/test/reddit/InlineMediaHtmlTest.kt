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

import androidx.appcompat.app.AppCompatActivity
import org.junit.Assert.assertEquals
import org.junit.Assert.assertNull
import org.junit.Assert.assertTrue
import org.junit.Test
import org.junit.runner.RunWith
import org.quantumbadger.redreader.reddit.prepared.InlineMediaResolver
import org.quantumbadger.redreader.reddit.prepared.bodytext.BodyElement
import org.quantumbadger.redreader.reddit.prepared.bodytext.BodyElementInlineMedia
import org.quantumbadger.redreader.reddit.prepared.bodytext.BodyElementLinkButton
import org.quantumbadger.redreader.reddit.prepared.bodytext.BodyElementTextSpanned
import org.quantumbadger.redreader.reddit.prepared.bodytext.BodyElementVerticalSequence
import org.quantumbadger.redreader.reddit.prepared.html.HtmlReader
import org.quantumbadger.redreader.reddit.prepared.html.InlineMediaCollector
import org.robolectric.Robolectric
import org.robolectric.RobolectricTestRunner

@RunWith(RobolectricTestRunner::class)
class InlineMediaHtmlTest {

	companion object {
		// As in body_html, where the URL of an uploaded image is linked to itself
		private const val IMAGE_HREF = "https://preview.redd.it/13ukkwdf324e1.jpeg" +
				"?width=828&amp;format=pjpg&amp;auto=webp" +
				"&amp;s=223c05d014c62128899605b9cfb2fb4975128b9b"

		private const val IMAGE_LINK = "<a href=\"$IMAGE_HREF\">$IMAGE_HREF</a>"

		private const val GIPHY_LINK =
			"<a href=\"https://giphy.com/gifs/fqfPOiwKagbluB8Ors\" target=\"_blank\">" +
					"<img src=\"https://external-preview.redd.it/" +
					"JIOcmGvocKjXObYt7f5_bx2TpbyDPsmYkBhPDxCHFE8.gif?width=200&amp;height=200" +
					"&amp;s=f7929cacaaaa2ca7ae993bda59e59b070774838c\" width=\"200\" " +
					"height=\"200\"></a>"
	}

	private val activity: AppCompatActivity =
		Robolectric.buildActivity(AppCompatActivity::class.java).get()

	private class Result(
		val elements: List<BodyElement>,
		val collector: InlineMediaCollector?
	) {
		fun count(type: Class<out BodyElement>) = elements.count { type.isInstance(it) }
		val mediaCount get() = count(BodyElementInlineMedia::class.java)
		val linkButtonCount get() = count(BodyElementLinkButton::class.java)
		val textCount get() = count(BodyElementTextSpanned::class.java)
	}

	private fun parse(
		body: String,
		withInlineMedia: Boolean = true,
		resolver: InlineMediaResolver = InlineMediaResolver(InlineMediaResolverTest.loadTestData())
	): Result {

		val collector = if (withInlineMedia) InlineMediaCollector(resolver) else null

		val parsed = HtmlReader.parse("<div class=\"md\">$body\n</div>", activity, collector)

		assertTrue(
			"Expected parse to succeed, got $parsed",
			parsed is BodyElementVerticalSequence
		)

		return Result((parsed as BodyElementVerticalSequence).elements, collector)
	}

	@Test
	fun imageReplacesLinkAndLinkButton() {

		val result = parse("<p>Before</p>\n\n<p>$IMAGE_LINK</p>")
		val withoutImage = parse("<p>Before</p>\n\n")

		assertEquals(1, result.mediaCount)
		assertEquals(0, result.linkButtonCount)

		// The URL isn't displayed as text
		assertEquals(withoutImage.textCount, result.textCount)

		val media = result.collector!!.generated.single()
		assertEquals(
			IMAGE_HREF.replace("&amp;", "&"),
			media.url.value
		)
		assertNull(media.caption)
	}

	@Test
	fun imageIsALinkWhenInlineMediaIsOff() {

		val result = parse("<p>$IMAGE_LINK</p>", withInlineMedia = false)

		assertEquals(0, result.mediaCount)
		assertEquals(1, result.linkButtonCount)
	}

	@Test
	fun giphyReplacesLinkAndEmbeddedImage() {

		val result = parse("<p>$GIPHY_LINK</p>")

		assertEquals(1, result.mediaCount)
		assertEquals(0, result.linkButtonCount)
		assertEquals(1, result.collector!!.generated.size)
	}

	@Test
	fun giphyWithoutDetailsFromReddit() {

		// The body of a real comment, captured on 2026-10-29
		val body = "<p><a href=\"https://giphy.com/gifs/NA7ANK2VDczE12jLwv\" target=\"_blank\">" +
				"https://giphy.com/gifs/NA7ANK2VDczE12jLwv</a></p>"

		val metadata = InlineMediaResolverTest.decodeMediaMetadata(
			"""{"giphy|NA7ANK2VDczE12jLwv": {"status": "invalid"}}"""
		)

		val asLink = parse(body, resolver = InlineMediaResolver(metadata))
		assertEquals(0, asLink.mediaCount)
		assertEquals(1, asLink.linkButtonCount)

		val loadedDirectly = parse(
			body,
			resolver = InlineMediaResolver(metadata, loadGiphyDirectly = true)
		)
		assertEquals(1, loadedDirectly.mediaCount)
		assertEquals(0, loadedDirectly.linkButtonCount)
	}

	@Test
	fun imageWithinTextKeepsItsPosition() {

		val result = parse("<p>Look at this $IMAGE_LINK it's great</p>")

		assertEquals(
			listOf(
				BodyElementTextSpanned::class.java,
				BodyElementInlineMedia::class.java,
				BodyElementTextSpanned::class.java
			),
			result.elements.filter {
				it is BodyElementTextSpanned || it is BodyElementInlineMedia
			}.map { it.javaClass }.take(3)
		)

		assertEquals(0, result.linkButtonCount)
	}

	@Test
	fun captionIsKept() {

		val result = parse("<p><a href=\"$IMAGE_HREF\">My cat</a></p>")

		assertEquals("My cat", result.collector!!.generated.single().caption)
	}

	@Test
	fun otherLinksAreUnchanged() {

		val result = parse("<p><a href=\"https://example.com/page\">A link</a></p>")

		assertEquals(0, result.mediaCount)
		assertEquals(1, result.linkButtonCount)
	}
}
