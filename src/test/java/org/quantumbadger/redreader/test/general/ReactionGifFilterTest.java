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

package org.quantumbadger.redreader.test.general;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;
import org.quantumbadger.redreader.reddit.ReactionGifFilter;

public class ReactionGifFilterTest {

	private final ReactionGifFilter mFilter
			= new ReactionGifFilter(ReactionGifFilter.DEFAULT_DOMAINS);

	@Test
	public void testGifOnly() {
		assertTrue(mFilter.isReactionGif("https://giphy.com/gifs/pynZagVcYxVUk"));
		assertTrue(mFilter.isReactionGif("![gif](giphy|pynZagVcYxVUk)"));
		assertTrue(mFilter.isReactionGif("![gif](giphy|pynZagVcYxVUk|downsized)"));
		assertTrue(mFilter.isReactionGif("https://media.giphy.com/media/abc/giphy.gif"));
		assertTrue(mFilter.isReactionGif("https://tenor.com/view/foo-bar-123"));
		assertTrue(mFilter.isReactionGif("https://i.imgur.com/abc.gifv"));
		assertTrue(mFilter.isReactionGif("[this](https://giphy.com/gifs/abc)"));
	}

	@Test
	public void testGifWithFewWords() {
		assertTrue(mFilter.isReactionGif("lol\n\n![gif](giphy|abc)"));
		assertTrue(mFilter.isReactionGif("Me right now: https://giphy.com/gifs/abc"));
		assertTrue(mFilter.isReactionGif("I can't even ![gif](giphy|abc)"));
	}

	@Test
	public void testGifWithRealText() {
		assertFalse(mFilter.isReactionGif(
				"This scene absolutely destroyed me when I saw it ![gif](giphy|abc)"));
		assertFalse(mFilter.isReactionGif(
				"Beautiful comment from a beautiful man. https://giphy.com/gifs/abc"));
	}

	@Test
	public void testNotGif() {
		assertFalse(mFilter.isReactionGif("lol"));
		assertFalse(mFilter.isReactionGif(""));
		assertFalse(mFilter.isReactionGif(null));
		assertFalse(mFilter.isReactionGif("https://en.wikipedia.org/wiki/Patrick_Stewart"));
		assertFalse(mFilter.isReactionGif("![img](abc123)"));
		assertFalse(mFilter.isReactionGif("https://notgiphy.com/abc"));
		assertFalse(mFilter.isReactionGif(
				"source https://www.bbc.co.uk/news https://giphy.com/gifs/abc"));
	}

	@Test
	public void testCustomDomains() {
		final ReactionGifFilter filter = new ReactionGifFilter("*.example.org,  gfycat.com");
		assertTrue(filter.isReactionGif("https://cdn.example.org/x"));
		assertTrue(filter.isReactionGif("https://gfycat.com/x"));
		assertFalse(filter.isReactionGif("https://giphy.com/gifs/abc"));
		assertTrue(filter.isReactionGif("![gif](giphy|abc)"));
	}
}
