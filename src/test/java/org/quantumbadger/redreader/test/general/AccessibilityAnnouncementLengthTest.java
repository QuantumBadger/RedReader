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

import org.junit.Test;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import java.io.File;
import java.util.ArrayList;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

public class AccessibilityAnnouncementLengthTest {

	// TalkBack only reports an announcement uninterruptibly when it is at most
	// VERBOSE_UTTERANCE_THRESHOLD_CHARACTERS (30) characters long. Longer
	// announcements are merely queued, and the queue is flushed when
	// accessibility focus moves, so an over-length announcement made just
	// before a focus change (such as an action confirmation) is discarded
	// silently. This test keeps every locale's announcement strings under
	// the limit.
	private static final int TALKBACK_UNINTERRUPTIBLE_MAX_LENGTH = 30;

	private static final String ANNOUNCEMENT_PREFIX = "accessibility_announcement_";

	@Test
	public void announcementStringsFitTalkBackThreshold() throws Exception {

		final File resDir = new File("src/main/res");

		assertTrue(
				"Resource directory not found: " + resDir.getAbsolutePath(),
				resDir.isDirectory());

		final File[] valueDirs = resDir.listFiles(
				(dir, name) -> name.startsWith("values"));

		assertNotNull(valueDirs);

		final DocumentBuilder builder
				= DocumentBuilderFactory.newInstance().newDocumentBuilder();

		final ArrayList<String> failures = new ArrayList<>();
		int checked = 0;

		for(final File valueDir : valueDirs) {

			final File stringsFile = new File(valueDir, "strings.xml");

			if(!stringsFile.isFile()) {
				continue;
			}

			final Document document = builder.parse(stringsFile);
			final NodeList strings = document.getElementsByTagName("string");

			for(int i = 0; i < strings.getLength(); i++) {

				final Element element = (Element)strings.item(i);
				final String name = element.getAttribute("name");

				if(!name.startsWith(ANNOUNCEMENT_PREFIX)) {
					continue;
				}

				final String text = unescapeAndroidString(element.getTextContent());
				checked++;

				if(text.length() > TALKBACK_UNINTERRUPTIBLE_MAX_LENGTH) {
					failures.add(valueDir.getName() + "/" + name + " is "
							+ text.length() + " characters: \"" + text + "\"");
				}
			}
		}

		assertTrue(
				"No announcement strings found; has the naming prefix changed?",
				checked > 0);

		assertTrue(
				"Announcement strings longer than TalkBack's uninterruptible "
						+ "limit of " + TALKBACK_UNINTERRUPTIBLE_MAX_LENGTH
						+ " characters (these would be silently discarded when "
						+ "accessibility focus moves):\n"
						+ String.join("\n", failures),
				failures.isEmpty());
	}

	private static String unescapeAndroidString(final String s) {

		String res = s.trim();

		if(res.length() >= 2
				&& res.startsWith("\"")
				&& res.endsWith("\"")) {
			res = res.substring(1, res.length() - 1);
		}

		return res
				.replace("\\'", "'")
				.replace("\\\"", "\"")
				.replace("\\n", "\n")
				.replace("\\t", "\t");
	}
}
