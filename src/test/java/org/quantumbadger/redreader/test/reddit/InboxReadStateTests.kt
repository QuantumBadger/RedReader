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

import org.junit.Assert
import org.junit.Test
import org.quantumbadger.redreader.reddit.kthings.JsonUtils
import org.quantumbadger.redreader.reddit.kthings.RedditThing

class InboxReadStateTests {

	private fun message(extraFields: String) = """
		{
			"kind": "t4",
			"data": {
				"id": "abc123",
				"name": "t4_abc123",
				"author": "someone",
				"subject": "Hello",
				"body": "Hi there",
				"body_html": "&lt;p&gt;Hi there&lt;/p&gt;",
				"created_utc": 1700000000.0
				$extraFields
			}
		}
	""".trimIndent()

	private fun comment(extraFields: String) = """
		{
			"kind": "t1",
			"data": {
				"id": "def456",
				"name": "t1_def456",
				"author": "someone",
				"body": "A reply",
				"body_html": "&lt;p&gt;A reply&lt;/p&gt;",
				"context": "/r/test/comments/xyz/_/def456/?context=3",
				"created_utc": 1700000000.0
				$extraFields
			}
		}
	""".trimIndent()

	@Test
	fun unreadMessageIsParsedAsNew() {
		val thing = JsonUtils.decodeRedditThingFromStream(
			message(""", "new": true""").byteInputStream())

		Assert.assertEquals(true, (thing as RedditThing.Message).data.new)
	}

	@Test
	fun readMessageIsParsedAsNotNew() {
		val thing = JsonUtils.decodeRedditThingFromStream(
			message(""", "new": false""").byteInputStream())

		Assert.assertEquals(false, (thing as RedditThing.Message).data.new)
	}

	@Test
	fun messageWithoutNewFieldHasUnknownReadState() {
		val thing = JsonUtils.decodeRedditThingFromStream(
			message("").byteInputStream())

		Assert.assertNull((thing as RedditThing.Message).data.new)
	}

	@Test
	fun unreadCommentReplyIsParsedAsNew() {
		val thing = JsonUtils.decodeRedditThingFromStream(
			comment(""", "new": true""").byteInputStream())

		Assert.assertEquals(true, (thing as RedditThing.Comment).data.new)
	}

	@Test
	fun commentWithoutNewFieldHasUnknownReadState() {
		val thing = JsonUtils.decodeRedditThingFromStream(
			comment("").byteInputStream())

		Assert.assertNull((thing as RedditThing.Comment).data.new)
	}
}
