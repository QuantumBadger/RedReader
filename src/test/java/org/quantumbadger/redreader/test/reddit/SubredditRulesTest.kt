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
import org.junit.Assert.assertFalse
import org.junit.Assert.assertNotNull
import org.junit.Assert.assertNull
import org.junit.Assert.assertTrue
import org.junit.Test
import org.quantumbadger.redreader.http.PostField
import org.quantumbadger.redreader.jsonwrap.JsonValue
import org.quantumbadger.redreader.reddit.api.ReportReason
import org.quantumbadger.redreader.reddit.api.SubredditRuleKind
import org.quantumbadger.redreader.reddit.api.SubredditRules
import java.io.File

class SubredditRulesTest {

	// Real response from /r/AskReddit/about/rules, captured 2026-07-19
	private fun loadTestData(): JsonValue =
		File("src/test/resources/reddit/subreddit_rules_askreddit.json")
			.inputStream()
			.use(JsonValue::parse)

	@Test
	fun testParseSubredditRules() {

		val result = SubredditRules.parse(loadTestData())

		assertNotNull(result)
		result!!

		assertEquals(12, result.rules.size)

		val rule1 = result.rules[0]
		assertEquals(SubredditRuleKind.POST, rule1.kind)
		assertEquals(
			"Rule 1 - Questions must be clear and direct and may not use the body textbox",
			rule1.shortName
		)
		assertEquals(rule1.shortName, rule1.violationReason)

		assertEquals(SubredditRuleKind.COMMENT, result.rules[8].kind)

		assertEquals(
			11,
			result.rules.count { it.appliesTo(SubredditRuleKind.POST) }
		)
		assertEquals(
			8,
			result.rules.count { it.appliesTo(SubredditRuleKind.COMMENT) }
		)
	}

	@Test
	fun testParseSiteRulesFlow() {

		val result = SubredditRules.parse(loadTestData())

		assertNotNull(result)
		result!!

		assertEquals(4, result.siteRulesFlow.size)

		// Simple leaf reason
		val spam = result.siteRulesFlow[0]
		assertEquals("This is spam", spam.reasonTextToShow)
		assertEquals("This is spam", spam.reasonText)
		assertTrue(spam.nextStepReasons.isEmpty())
		assertFalse(spam.fileComplaint)

		// Nested reasons, two levels deep
		val abusive = result.siteRulesFlow[2]
		assertEquals("This is abusive or harassing", abusive.reasonTextToShow)
		assertEquals("In what way?", abusive.nextStepHeader)
		assertNull(abusive.reasonText) // Empty string in the JSON
		assertEquals(4, abusive.nextStepReasons.size)

		val targeted = abusive.nextStepReasons[0]
		assertEquals("It's targeted harassment", targeted.reasonTextToShow)
		assertEquals(2, targeted.nextStepReasons.size)
		assertEquals(
			"It's targeted harassment at me",
			targeted.nextStepReasons[0].reasonText
		)

		// Leaf with optional notes
		val reportAbuse = abusive.nextStepReasons[3]
		assertTrue(reportAbuse.canWriteNotes)
		assertEquals("Additional information (optional)", reportAbuse.notesInputTitle)

		// Complaint entries are handled on the Reddit website
		val other = result.siteRulesFlow[3]
		val copyright = other.nextStepReasons[0]
		assertEquals("It infringes my copyright", copyright.reasonTextToShow)
		assertTrue(copyright.fileComplaint)
		assertNotNull(copyright.complaintUrl)
		assertNotNull(copyright.complaintPrompt)

		// Crisis support entry requires a username
		val selfHarm = other.nextStepReasons.last()
		assertTrue(selfHarm.canSpecifyUsernames)
		assertEquals("Username", selfHarm.usernamesInputTitle)
	}

	@Test
	fun testParseInvalidData() {
		assertNull(SubredditRules.parse(JsonValue.parse("{}".byteInputStream())))
		assertNull(SubredditRules.parse(JsonValue.parse("[1, 2, 3]".byteInputStream())))
		assertNull(SubredditRules.parse(JsonValue.parse(
			"{\"rules\": [{\"kind\": \"link\"}], \"site_rules_flow\": [{}]}"
				.byteInputStream())))
	}

	@Test
	fun testRuleReasonFields() {
		assertEquals(
			listOf(
				PostField("reason", "rule_reason_selected"),
				PostField("rule_reason", "No spam")
			),
			ReportReason.Rule("No spam").toPostFields()
		)

		// Long rule names are truncated to the API limit
		val longName = "a".repeat(150)
		assertEquals(
			listOf(
				PostField("reason", "rule_reason_selected"),
				PostField("rule_reason", "a".repeat(100))
			),
			ReportReason.Rule(longName).toPostFields()
		)
	}

	@Test
	fun testSiteReasonFields() {
		assertEquals(
			listOf(
				PostField("reason", "site_reason_selected"),
				PostField("site_reason", "This is spam")
			),
			ReportReason.Site("This is spam").toPostFields()
		)

		// Blank notes and usernames are omitted
		assertEquals(
			listOf(
				PostField("reason", "site_reason_selected"),
				PostField("site_reason", "It's abusing the report button")
			),
			ReportReason.Site(
				reasonText = "It's abusing the report button",
				customText = "  ",
				usernames = ""
			).toPostFields()
		)

		assertEquals(
			listOf(
				PostField("reason", "site_reason_selected"),
				PostField("site_reason", "Someone is considering suicide or serious self-harm."),
				PostField("custom_text", "Some notes"),
				PostField("usernames", "someuser")
			),
			ReportReason.Site(
				reasonText = "Someone is considering suicide or serious self-harm.",
				customText = "Some notes",
				usernames = " someuser "
			).toPostFields()
		)
	}

	@Test
	fun testOtherReasonFields() {
		assertEquals(
			listOf(
				PostField("reason", "other"),
				PostField("other_reason", "Some other reason")
			),
			ReportReason.Other("Some other reason").toPostFields()
		)
	}
}
