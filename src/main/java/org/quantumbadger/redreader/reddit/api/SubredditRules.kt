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

package org.quantumbadger.redreader.reddit.api

import androidx.compose.runtime.Immutable
import org.quantumbadger.redreader.http.PostField
import org.quantumbadger.redreader.jsonwrap.JsonObject
import org.quantumbadger.redreader.jsonwrap.JsonValue

/**
 * The kind of thing a subreddit rule applies to, from the `kind` field of
 * `/r/subreddit/about/rules`.
 */
enum class SubredditRuleKind {
	POST,
	COMMENT,
	ALL
}

/**
 * A single subreddit rule from `/r/subreddit/about/rules`. When reporting,
 * [shortName] is submitted as the `rule_reason` field, and [violationReason]
 * is the text shown to the user.
 */
@Immutable
data class SubredditRule(
	val kind: SubredditRuleKind,
	val shortName: String,
	val violationReason: String
) {
	fun appliesTo(kind: SubredditRuleKind) =
		this.kind == SubredditRuleKind.ALL || this.kind == kind
}

/**
 * One entry in the dynamic tree of sitewide report reasons, from the
 * `site_rules_flow` field of `/r/subreddit/about/rules`.
 *
 * An entry either has children in [nextStepReasons], or is a leaf. A leaf
 * normally has a non-empty [reasonText] which is submitted as the
 * `site_reason` field, but if [fileComplaint] is set then the report is
 * instead made on the Reddit website via [complaintUrl].
 */
@Immutable
data class SiteRulesFlowEntry(
	val reasonTextToShow: String,
	val reasonText: String?,
	val nextStepHeader: String?,
	val nextStepReasons: List<SiteRulesFlowEntry>,
	val fileComplaint: Boolean,
	val complaintPrompt: String?,
	val complaintButtonText: String?,
	val complaintUrl: String?,
	val canWriteNotes: Boolean,
	val notesInputTitle: String?,
	val canSpecifyUsernames: Boolean,
	val usernamesInputTitle: String?
)

@Immutable
data class SubredditRules(
	val rules: List<SubredditRule>,
	val siteRulesFlow: List<SiteRulesFlowEntry>
) {
	companion object {
		/**
		 * Parses the response from `/r/subreddit/about/rules`. Returns null if the
		 * response is not in the expected format. Individual malformed entries are
		 * skipped rather than failing the whole parse, since Reddit changes this
		 * data over time.
		 */
		@JvmStatic
		fun parse(value: JsonValue): SubredditRules? {

			val obj = value.asObject() ?: return null

			val rules = obj.getArray("rules")?.mapNotNull {
				it.asObject()?.let(::parseRule)
			} ?: emptyList()

			val siteRulesFlow = obj.getArray("site_rules_flow")?.mapNotNull {
				it.asObject()?.let(::parseFlowEntry)
			} ?: emptyList()

			if (rules.isEmpty() && siteRulesFlow.isEmpty()) {
				return null
			}

			return SubredditRules(rules, siteRulesFlow)
		}

		private fun parseRule(obj: JsonObject): SubredditRule? {

			val shortName = obj.getString("short_name") ?: return null

			val kind = when (obj.getString("kind")) {
				"link" -> SubredditRuleKind.POST
				"comment" -> SubredditRuleKind.COMMENT
				else -> SubredditRuleKind.ALL
			}

			return SubredditRule(
				kind = kind,
				shortName = shortName,
				violationReason = obj.getString("violation_reason") ?: shortName
			)
		}

		private fun parseFlowEntry(obj: JsonObject): SiteRulesFlowEntry? {

			val reasonTextToShow = obj.getString("reasonTextToShow") ?: return null

			val children = obj.getArray("nextStepReasons")?.mapNotNull {
				it.asObject()?.let(::parseFlowEntry)
			} ?: emptyList()

			val reasonText = obj.getString("reasonText")?.takeIf { it.isNotBlank() }
			val fileComplaint = obj.getBoolean("fileComplaint") == true
			val complaintUrl = obj.getString("complaintUrl")

			// A leaf entry must have something we can act on
			if (children.isEmpty()
					&& reasonText == null
					&& !(fileComplaint && complaintUrl != null)) {
				return null
			}

			return SiteRulesFlowEntry(
				reasonTextToShow = reasonTextToShow,
				reasonText = reasonText,
				nextStepHeader = obj.getString("nextStepHeader"),
				nextStepReasons = children,
				fileComplaint = fileComplaint,
				complaintPrompt = obj.getString("complaintPrompt"),
				complaintButtonText = obj.getString("complaintButtonText"),
				complaintUrl = complaintUrl,
				canWriteNotes = obj.getBoolean("canWriteNotes") == true,
				notesInputTitle = obj.getString("notesInputTitle"),
				canSpecifyUsernames = obj.getBoolean("canSpecifyUsernames") == true,
				usernamesInputTitle = obj.getString("usernamesInputTitle")
			)
		}
	}
}

/**
 * Everything needed to display the report flow for a thing in a particular
 * subreddit: the rules data, plus whether the subreddit allows free-form
 * report reasons (the `free_form_reports` field of `/r/subreddit/about`).
 */
@Immutable
data class SubredditReportFlow(
	val rules: List<SubredditRule>,
	val siteRulesFlow: List<SiteRulesFlowEntry>,
	val freeFormReports: Boolean
)

/**
 * A reason chosen by the user for reporting a post/comment, in the form
 * expected by the `/api/report` endpoint.
 */
sealed class ReportReason {

	abstract fun toPostFields(): List<PostField>

	/** A subreddit rule: submits the rule's `short_name`. */
	data class Rule(val ruleShortName: String) : ReportReason() {
		override fun toPostFields() = listOf(
			PostField("reason", "rule_reason_selected"),
			PostField("rule_reason", ruleShortName.take(MAX_REASON_LENGTH))
		)
	}

	/** A sitewide reason from the `site_rules_flow` tree. */
	data class Site(
		val reasonText: String,
		val customText: String? = null,
		val usernames: String? = null
	) : ReportReason() {
		override fun toPostFields() = buildList {
			add(PostField("reason", "site_reason_selected"))
			add(PostField("site_reason", reasonText.take(MAX_REASON_LENGTH)))
			customText?.takeIf { it.isNotBlank() }?.let {
				add(PostField("custom_text", it.take(MAX_CUSTOM_TEXT_LENGTH)))
			}
			usernames?.takeIf { it.isNotBlank() }?.let {
				add(PostField("usernames", it.trim()))
			}
		}
	}

	/** A free-form reason, only permitted when `free_form_reports` is enabled. */
	data class Other(val text: String) : ReportReason() {
		override fun toPostFields() = listOf(
			PostField("reason", "other"),
			PostField("other_reason", text.take(MAX_REASON_LENGTH))
		)
	}

	companion object {
		// Maximum lengths from https://www.reddit.com/dev/api/#POST_api_report
		const val MAX_REASON_LENGTH = 100
		const val MAX_CUSTOM_TEXT_LENGTH = 2000
	}
}
