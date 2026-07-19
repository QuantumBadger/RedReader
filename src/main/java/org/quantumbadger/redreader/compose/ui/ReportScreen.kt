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

package org.quantumbadger.redreader.compose.ui

import androidx.activity.compose.BackHandler
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.ColumnScope
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.RowScope
import androidx.compose.foundation.layout.Spacer
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.height
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.width
import androidx.compose.foundation.layout.widthIn
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material3.CircularProgressIndicator
import androidx.compose.material3.Icon
import androidx.compose.material3.OutlinedTextField
import androidx.compose.material3.Surface
import androidx.compose.material3.Text
import androidx.compose.material3.TextButton
import androidx.compose.runtime.Composable
import androidx.compose.runtime.Immutable
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.runtime.setValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.semantics.Role
import androidx.compose.ui.semantics.heading
import androidx.compose.ui.semantics.semantics
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import org.quantumbadger.redreader.R
import org.quantumbadger.redreader.compose.ctx.RRComposeContextTest
import org.quantumbadger.redreader.compose.net.NetRequestStatus
import org.quantumbadger.redreader.compose.net.fetchSubredditReportFlow
import org.quantumbadger.redreader.compose.theme.LocalComposeTheme
import org.quantumbadger.redreader.compose.theme.StyledText
import org.quantumbadger.redreader.reddit.api.ReportReason
import org.quantumbadger.redreader.reddit.api.SiteRulesFlowEntry
import org.quantumbadger.redreader.reddit.api.SubredditReportFlow
import org.quantumbadger.redreader.reddit.api.SubredditRule
import org.quantumbadger.redreader.reddit.api.SubredditRuleKind

/**
 * The full report flow: fetches the report reasons for the subreddit, then
 * shows the reason list (drilling into nested sitewide reasons), and finally
 * any extra input fields required by the selected reason.
 */
@Composable
fun ReportScreen(
	subredditName: String,
	targetKind: SubredditRuleKind,
	submitting: Boolean,
	onCancel: () -> Unit,
	onSubmit: (ReportReason) -> Unit,
	onFileComplaint: (SiteRulesFlowEntry) -> Unit
) {
	val flowStatus by fetchSubredditReportFlow(subredditName)

	ReportDialogSurface {
		when (val it = flowStatus) {
			NetRequestStatus.Connecting, is NetRequestStatus.Downloading -> {
				ReportProgress()

				CancelButtonRow(onCancel)
			}

			is NetRequestStatus.Failed -> {
				RRErrorView(error = it.error)

				CancelButtonRow(onCancel)
			}

			is NetRequestStatus.Success -> {
				ReportScreenContent(
					flow = it.result,
					targetKind = targetKind,
					submitting = submitting,
					onCancel = onCancel,
					onSubmit = onSubmit,
					onFileComplaint = onFileComplaint
				)
			}
		}
	}
}

@Composable
private fun ColumnScope.ReportScreenContent(
	flow: SubredditReportFlow,
	targetKind: SubredditRuleKind,
	submitting: Boolean,
	onCancel: () -> Unit,
	onSubmit: (ReportReason) -> Unit,
	onFileComplaint: (SiteRulesFlowEntry) -> Unit
) {
	val theme = LocalComposeTheme.current.reportFlow

	val otherLabel = stringResource(R.string.report_reason_other)

	val rootNodes = remember(flow, targetKind, otherLabel) {
		buildReportTree(flow, targetKind, otherLabel)
	}

	var path by rememberSaveable { mutableStateOf(listOf<Int>()) }

	val position = resolvePath(rootNodes, path)
		?: ReportPosition.ListScreen(null, rootNodes)

	BackHandler(enabled = path.isNotEmpty() && !submitting) {
		path = path.dropLast(1)
	}

	if (submitting) {
		ReportProgress()
		return
	}

	when (position) {
		is ReportPosition.ListScreen -> {
			theme.body.StyledText(
				text = position.header
					?: stringResource(R.string.report_prompt_why),
				modifier = Modifier.padding(horizontal = 24.dp)
			)

			Spacer(Modifier.height(4.dp))

			LazyColumn(
				modifier = Modifier
					.fillMaxWidth()
					.weight(1f, fill = false)
			) {
				items(position.nodes.size) { index ->
					ReportOptionRow(position.nodes[index]) {
						path = path + index
					}
				}
			}

			ButtonRow {
				if (path.isNotEmpty()) {
					TextButton(onClick = { path = path.dropLast(1) }) {
						Text(stringResource(R.string.dialog_back))
					}
				}

				Spacer(Modifier.weight(1f))

				TextButton(onClick = onCancel) {
					Text(stringResource(R.string.dialog_cancel))
				}
			}
		}

		is ReportPosition.LeafScreen -> {
			ReportLeafScreen(
				leaf = position.leaf,
				onBack = { path = path.dropLast(1) },
				onCancel = onCancel,
				onSubmit = onSubmit,
				onFileComplaint = onFileComplaint
			)
		}
	}
}

@Composable
private fun ColumnScope.ReportLeafScreen(
	leaf: ReportNode,
	onBack: () -> Unit,
	onCancel: () -> Unit,
	onSubmit: (ReportReason) -> Unit,
	onFileComplaint: (SiteRulesFlowEntry) -> Unit
) {
	val theme = LocalComposeTheme.current.reportFlow

	var notesText by rememberSaveable(leaf) { mutableStateOf("") }
	var usernameText by rememberSaveable(leaf) { mutableStateOf("") }
	var otherText by rememberSaveable(leaf) { mutableStateOf("") }

	Column(
		modifier = Modifier
			.fillMaxWidth()
			.weight(1f, fill = false)
			.padding(horizontal = 24.dp)
	) {
		when (leaf) {
			is ReportNode.ComplaintLeaf -> {
				theme.body.StyledText(
					leaf.entry.complaintPrompt ?: leaf.label
				)
			}

			is ReportNode.OtherLeaf -> {
				OutlinedTextField(
					value = otherText,
					onValueChange = {
						otherText = it.take(ReportReason.MAX_REASON_LENGTH)
					},
					modifier = Modifier.fillMaxWidth(),
					label = {
						Text(stringResource(R.string.report_other_reason_hint))
					}
				)
			}

			else -> {
				theme.item.StyledText(leaf.label)
			}
		}

		if (leaf is ReportNode.SiteLeaf) {
			if (leaf.entry.canSpecifyUsernames) {
				Spacer(Modifier.height(12.dp))

				OutlinedTextField(
					value = usernameText,
					onValueChange = { usernameText = it },
					modifier = Modifier.fillMaxWidth(),
					singleLine = true,
					label = {
						Text(
							leaf.entry.usernamesInputTitle
								?: stringResource(R.string.report_username_hint)
						)
					}
				)
			}

			if (leaf.entry.canWriteNotes) {
				Spacer(Modifier.height(12.dp))

				OutlinedTextField(
					value = notesText,
					onValueChange = {
						notesText = it.take(ReportReason.MAX_CUSTOM_TEXT_LENGTH)
					},
					modifier = Modifier.fillMaxWidth(),
					label = {
						Text(
							leaf.entry.notesInputTitle
								?: stringResource(R.string.report_additional_info_hint)
						)
					}
				)
			}
		}
	}

	ButtonRow {
		TextButton(onClick = onBack) {
			Text(stringResource(R.string.dialog_back))
		}

		Spacer(Modifier.weight(1f))

		TextButton(onClick = onCancel) {
			Text(stringResource(R.string.dialog_cancel))
		}

		when (leaf) {
			is ReportNode.ComplaintLeaf -> {
				TextButton(onClick = { onFileComplaint(leaf.entry) }) {
					Text(
						leaf.entry.complaintButtonText
							?: stringResource(R.string.report_file_complaint)
					)
				}
			}

			is ReportNode.RuleLeaf -> {
				TextButton(onClick = {
					onSubmit(ReportReason.Rule(leaf.shortName))
				}) {
					Text(stringResource(R.string.action_report))
				}
			}

			is ReportNode.SiteLeaf -> {
				TextButton(onClick = {
					onSubmit(ReportReason.Site(
						reasonText = leaf.entry.reasonText ?: leaf.label,
						customText = notesText,
						usernames = usernameText
					))
				}) {
					Text(stringResource(R.string.action_report))
				}
			}

			is ReportNode.OtherLeaf -> {
				TextButton(
					onClick = { onSubmit(ReportReason.Other(otherText)) },
					enabled = otherText.isNotBlank()
				) {
					Text(stringResource(R.string.action_report))
				}
			}

			is ReportNode.Branch -> {
				// Not reachable: branches never appear as a leaf screen
			}
		}
	}
}

@Composable
private fun ReportDialogSurface(
	content: @Composable ColumnScope.() -> Unit
) {
	val theme = LocalComposeTheme.current.reportFlow

	Box(
		modifier = Modifier
			.fillMaxWidth()
			.padding(horizontal = 24.dp, vertical = 24.dp),
		contentAlignment = Alignment.Center
	) {
		Surface(
			modifier = Modifier
				.widthIn(max = 560.dp)
				.fillMaxWidth(),
			shape = RoundedCornerShape(12.dp),
			color = theme.background
		) {
			Column(
				modifier = Modifier.padding(top = 20.dp, bottom = 8.dp)
			) {
				theme.title.StyledText(
					text = stringResource(R.string.action_report),
					modifier = Modifier
						.padding(horizontal = 24.dp)
						.semantics { heading() }
				)

				Spacer(Modifier.height(12.dp))

				content(this)
			}
		}
	}
}

@Composable
private fun ReportProgress() {
	Box(
		modifier = Modifier
			.fillMaxWidth()
			.height(120.dp),
		contentAlignment = Alignment.Center
	) {
		CircularProgressIndicator()
	}
}

@Composable
private fun CancelButtonRow(
	onCancel: () -> Unit
) {
	ButtonRow {
		Spacer(Modifier.weight(1f))

		TextButton(onClick = onCancel) {
			Text(stringResource(R.string.dialog_cancel))
		}
	}
}

@Composable
private fun ButtonRow(
	content: @Composable RowScope.() -> Unit
) {
	Row(
		modifier = Modifier
			.fillMaxWidth()
			.padding(horizontal = 16.dp, vertical = 4.dp),
		verticalAlignment = Alignment.CenterVertically,
		content = content
	)
}

@Composable
private fun ReportOptionRow(
	node: ReportNode,
	onClick: () -> Unit
) {
	val theme = LocalComposeTheme.current.reportFlow

	Row(
		modifier = Modifier
			.fillMaxWidth()
			.clickable(onClick = onClick, role = Role.Button)
			.padding(horizontal = 24.dp, vertical = 12.dp),
		verticalAlignment = Alignment.CenterVertically
	) {
		theme.item.StyledText(
			text = node.label,
			modifier = Modifier.weight(1f)
		)

		if (node is ReportNode.Branch) {
			Spacer(Modifier.width(8.dp))

			Icon(
				painter = painterResource(R.drawable.chevron_right_dark),
				contentDescription = null,
				tint = theme.iconColor
			)
		}
	}
}

@Immutable
private sealed interface ReportNode {
	val label: String

	@Immutable
	data class Branch(
		override val label: String,
		val header: String?,
		val children: List<ReportNode>
	) : ReportNode

	@Immutable
	data class RuleLeaf(
		override val label: String,
		val shortName: String
	) : ReportNode

	@Immutable
	data class SiteLeaf(
		override val label: String,
		val entry: SiteRulesFlowEntry
	) : ReportNode

	@Immutable
	data class ComplaintLeaf(
		override val label: String,
		val entry: SiteRulesFlowEntry
	) : ReportNode

	@Immutable
	data class OtherLeaf(
		override val label: String
	) : ReportNode
}

private sealed interface ReportPosition {
	data class ListScreen(
		val header: String?,
		val nodes: List<ReportNode>
	) : ReportPosition

	data class LeafScreen(
		val leaf: ReportNode
	) : ReportPosition
}

private fun buildReportTree(
	flow: SubredditReportFlow,
	targetKind: SubredditRuleKind,
	otherLabel: String
): List<ReportNode> = buildList {

	flow.rules.filter { it.appliesTo(targetKind) }.forEach {
		add(ReportNode.RuleLeaf(label = it.violationReason, shortName = it.shortName))
	}

	flow.siteRulesFlow.forEach {
		add(buildSiteNode(it))
	}

	if (flow.freeFormReports) {
		add(ReportNode.OtherLeaf(label = otherLabel))
	}
}

private fun buildSiteNode(entry: SiteRulesFlowEntry): ReportNode = when {
	entry.nextStepReasons.isNotEmpty() -> ReportNode.Branch(
		label = entry.reasonTextToShow,
		header = entry.nextStepHeader,
		children = entry.nextStepReasons.map(::buildSiteNode)
	)

	entry.fileComplaint && entry.complaintUrl != null -> ReportNode.ComplaintLeaf(
		label = entry.reasonTextToShow,
		entry = entry
	)

	else -> ReportNode.SiteLeaf(
		label = entry.reasonTextToShow,
		entry = entry
	)
}

private fun resolvePath(
	root: List<ReportNode>,
	path: List<Int>
): ReportPosition? {

	var nodes = root
	var header: String? = null

	path.forEachIndexed { depth, index ->
		when (val node = nodes.getOrNull(index) ?: return null) {
			is ReportNode.Branch -> {
				header = node.header
				nodes = node.children
			}

			else -> return if (depth == path.lastIndex) {
				ReportPosition.LeafScreen(node)
			} else {
				null
			}
		}
	}

	return ReportPosition.ListScreen(header, nodes)
}

@Composable
@Preview
private fun PreviewReportScreen() {
	RRComposeContextTest {
		ReportDialogSurface {
			ReportScreenContent(
				flow = SubredditReportFlow(
					rules = listOf(
						SubredditRule(
							kind = SubredditRuleKind.ALL,
							shortName = "No spam",
							violationReason = "This is spam"
						),
						SubredditRule(
							kind = SubredditRuleKind.POST,
							shortName = "No memes",
							violationReason = "This is a meme"
						)
					),
					siteRulesFlow = listOf(
						SiteRulesFlowEntry(
							reasonTextToShow = "This is abusive or harassing",
							reasonText = null,
							nextStepHeader = "In what way?",
							nextStepReasons = listOf(
								SiteRulesFlowEntry(
									reasonTextToShow = "It's rude or vulgar",
									reasonText = "It's rude or vulgar",
									nextStepHeader = null,
									nextStepReasons = emptyList(),
									fileComplaint = false,
									complaintPrompt = null,
									complaintButtonText = null,
									complaintUrl = null,
									canWriteNotes = false,
									notesInputTitle = null,
									canSpecifyUsernames = false,
									usernamesInputTitle = null
								)
							),
							fileComplaint = false,
							complaintPrompt = null,
							complaintButtonText = null,
							complaintUrl = null,
							canWriteNotes = false,
							notesInputTitle = null,
							canSpecifyUsernames = false,
							usernamesInputTitle = null
						)
					),
					freeFormReports = true
				),
				targetKind = SubredditRuleKind.POST,
				submitting = false,
				onCancel = {},
				onSubmit = {},
				onFileComplaint = {}
			)
		}
	}
}
