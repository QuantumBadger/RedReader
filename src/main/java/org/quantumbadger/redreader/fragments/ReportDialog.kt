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

package org.quantumbadger.redreader.fragments

import android.os.Bundle
import androidx.appcompat.app.AppCompatActivity
import androidx.compose.runtime.Composable
import androidx.compose.runtime.mutableStateOf
import androidx.core.os.BundleCompat
import org.quantumbadger.redreader.R
import org.quantumbadger.redreader.account.RedditAccountManager
import org.quantumbadger.redreader.activities.BugReportActivity
import org.quantumbadger.redreader.cache.CacheManager
import org.quantumbadger.redreader.common.AndroidCommon
import org.quantumbadger.redreader.common.General
import org.quantumbadger.redreader.common.LinkHandler
import org.quantumbadger.redreader.common.RRError
import org.quantumbadger.redreader.common.UriString
import org.quantumbadger.redreader.compose.activity.ComposeDialogFragment
import org.quantumbadger.redreader.compose.ui.ReportScreen
import org.quantumbadger.redreader.reddit.APIResponseHandler
import org.quantumbadger.redreader.reddit.RedditAPI
import org.quantumbadger.redreader.reddit.api.ReportReason
import org.quantumbadger.redreader.reddit.api.SiteRulesFlowEntry
import org.quantumbadger.redreader.reddit.api.SubredditRuleKind
import org.quantumbadger.redreader.reddit.kthings.RedditIdAndType

/**
 * Dialog for reporting a post or comment. Shows the subreddit's rules and the
 * sitewide report reasons, then submits the report to the Reddit API.
 */
class ReportDialog : ComposeDialogFragment() {

	companion object {
		private const val ARG_ID_AND_TYPE = "idAndType"
		private const val ARG_SUBREDDIT = "subreddit"
		private const val ARG_IS_COMMENT = "isComment"

		@JvmStatic
		fun show(
			activity: AppCompatActivity,
			idAndType: RedditIdAndType,
			subredditName: String,
			isComment: Boolean
		) {
			ReportDialog().apply {
				arguments = Bundle().apply {
					putParcelable(ARG_ID_AND_TYPE, idAndType)
					putString(ARG_SUBREDDIT, subredditName)
					putBoolean(ARG_IS_COMMENT, isComment)
				}
			}.show(activity.supportFragmentManager, null)
		}
	}

	private val idAndType: RedditIdAndType by lazy {
		BundleCompat.getParcelable(
			requireArguments(),
			ARG_ID_AND_TYPE,
			RedditIdAndType::class.java
		)!!
	}

	private val subredditName: String by lazy {
		requireArguments().getString(ARG_SUBREDDIT)!!
	}

	private val submitting = mutableStateOf(false)

	@Composable
	override fun DialogContent() {
		ReportScreen(
			subredditName = subredditName,
			targetKind = if (requireArguments().getBoolean(ARG_IS_COMMENT)) {
				SubredditRuleKind.COMMENT
			} else {
				SubredditRuleKind.POST
			},
			submitting = submitting.value,
			onCancel = ::dismiss,
			onSubmit = ::submit,
			onFileComplaint = ::fileComplaint
		)
	}

	private fun submit(reason: ReportReason) {

		val activity = baseActivity
		val user = RedditAccountManager.getInstance(activity).defaultAccount

		submitting.value = true

		RedditAPI.report(
			CacheManager.getInstance(activity),
			object : APIResponseHandler.ActionResponseHandler(activity) {
				override fun onCallbackException(t: Throwable) {
					BugReportActivity.handleGlobalError(activity, t)
				}

				override fun onFailure(error: RRError) {
					AndroidCommon.runOnUiThread {
						submitting.value = false
						General.showResultDialog(activity, error)
					}
				}

				override fun onSuccess() {
					AndroidCommon.runOnUiThread {
						General.quickToast(activity, R.string.report_success)
						dismissAllowingStateLoss()
					}
				}
			},
			user,
			idAndType,
			subredditName,
			reason.toPostFields(),
			activity
		)
	}

	private fun fileComplaint(entry: SiteRulesFlowEntry) {

		val url = entry.complaintUrl ?: return

		// The URL contains a Python-style template for the thing's fullname,
		// URL-encoded once: %(thing)s -> %25%28thing%29s
		val resolved = url
			.replace("%25%28thing%29s", idAndType.value)
			.replace("%(thing)s", idAndType.value)

		LinkHandler.onLinkClicked(baseActivity, UriString(resolved))

		dismiss()
	}
}
