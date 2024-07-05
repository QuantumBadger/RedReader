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

package org.quantumbadger.redreader.compose.ctx

import android.content.Intent
import androidx.compose.runtime.Composable
import androidx.compose.runtime.CompositionLocalProvider
import androidx.compose.runtime.DisposableEffect
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableIntStateOf
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.setValue
import androidx.compose.runtime.staticCompositionLocalOf
import org.quantumbadger.redreader.account.RedditAccountId
import org.quantumbadger.redreader.account.RedditAccountManager
import org.quantumbadger.redreader.activities.RedditTermsActivity
import org.quantumbadger.redreader.common.AndroidCommon
import org.quantumbadger.redreader.common.General
import org.quantumbadger.redreader.common.LinkHandler
import org.quantumbadger.redreader.common.RRError
import org.quantumbadger.redreader.common.UriString
import org.quantumbadger.redreader.compose.activity.ComposeBaseActivity
import org.quantumbadger.redreader.compose.prefs.ComposePrefsSingleton
import org.quantumbadger.redreader.compose.prefs.LocalComposePrefs
import org.quantumbadger.redreader.compose.prefs.Preference
import org.quantumbadger.redreader.compose.theme.RRComposeContextTheme
import org.quantumbadger.redreader.fragments.AccountListDialog
import org.quantumbadger.redreader.fragments.ErrorPropertiesDialog
import org.quantumbadger.redreader.image.AlbumInfo
import org.quantumbadger.redreader.settings.SettingsActivity

@Composable
fun RRComposeContext(
	activity: ComposeBaseActivity,
	content: @Composable () -> Unit
) {
	var currentAccountId by remember { mutableStateOf(RedditAccountId.ANON) }

	DisposableEffect(Unit) {
		val accountManager = RedditAccountManager.getInstance(activity)

		val updateListener = {
			AndroidCommon.runOnUiThread {
				currentAccountId = RedditAccountId(accountManager.defaultAccount.canonicalUsername)
			}
		}

		accountManager.addUpdateListener(updateListener)

		onDispose {
			accountManager.removeUpdateListener(updateListener)
		}
	}

	CompositionLocalProvider(
		LocalRedditUser provides currentAccountId,
		LocalComposePrefs provides ComposePrefsSingleton.instance,
		LocalLauncher provides {
			when (it) {
				Dest.Settings -> {
					activity.startActivity(Intent(activity, SettingsActivity::class.java))
				}

				is Dest.Link -> {
					LinkHandler.onLinkClicked(
						activity = activity,
						url = it.url,
						albumInfo = it.albumInfo,
						albumImageIndex = it.albumImageIndex
					)
				}

				is Dest.LinkLongClick -> {
					LinkHandler.onLinkLongClicked(
						activity = activity,
						uri = it.url,
						forceNoImage = false
					)
				}

				is Dest.ResultDialog -> {
					General.showResultDialog(activity, it.error)
				}

				is Dest.SaveMedia -> {
					LinkHandler.onActionMenuItemSelected(
						uri = it.url,
						activity = activity,
						action = LinkHandler.LinkAction.SAVE_IMAGE
					)
				}

				is Dest.ShareLink -> {
					LinkHandler.onActionMenuItemSelected(
						uri = it.url,
						activity = activity,
						action = LinkHandler.LinkAction.SHARE
					)
				}

				is Dest.ShareMedia -> {
					LinkHandler.onActionMenuItemSelected(
						uri = it.url,
						activity = activity,
						action = LinkHandler.LinkAction.SHARE_IMAGE
					)
				}

				is Dest.ErrorPropertiesDialog -> {
					ErrorPropertiesDialog.newInstance(it.error)
						.show(activity.supportFragmentManager, null)
				}

				Dest.AccountsList -> {
					AccountListDialog.show(activity)
				}

				Dest.RedditTerms -> {
					RedditTermsActivity.launch(activity, false)
				}
			}
		},
	) {
		RRComposeContextTheme {
			content()
		}
	}
}

val LocalRedditUser = staticCompositionLocalOf { RedditAccountId.ANON }

val LocalLauncher = staticCompositionLocalOf<(Dest) -> Unit> {
	throw Exception("LocalLauncher not set")
}

// Increment this to retry all failed in-scope network requests
val GlobalNetworkRetry = mutableIntStateOf(0)

sealed interface Dest {

	data object Settings : Dest

	data class Link(
		val url: UriString,
		val albumInfo: AlbumInfo? = null,
		val albumImageIndex: Int? = null
	) : Dest

	data class LinkLongClick(
		val url: UriString
	) : Dest

	data class ResultDialog(
		val error: RRError
	) : Dest

	data class ErrorPropertiesDialog(
		val error: RRError
	) : Dest

	data class SaveMedia(
		val url: UriString
	) : Dest

	data class ShareMedia(
		val url: UriString
	) : Dest

	data class ShareLink(
		val url: UriString
	) : Dest

	data object RedditTerms : Dest

	data object AccountsList : Dest
}

private fun <T> testPref(value: T) = object : Preference<T> {
	override var value: T
		get() = value
		set(value) {}
}
