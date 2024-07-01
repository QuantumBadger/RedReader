package org.quantumbadger.redreader.compose.ctx

import androidx.appcompat.app.AppCompatDialogFragment
import androidx.compose.runtime.Composable
import androidx.compose.runtime.CompositionLocalProvider
import androidx.compose.runtime.DisposableEffect
import androidx.compose.runtime.Stable
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.setValue
import androidx.compose.runtime.staticCompositionLocalOf
import org.quantumbadger.redreader.account.RedditAccountId
import org.quantumbadger.redreader.account.RedditAccountManager
import org.quantumbadger.redreader.common.AndroidCommon
import org.quantumbadger.redreader.common.LinkHandler
import org.quantumbadger.redreader.common.UriString
import org.quantumbadger.redreader.compose.activity.ComposeBaseActivity
import org.quantumbadger.redreader.compose.prefs.ComposePrefsSingleton
import org.quantumbadger.redreader.compose.prefs.LocalComposePrefs
import org.quantumbadger.redreader.compose.prefs.Preference
import org.quantumbadger.redreader.compose.theme.RRComposeContextTheme

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
		LocalActivity provides activity,
		LocalLauncher provides object : Launcher {
			override fun launch(dialog: AppCompatDialogFragment) {
				dialog.show(activity.supportFragmentManager, null)
			}

			override fun launch(url: UriString) {
				LinkHandler.onLinkClicked(activity, url)
			}

			override fun linkLongClicked(url: UriString) {
				LinkHandler.onLinkLongClicked(
					activity = activity,
					uri = url,
					forceNoImage = false)
			}
		},
	) {
		RRComposeContextTheme {
			content()
		}
	}
}

val LocalRedditUser = staticCompositionLocalOf { RedditAccountId.ANON }

val LocalActivity = staticCompositionLocalOf<ComposeBaseActivity> {
	throw Exception("LocalActivity not set")
}

val LocalLauncher = staticCompositionLocalOf<Launcher> {
	throw Exception("LocalLauncher not set")
}

@Stable
interface Launcher {
	fun launch(dialog: AppCompatDialogFragment)
	fun launch(url: UriString)
	fun linkLongClicked(url: UriString)
}

private fun <T> testPref(value: T) = object : Preference<T> {
	override var value: T
		get() = value
		set(value) {}
}
