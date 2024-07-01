package org.quantumbadger.redreader.compose.ctx

import androidx.appcompat.app.AppCompatDialogFragment
import androidx.compose.runtime.Composable
import androidx.compose.runtime.CompositionLocalProvider
import androidx.compose.runtime.DisposableEffect
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.setValue
import androidx.compose.runtime.staticCompositionLocalOf
import org.quantumbadger.redreader.account.RedditAccountId
import org.quantumbadger.redreader.account.RedditAccountManager
import org.quantumbadger.redreader.common.AndroidCommon
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
		LocalDialogLauncher provides {
			it.show(activity.supportFragmentManager, null)
		}
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

val LocalDialogLauncher = staticCompositionLocalOf<DialogLauncher> {
	throw Exception("LocalDialogLauncher not set")
}

fun interface DialogLauncher {
	fun launch(dialog: AppCompatDialogFragment)
}

private fun <T> testPref(value: T) = object : Preference<T> {
	override var value: T
		get() = value
		set(value) {}
}
