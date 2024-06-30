package org.quantumbadger.redreader.compose.ctx

import androidx.appcompat.app.AppCompatDialogFragment
import androidx.compose.runtime.Composable
import androidx.compose.runtime.CompositionLocalProvider
import androidx.compose.runtime.staticCompositionLocalOf
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

	CompositionLocalProvider(
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
