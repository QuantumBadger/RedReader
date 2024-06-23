package org.quantumbadger.redreader.compose.ctx

import androidx.compose.runtime.Composable
import androidx.compose.runtime.CompositionLocalProvider
import org.quantumbadger.redreader.compose.prefs.ComposePrefsSingleton
import org.quantumbadger.redreader.compose.prefs.LocalComposePrefs
import org.quantumbadger.redreader.compose.prefs.Preference
import org.quantumbadger.redreader.compose.theme.ComposeThemeLight
import org.quantumbadger.redreader.compose.theme.LocalComposeTheme

@Composable
fun RRComposeContext(content: @Composable () -> Unit) {

	CompositionLocalProvider(LocalComposePrefs provides ComposePrefsSingleton.instance) {

		val prefs = LocalComposePrefs.current

		CompositionLocalProvider(LocalComposeTheme provides ComposeThemeLight(prefs)) {
			content()
		}
	}
}

private fun <T> testPref(value: T) = object : Preference<T> {
	override var value: T
		get() = value
		set(value) {}
}
