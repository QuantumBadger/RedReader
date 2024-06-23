package org.quantumbadger.redreader.compose.ctx

import androidx.compose.runtime.Composable
import androidx.compose.runtime.CompositionLocalProvider
import org.quantumbadger.redreader.compose.prefs.ComposePrefs
import org.quantumbadger.redreader.compose.prefs.LocalComposePrefs
import org.quantumbadger.redreader.compose.theme.ComposeThemeTest
import org.quantumbadger.redreader.compose.theme.LocalComposeTheme

@Composable
fun RRComposeContextTest(content: @Composable () -> Unit) {

	val prefValues = object : ComposePrefs {
		override val appearanceFontScaleBodyText = 1f
		override val appearanceFontScalePosts = 1f
		override val appearanceFontScalePostSubtitles = 1f
	}

	CompositionLocalProvider(LocalComposePrefs provides prefValues) {
		CompositionLocalProvider(LocalComposeTheme provides ComposeThemeTest(prefValues)) {
			content()
		}
	}
}
