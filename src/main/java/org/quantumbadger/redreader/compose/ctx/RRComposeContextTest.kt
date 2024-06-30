package org.quantumbadger.redreader.compose.ctx

import androidx.compose.runtime.Composable
import androidx.compose.runtime.CompositionLocalProvider
import org.quantumbadger.redreader.compose.prefs.ComposePrefs
import org.quantumbadger.redreader.compose.prefs.LocalComposePrefs
import org.quantumbadger.redreader.compose.prefs.Preference
import org.quantumbadger.redreader.compose.theme.ComposeThemeImpl
import org.quantumbadger.redreader.compose.theme.LocalComposeTheme
import org.quantumbadger.redreader.settings.types.AlbumViewMode
import org.quantumbadger.redreader.settings.types.AppearanceTheme

@Composable
fun RRComposeContextTest(content: @Composable () -> Unit) {

	val prefValues = object : ComposePrefs {
		override val appearanceTheme: Preference<AppearanceTheme> = testPref(AppearanceTheme.RED)
		override val appearanceFontScaleGlobal = 1f
		override val appearanceFontScaleBodyText = 1f
		override val appearanceFontScalePosts = 1f
		override val appearanceFontScalePostSubtitles = 1f
		override val albumViewMode = testPref(AlbumViewMode.Cards)
		override val albumCardShowButtons = testPref(true)
		override val albumListShowThumbnails = testPref(true)
		override val albumGridCropToSquare = testPref(false)
		override val albumGridColumns = testPref(3)
		override val albumListThumbnailSize = testPref(64)
		override val albumListShowButtons = testPref(true)
	}

	CompositionLocalProvider(LocalComposePrefs provides prefValues) {
		CompositionLocalProvider(LocalComposeTheme provides ComposeThemeImpl(prefValues)) {
			content()
		}
	}
}

private fun <T> testPref(value: T) = object : Preference<T> {
	override var value: T
		get() = value
		set(value) {}
}
