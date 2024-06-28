package org.quantumbadger.redreader.compose.theme

import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.lightColorScheme
import androidx.compose.runtime.Composable
import androidx.compose.runtime.CompositionLocalProvider
import androidx.compose.runtime.staticCompositionLocalOf
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.sp
import org.quantumbadger.redreader.compose.prefs.ComposePrefs
import org.quantumbadger.redreader.compose.prefs.LocalComposePrefs

interface ComposeThemePostCard {
	val title: TextStyle
	val subtitle: TextStyle
	val caption: TextStyle
	val iconColor: Color
	val backgroundColor: Color
	val listBackgroundColor: Color
	val previewImageBackgroundColor: Color
}

interface ComposeThemeAlbum {
	val toolbarIconColor: Color
	val title: TextStyle
	val subtitle: TextStyle
}

interface ComposeThemeDropdownMenu {
	val text: TextStyle
}

interface ComposeTheme {
	val dropdownMenu: ComposeThemeDropdownMenu
	val postCard: ComposeThemePostCard
	val album: ComposeThemeAlbum
}

@Composable
fun RRComposeContextTheme(
	content: @Composable () -> Unit,
) {
	val prefs = LocalComposePrefs.current

	val theme = ComposeThemeLight(prefs)

	MaterialTheme(
		// TODO set all values from theme
		colorScheme = lightColorScheme(
			primary = Color(0xE7, 0x00, 0x00),
			secondary = Color(0xD0, 0x00, 0x00),
		)
	) {
		CompositionLocalProvider(LocalComposeTheme provides theme) {
			content()
		}
	}
}

class ComposeThemeLight(prefs: ComposePrefs) : ComposeTheme {

	val baseTextStyle = TextStyle()

	override val postCard = object : ComposeThemePostCard {

		override val title = baseTextStyle.copy(
			color = Color.Black,
			fontWeight = FontWeight.W600,
			fontSize = 18.sp * prefs.appearanceFontScalePosts
		)

		override val subtitle = baseTextStyle.copy(
			color = Color.DarkGray,
			fontWeight = FontWeight.W400,
			fontSize = 14.sp * prefs.appearanceFontScalePostSubtitles
		)

		override val caption = baseTextStyle.copy(
			color = Color.Black,
			fontWeight = FontWeight.W500,
			fontSize = 16.sp * prefs.appearanceFontScalePostSubtitles
		)

		override val iconColor = Color(0x44, 0x44, 0x44)
		override val backgroundColor = Color.White
		override val listBackgroundColor = Color(0.97f, 0.97f, 0.97f)
		override val previewImageBackgroundColor = Color(0.96f, 0.96f, 0.96f)
	}

	override val album = object : ComposeThemeAlbum {
		override val title = baseTextStyle.copy(
			color = Color.Black,
			fontWeight = FontWeight.W600,
			fontSize = 22.sp * prefs.appearanceFontScalePosts // TODO different setting?
		)

		override val subtitle = baseTextStyle.copy(
			color = Color.DarkGray,
			fontWeight = FontWeight.W400,
			fontSize = 16.sp * prefs.appearanceFontScalePostSubtitles
		)

		override val toolbarIconColor = Color(0x44, 0x44, 0x44)
	}

	override val dropdownMenu = object : ComposeThemeDropdownMenu {
		override val text = baseTextStyle.copy(
			color = Color.Black,
			fontWeight = FontWeight.W500,
			fontSize = 16.sp * prefs.appearanceFontScaleGlobal // TODO different setting
		)
	}
}

val LocalComposeTheme =
	staticCompositionLocalOf<ComposeTheme> { throw RuntimeException("Theme not initialized") }
