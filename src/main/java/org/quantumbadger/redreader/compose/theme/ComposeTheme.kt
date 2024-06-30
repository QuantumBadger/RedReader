package org.quantumbadger.redreader.compose.theme

import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.darkColorScheme
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
import org.quantumbadger.redreader.settings.types.AppearanceTheme
import org.quantumbadger.redreader.settings.types.ThemeLightness

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
	val themePref = prefs.appearanceTheme.value

	val theme = ComposeThemeImpl(prefs)

	MaterialTheme(
		colorScheme = when (themePref.lightness) {
			ThemeLightness.Light -> lightColorScheme(
				primary = themePref.colorPrimary,
				secondary = themePref.colorPrimaryDark,
			)

			ThemeLightness.Dark -> darkColorScheme(
				primary = themePref.colorPrimary,
				secondary = themePref.colorPrimaryDark,
			)
		}
	) {
		CompositionLocalProvider(LocalComposeTheme provides theme) {
			content()
		}
	}
}

class ComposeThemeImpl(prefs: ComposePrefs) : ComposeTheme {

	val theme = prefs.appearanceTheme.value
	val light = theme.lightness == ThemeLightness.Light

	val baseTextStyle = TextStyle()

	val colorText = if (light) Color.Black else Color.White
	val colorSubtext = if (light) Color.DarkGray else Color.LightGray
	val colorIcon = if (light) Color(0x44, 0x44, 0x44) else Color(0xAA, 0xAA, 0xAA)

	val colorCardBackground = if (light) {
		Color.White
	} else if (theme == AppearanceTheme.ULTRABLACK) {
		Color.Black
	} else {
		Color(0x22, 0x22, 0x22)
	}

	val colorListBackground = if (light) {
		Color(0.97f, 0.97f, 0.97f)
	} else {
		Color.Black
	}

	val colorImageBackground = if (light) {
		Color(0.96f, 0.96f, 0.96f)
	} else {
		Color(0x33, 0x33, 0x33)
	}

	override val postCard = object : ComposeThemePostCard {

		override val title = baseTextStyle.copy(
			color = colorText,
			fontWeight = FontWeight.W600,
			fontSize = 18.sp * prefs.appearanceFontScalePosts
		)

		override val subtitle = baseTextStyle.copy(
			color = colorSubtext,
			fontWeight = FontWeight.W400,
			fontSize = 14.sp * prefs.appearanceFontScalePostSubtitles
		)

		override val caption = baseTextStyle.copy(
			color = colorText,
			fontWeight = FontWeight.W500,
			fontSize = 16.sp * prefs.appearanceFontScalePostSubtitles
		)

		override val iconColor = colorIcon
		override val backgroundColor = colorCardBackground
		override val listBackgroundColor = colorListBackground
		override val previewImageBackgroundColor = colorImageBackground
	}

	override val album = object : ComposeThemeAlbum {
		override val title = baseTextStyle.copy(
			color = colorText,
			fontWeight = FontWeight.W600,
			fontSize = 22.sp * prefs.appearanceFontScalePosts // TODO different setting?
		)

		override val subtitle = baseTextStyle.copy(
			color = colorSubtext,
			fontWeight = FontWeight.W400,
			fontSize = 16.sp * prefs.appearanceFontScalePostSubtitles
		)

		override val toolbarIconColor = colorIcon
	}

	override val dropdownMenu = object : ComposeThemeDropdownMenu {
		override val text = baseTextStyle.copy(
			color = colorText,
			fontWeight = FontWeight.W500,
			fontSize = 16.sp * prefs.appearanceFontScaleGlobal // TODO different setting
		)
	}
}

val LocalComposeTheme =
	staticCompositionLocalOf<ComposeTheme> { throw RuntimeException("Theme not initialized") }
