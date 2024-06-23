package org.quantumbadger.redreader.compose.theme

import androidx.compose.runtime.staticCompositionLocalOf
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.sp
import org.quantumbadger.redreader.compose.prefs.ComposePrefs

interface ComposeThemePostCard {
	val title: TextStyle
	val subtitle: TextStyle
	val iconColor: Color
	val backgroundColor: Color
	val listBackgroundColor: Color
}

interface ComposeTheme {
	val postCard: ComposeThemePostCard
}

class ComposeThemeTest(prefs: ComposePrefs) : ComposeTheme {
	override val postCard = object : ComposeThemePostCard {

		val baseTextStyle = TextStyle()

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

		override val iconColor = Color.Black
		override val backgroundColor = Color.White
		override val listBackgroundColor = Color(0.9f, 0.9f, 0.9f)
	}
}

val LocalComposeTheme =
	staticCompositionLocalOf<ComposeTheme> { throw RuntimeException("Theme not initialized") }
