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

package org.quantumbadger.redreader.compose.theme

import android.app.Activity
import androidx.compose.foundation.ExperimentalFoundationApi
import androidx.compose.foundation.combinedClickable
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.Text
import androidx.compose.material3.darkColorScheme
import androidx.compose.material3.lightColorScheme
import androidx.compose.runtime.Composable
import androidx.compose.runtime.CompositionLocalProvider
import androidx.compose.runtime.Immutable
import androidx.compose.runtime.SideEffect
import androidx.compose.runtime.staticCompositionLocalOf
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.Shape
import androidx.compose.ui.hapticfeedback.HapticFeedbackType
import androidx.compose.ui.platform.LocalHapticFeedback
import androidx.compose.ui.platform.LocalView
import androidx.compose.ui.semantics.Role
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.Dp
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import androidx.core.view.WindowCompat
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
	val titleCompact: TextStyle
}

interface ComposeThemeDropdownMenu {
	val text: TextStyle
	val background: Color
}

interface ComposeThemeError {
	val title: TextStyle
	val message: TextStyle
	val border: Color
	val background: Color
	val primaryButton: ComposeThemeButton
	val secondaryButton: ComposeThemeButton
}

@Immutable
data class ComposeThemeButton(
	val text: TextStyle,
	val background: Color,
	val shape: Shape,
	val border: Color? = null,
	val borderThickness: Dp? = null,
)

@Immutable
data class ComposeThemeLinkButton(
	val title: TextStyle,
	val subtitle: TextStyle,
	val shape: Shape,
	val borderColor: Color,
	val borderThickness: Dp,
	val iconColor: Color
)

interface ComposeTheme {
	val dropdownMenu: ComposeThemeDropdownMenu
	val postCard: ComposeThemePostCard
	val album: ComposeThemeAlbum
	val error: ComposeThemeError
	val linkButton: ComposeThemeLinkButton
}

@Composable
fun RRComposeContextTheme(
	content: @Composable () -> Unit,
) {
	val prefs = LocalComposePrefs.current
	val themePref = prefs.appearanceTheme.value

	val theme = ComposeThemeImpl(prefs)

	val view = LocalView.current
	if (!view.isInEditMode) {
		SideEffect {
			val window = (view.context as Activity).window
			WindowCompat.getInsetsController(window, view).apply {
				isAppearanceLightStatusBars = themePref.lightness == ThemeLightness.Light
			}
		}
	}

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

	val colorText = when (theme) {
		AppearanceTheme.GRUVBOX_LIGHT -> Colors.Gruvbox.Light.Strong.fg
		AppearanceTheme.GRUVBOX_DARK -> Colors.Gruvbox.Dark.Strong.fg
		else -> if (light) {
			Colors.Grey.s10
		} else if (theme == AppearanceTheme.NIGHT_LOWCONTRAST) {
			Colors.Grey.s2
		} else {
			Colors.Grey.s1
		}
	}

	val colorSubtext = when (theme) {
		AppearanceTheme.GRUVBOX_LIGHT -> Colors.Gruvbox.Light.Muted.fg
		AppearanceTheme.GRUVBOX_DARK -> Colors.Gruvbox.Dark.Muted.fg
		else -> if (light) {
			Colors.Grey.s6
		} else if (theme == AppearanceTheme.NIGHT_LOWCONTRAST) {
			Colors.Grey.s5
		} else {
			Colors.Grey.s3
		}
	}

	val colorIcon = when (theme) {
		AppearanceTheme.GRUVBOX_LIGHT -> Colors.Gruvbox.Light.Muted.fg
		AppearanceTheme.GRUVBOX_DARK -> Colors.Gruvbox.Dark.Muted.fg
		else -> if (light) {
			Colors.Grey.s7
		} else if (theme == AppearanceTheme.NIGHT_LOWCONTRAST) {
			Colors.Grey.s6
		} else {
			Colors.Grey.s4
		}
	}

	val colorCardBackground = when (theme) {
		AppearanceTheme.GRUVBOX_LIGHT -> Colors.Gruvbox.Light.Background.neutral1
		AppearanceTheme.GRUVBOX_DARK -> Colors.Gruvbox.Dark.Background.neutral1
		else -> if (light) {
			Color.White
		} else if (theme == AppearanceTheme.ULTRABLACK) {
			Color.Black
		} else {
			Colors.Grey.s9
		}
	}

	val colorListBackground = when (theme) {
		AppearanceTheme.GRUVBOX_LIGHT -> Colors.Gruvbox.Light.Background.hard
		AppearanceTheme.GRUVBOX_DARK -> Colors.Gruvbox.Dark.Background.hard
		else -> if (light) {
			Colors.Grey.s1
		} else if (theme == AppearanceTheme.NIGHT_LOWCONTRAST) {
			Colors.Grey.s10
		} else {
			Color.Black
		}
	}

	val colorPopupBackground = when (theme) {
		AppearanceTheme.GRUVBOX_LIGHT -> Colors.Gruvbox.Light.Background.neutral2
		AppearanceTheme.GRUVBOX_DARK -> Colors.Gruvbox.Dark.Background.neutral2
		else -> if (light) {
			Color.White
		} else if (theme == AppearanceTheme.NIGHT_LOWCONTRAST) {
			Colors.Grey.s7
		} else {
			Colors.Grey.s8
		}
	}

	val colorImageBackground = when (theme) {
		AppearanceTheme.GRUVBOX_LIGHT -> Colors.Gruvbox.Light.Background.neutral3
		AppearanceTheme.GRUVBOX_DARK -> Colors.Gruvbox.Dark.Background.neutral3
		else -> if (light) {
			Colors.Grey.s2
		} else {
			Colors.Grey.s8
		}
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
		override val titleCompact = baseTextStyle.copy(
			color = colorText,
			fontWeight = FontWeight.W600,
			fontSize = 16.sp
		)

		override val toolbarIconColor = colorIcon
	}

	override val dropdownMenu = object : ComposeThemeDropdownMenu {
		override val text = baseTextStyle.copy(
			color = colorText,
			fontWeight = FontWeight.W500,
			fontSize = 16.sp * prefs.appearanceFontScaleGlobal // TODO different setting
		)
		override val background = colorPopupBackground
	}

	override val error = object : ComposeThemeError {
		override val title = baseTextStyle.copy(
			color = colorText,
			fontWeight = FontWeight.W600,
			fontSize = 16.sp
		)

		override val message = baseTextStyle.copy(
			color = colorSubtext,
			fontWeight = FontWeight.W400,
			fontSize = 13.sp
		)

		override val background = if (light) {
			Colors.Red.s0
		} else {
			Colors.Red.s10
		}

		override val border = if (light) {
			Colors.Red.s4
		} else {
			Colors.Red.s7
		}

		override val primaryButton = ComposeThemeButton(
			text = baseTextStyle.copy(
				color = Color.White,
				fontWeight = FontWeight.W500,
				fontSize = 13.sp
			),
			background = if (light) {
				Colors.Red.s5
			} else {
				Colors.Red.s7
			},
			shape = RoundedCornerShape(6.dp)
		)

		override val secondaryButton = ComposeThemeButton(
			text = baseTextStyle.copy(
				color = colorText,
				fontWeight = FontWeight.W500,
				fontSize = 13.sp
			),
			background = Color.Transparent,
			shape = RoundedCornerShape(6.dp),
		)
	}

	override val linkButton = ComposeThemeLinkButton(
		title = baseTextStyle.copy(
			color = colorText,
			fontWeight = FontWeight.W500,
			fontSize = 14.sp
		),
		subtitle = baseTextStyle.copy(
			color = colorSubtext,
			fontWeight = FontWeight.W400,
			fontSize = 12.sp
		),
		shape = RoundedCornerShape(6.dp),
		borderColor = if (light) Colors.Grey.s4 else Colors.Grey.s7,
		borderThickness = 1.dp,
		iconColor = if (light) Colors.Grey.s4 else Colors.Grey.s7
	)
}

val LocalComposeTheme =
	staticCompositionLocalOf<ComposeTheme> { throw RuntimeException("Theme not initialized") }

object Colors {
	object Grey {
		val s1 = Color(0xFFF8F9FA)
		val s2 = Color(0xFFE9ECEF)
		val s3 = Color(0xFFDEE2E6)
		val s4 = Color(0xFFCED4DA)
		val s5 = Color(0xFFADB5BD)
		val s6 = Color(0xFF6C757D)
		val s7 = Color(0xFF495057)
		val s8 = Color(0xFF343A40)
		val s9 = Color(0xFF212529)
		val s10 = Color(0xFF111213)
	}

	object Red {
		val s0 = Color(0xFFfdf6f6)
		val s1 = Color(0xFFf6d8d8)
		val s2 = Color(0xFFeeb2b2)
		val s3 = Color(0xFFe68c8c)
		val s4 = Color(0xFFde6565)
		val s5 = Color(0xFFd63f3f)
		val mid = Color(0xFFd32f2f)
		val s6 = Color(0xFFbf2828)
		val s7 = Color(0xFF992020)
		val s8 = Color(0xFF721818)
		val s9 = Color(0xFF4c1010)
		val s10 = Color(0xFF260808)
	}

	object Gruvbox {
		object Dark {
			object Muted {
				val bg = Color(0xFF282828)
				val red = Color(0xFFCC241D)
				val green = Color(0xFF98971A)
				val yellow = Color(0xFFD79921)
				val blue = Color(0xFF458588)
				val purple = Color(0xFFB16286)
				val aqua = Color(0xFF689D6A)
				val orange = Color(0xFFD65D0E)
				val fg = Color(0xFFA89984)
			}
			object Strong {
				val bg = Color(0xFF928374) // This seems wrong based on typical Gruvbox usage, using the muted hard background instead.
				val red = Color(0xFFFB4934)
				val green = Color(0xFFB8BB26)
				val yellow = Color(0xFFFABD2F)
				val blue = Color(0xFF83A598)
				val purple = Color(0xFFD3869B)
				val aqua = Color(0xFF8EC07C)
				val orange = Color(0xFFFE8019)
				val fg = Color(0xFFEBDBB2)
			}
            object Background {
                val hard = Color(0xFF1D2021)
                val soft = Color(0xFF32302F)
                val neutral1 = Color(0xFF3C3836)
                val neutral2 = Color(0xFF504945)
                val neutral3 = Color(0xFF665C54)
                val neutral4 = Color(0xFF7C6F64)
            }
		}
		object Light {
			object Muted {
				val bg = Color(0xFFFBF1C7)
				val red = Color(0xFFCC241D)
				val green = Color(0xFF98971A)
				val yellow = Color(0xFFD79921)
				val blue = Color(0xFF458588)
				val purple = Color(0xFFB16286)
				val aqua = Color(0xFF689D6A)
				val orange = Color(0xFFD65D0E)
				val fg = Color(0xFF7C6F64)
			}
			object Strong {
				val bg = Color(0xFF928374) // This seems wrong based on typical Gruvbox usage, using the muted hard background instead.
				val red = Color(0xFF9d0006)
				val green = Color(0xFF79740E)
				val yellow = Color(0xFFB57614)
				val blue = Color(0xFF076678)
				val purple = Color(0xFF8F3F71)
				val aqua = Color(0xFF427B58)
				val orange = Color(0xFAF4C7) // This seems wrong based on typical Gruvbox usage, using the muted hard background instead.
				val fg = Color(0xFF3C3836)
			}
             object Background {
                val hard = Color(0xFFF9F5D7)
                val soft = Color(0xFFF2E5BC)
                val neutral1 = Color(0xFFEBDBB2)
                val neutral2 = Color(0xFFD5C4A1)
                val neutral3 = Color(0xBDAE93)
                val neutral4 = Color(0xA89984)
            }
		}
	}
}

@Composable
fun TextStyle.StyledText(
	text: String,
	modifier: Modifier = Modifier,
	overflow: TextOverflow = TextOverflow.Clip,
	maxLines: Int = Int.MAX_VALUE
) {
	Text(
		modifier = modifier,
		text = text,
		style = this,
		fontSize = fontSize,
		fontWeight = fontWeight,
		color = color,
		overflow = overflow,
		maxLines = maxLines
	)
}

@OptIn(ExperimentalFoundationApi::class)
@Composable
fun Modifier.combinedClickableWithHaptics(
	enabled: Boolean = true,
	onClickLabel: String? = null,
	role: Role? = null,
	onLongClickLabel: String? = null,
	onLongClick: (() -> Unit)? = null,
	onDoubleClick: (() -> Unit)? = null,
	onClick: () -> Unit
): Modifier {
	val haptics = LocalHapticFeedback.current

	return this.combinedClickable(
		enabled = enabled,
		onClickLabel = onClickLabel,
		role = role,
		onLongClickLabel = onLongClickLabel,
		onLongClick = onLongClick?.let {
			{
				haptics.performHapticFeedback(HapticFeedbackType.LongPress)
				it()
			}
		},
		onDoubleClick = onDoubleClick,
		onClick = onClick
	)
}
