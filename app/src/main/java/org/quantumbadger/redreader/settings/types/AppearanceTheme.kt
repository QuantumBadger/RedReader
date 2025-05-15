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

package org.quantumbadger.redreader.settings.types

import androidx.compose.ui.graphics.Color
import org.quantumbadger.redreader.compose.theme.Colors

enum class AppearanceTheme(
	override val stringValue: String,
	val lightness: ThemeLightness,
	val colorPrimary: Color,
	val colorPrimaryDark: Color,
) : SerializableEnum<AppearanceTheme> {
	RED(
		stringValue = "red",
		lightness = ThemeLightness.Light,
		colorPrimary = Colors.Red.mid,
		colorPrimaryDark = Colors.Red.s6
	),
	GREEN(
		stringValue = "green",
		lightness = ThemeLightness.Light,
		colorPrimary = Color(0x4c, 0xaf, 0x50),
		colorPrimaryDark = Color(0x38, 0x8e, 0x3c)
	),
	BLUE(
		stringValue = "blue",
		lightness = ThemeLightness.Light,
		colorPrimary = Color(0x12, 0x62, 0x91),
		colorPrimaryDark = Color(0x10, 0x49, 0x6b)
	),
	LTBLUE(
		stringValue = "ltblue",
		lightness = ThemeLightness.Light,
		colorPrimary = Color(0x03, 0xa9, 0xf4),
		colorPrimaryDark = Color(0x02, 0x88, 0xd1)
	),
	ORANGE(
		stringValue = "orange",
		lightness = ThemeLightness.Light,
		colorPrimary = Color(0xff, 0x98, 0x00),
		colorPrimaryDark = Color(0xf5, 0x7c, 0x00)
	),
	GRAY(
		stringValue = "gray",
		lightness = ThemeLightness.Light,
		colorPrimary = Color(0x22, 0x22, 0x22),
		colorPrimaryDark = Color(0x60, 0x7d, 0x8b)
	),
	NIGHT(
		stringValue = "night",
		lightness = ThemeLightness.Dark,
		colorPrimary = Color(0xCC, 0xCC, 0xCC),
		colorPrimaryDark = Color.Black
	),
	NIGHT_LOWCONTRAST(
		stringValue = "night_lowcontrast",
		lightness = ThemeLightness.Dark,
		colorPrimary = Color(0xCC, 0xCC, 0xCC),
		colorPrimaryDark = Color.Black
	),
	ULTRABLACK(
		stringValue = "ultrablack",
		lightness = ThemeLightness.Dark,
		colorPrimary = Color(0xCC, 0xCC, 0xCC),
		colorPrimaryDark = Color.Black
	);

	companion object {
		val settingSerializer = EnumSettingSerializer(AppearanceTheme.entries)
	}
}

enum class ThemeLightness {
	Light,
	Dark
}
