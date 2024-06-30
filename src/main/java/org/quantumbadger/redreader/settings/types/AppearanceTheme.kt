package org.quantumbadger.redreader.settings.types

import androidx.compose.ui.graphics.Color

enum class AppearanceTheme(
	override val stringValue: String,
	val lightness: ThemeLightness,
	val colorPrimary: Color,
	val colorPrimaryDark: Color,
) : SerializableEnum<AppearanceTheme> {
	RED(
		stringValue = "red",
		lightness = ThemeLightness.Light,
		colorPrimary = Color(0xd3, 0x2f, 0x2f),
		colorPrimaryDark = Color(0xb5, 0x26, 0x26)
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
