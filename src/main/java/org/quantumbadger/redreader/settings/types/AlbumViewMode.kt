package org.quantumbadger.redreader.settings.types

enum class AlbumViewMode(
	override val stringValue: String
): SerializableEnum<AlbumViewMode> {
	Cards("cards"),
	List("list"),
	Grid("grid");

	companion object {
		val settingSerializer = EnumSettingSerializer(entries)
	}
}
