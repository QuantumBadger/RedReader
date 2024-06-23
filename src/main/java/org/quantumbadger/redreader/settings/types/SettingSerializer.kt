package org.quantumbadger.redreader.settings.types

interface SettingSerializer<T> {
	fun serialize(value: T): String
	fun deserialize(value: String): T?
}

interface SerializableEnum<T : Enum<T>> {
	val stringValue: String
}

class EnumSettingSerializer<T>(
	values: Iterable<T>
) : SettingSerializer<T> where T : SerializableEnum<T>, T : Enum<T> {

	private val lookupTable = values.associateBy { it.stringValue }

	override fun serialize(value: T) = value.stringValue
	override fun deserialize(value: String) = lookupTable[value]
}
