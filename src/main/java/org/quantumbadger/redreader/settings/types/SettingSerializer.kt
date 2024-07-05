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
