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

package org.quantumbadger.redreader.reddit.kthings

import android.os.Parcelable
import kotlinx.parcelize.Parcelize
import kotlinx.serialization.KSerializer
import kotlinx.serialization.Serializable
import kotlinx.serialization.SerializationException
import kotlinx.serialization.descriptors.PrimitiveKind
import kotlinx.serialization.descriptors.PrimitiveSerialDescriptor
import kotlinx.serialization.descriptors.SerialDescriptor
import kotlinx.serialization.encoding.Decoder
import kotlinx.serialization.encoding.Encoder
import kotlinx.serialization.json.JsonContentPolymorphicSerializer
import kotlinx.serialization.json.JsonElement
import kotlinx.serialization.json.JsonPrimitive
import kotlinx.serialization.json.booleanOrNull

@Serializable(with = RedditBoolOrTimestampUTCSerializer::class)
@Parcelize
sealed class RedditFieldEdited : Parcelable {

	@Serializable(with = RedditFieldEditedBoolSerializer::class)
	@Parcelize
	data class Bool(val value: Boolean) : RedditFieldEdited()

	@Serializable(with = RedditFieldEditedTimestampSerializer::class)
	@Parcelize
	data class Timestamp(val value: RedditTimestampUTC) : RedditFieldEdited()
}

object RedditBoolOrTimestampUTCSerializer : JsonContentPolymorphicSerializer<RedditFieldEdited>(
	RedditFieldEdited::class
) {
	override fun selectDeserializer(element: JsonElement): KSerializer<out RedditFieldEdited> {

		if (!(element is JsonPrimitive)) {
			throw SerializationException("Expecting JSON primitive for BoolOrTimestamp")
		}

		return if (element.booleanOrNull == null) {
			RedditFieldEdited.Timestamp.serializer()
		} else {
			RedditFieldEdited.Bool.serializer()
		}
	}
}

object RedditFieldEditedBoolSerializer : KSerializer<RedditFieldEdited.Bool> {
	override val descriptor: SerialDescriptor
		get() = PrimitiveSerialDescriptor("RedditFieldEdited.Bool", PrimitiveKind.BOOLEAN)

	override fun deserialize(decoder: Decoder)
		= RedditFieldEdited.Bool(decoder.decodeBoolean())

	override fun serialize(encoder: Encoder, value: RedditFieldEdited.Bool) {
		encoder.encodeBoolean(value.value)
	}
}

object RedditFieldEditedTimestampSerializer : KSerializer<RedditFieldEdited.Timestamp> {
	override val descriptor: SerialDescriptor
		get() = PrimitiveSerialDescriptor("RedditFieldEdited.Timestamp", PrimitiveKind.BOOLEAN)

	override fun deserialize(decoder: Decoder) = RedditFieldEdited.Timestamp(
		decoder.decodeSerializableValue(RedditTimestampUTC.serializer()))

	override fun serialize(encoder: Encoder, value: RedditFieldEdited.Timestamp) {
		encoder.encodeSerializableValue(RedditTimestampUTC.serializer(), value.value)
	}
}
