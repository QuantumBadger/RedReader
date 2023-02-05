package org.quantumbadger.redreader.reddit.kthings

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
sealed class RedditBoolOrTimestampUTC {

	@Serializable(with = RedditBoolOrTimestampUTCBoolSerializer::class)
	data class Bool(val value: Boolean) : RedditBoolOrTimestampUTC()

	@Serializable(with = RedditBoolOrTimestampUTCTimestampSerializer::class)
	data class Timestamp(val value: RedditTimestampUTC) : RedditBoolOrTimestampUTC()
}

object RedditBoolOrTimestampUTCSerializer : JsonContentPolymorphicSerializer<RedditBoolOrTimestampUTC>(
	RedditBoolOrTimestampUTC::class
) {
	override fun selectDeserializer(element: JsonElement): KSerializer<out RedditBoolOrTimestampUTC> {

		if (!(element is JsonPrimitive)) {
			throw SerializationException("Expecting JSON primitive for BoolOrTimestamp")
		}

		return if (element.booleanOrNull == null) {
			RedditBoolOrTimestampUTC.Timestamp.serializer()
		} else {
			RedditBoolOrTimestampUTC.Bool.serializer()
		}
	}
}

class RedditBoolOrTimestampUTCBoolSerializer : KSerializer<RedditBoolOrTimestampUTC.Bool> {
	override val descriptor: SerialDescriptor
		get() = PrimitiveSerialDescriptor("RedditBoolOrTimestampUTCBool", PrimitiveKind.BOOLEAN)

	override fun deserialize(decoder: Decoder)
		= RedditBoolOrTimestampUTC.Bool(decoder.decodeBoolean())

	override fun serialize(encoder: Encoder, value: RedditBoolOrTimestampUTC.Bool) {
		encoder.encodeBoolean(value.value)
	}
}

class RedditBoolOrTimestampUTCTimestampSerializer : KSerializer<RedditBoolOrTimestampUTC.Timestamp> {
	override val descriptor: SerialDescriptor
		get() = PrimitiveSerialDescriptor("RedditBoolOrTimestampUTCBool", PrimitiveKind.BOOLEAN)

	override fun deserialize(decoder: Decoder) = RedditBoolOrTimestampUTC.Timestamp(
		decoder.decodeSerializableValue(RedditTimestampUTC.serializer()))

	override fun serialize(encoder: Encoder, value: RedditBoolOrTimestampUTC.Timestamp) {
		encoder.encodeSerializableValue(RedditTimestampUTC.serializer(), value.value)
	}
}
