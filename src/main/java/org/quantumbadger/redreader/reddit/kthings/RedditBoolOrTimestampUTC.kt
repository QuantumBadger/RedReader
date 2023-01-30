package org.quantumbadger.redreader.reddit.kthings

import kotlinx.serialization.KSerializer
import kotlinx.serialization.Serializable
import kotlinx.serialization.descriptors.*
import kotlinx.serialization.json.*

@Serializable(with = RedditBoolOrTimestampUTCSerializer::class)
sealed class RedditBoolOrTimestampUTC {
	// TODO both the inner classes need custom serializers too...
	@Serializable
	data class Bool(val value: Boolean) : RedditBoolOrTimestampUTC()
	data class Timestamp(val value: RedditTimestampUTC) : RedditBoolOrTimestampUTC()
}

/*@OptIn(ExperimentalSerializationApi::class)
object RedditBoolOrTimestampUTCSerializer : KSerializer<RedditBoolOrTimestampUTC> {
	override val descriptor: SerialDescriptor
		get() = buildSerialDescriptor(
			"RedditBoolOrTimestampUTC",
			SerialKind.CONTEXTUAL,
			PrimitiveSerialDescriptor("RedditBoolOrTimestampUTC", PrimitiveKind.BOOLEAN),
			PrimitiveSerialDescriptor("RedditBoolOrTimestampUTC", PrimitiveKind.DOUBLE),
		)

	override fun deserialize(decoder: Decoder) =
		RedditTimestampUTC(value = TimestampUTC(utcMs = (decoder.decodeDouble().toLong()) * 1000))

	override fun serialize(encoder: Encoder, value: RedditTimestampUTC) {
		encoder.encodeDouble((value.value.utcMs / 1000).toDouble())
	}
}*/

object RedditBoolOrTimestampUTCSerializer : JsonContentPolymorphicSerializer<RedditBoolOrTimestampUTC>(RedditBoolOrTimestampUTC::class) {
	override fun selectDeserializer(element: JsonElement): KSerializer<RedditBoolOrTimestampUTC.Bool> {

		if (!(element is JsonPrimitive)) {
			throw
		}

		return if (element.booleanOrNull == null) {
			RedditBoolOrTimestampUTC.Timestamp.serializer()
		} else {
			RedditBoolOrTimestampUTC.Bool.serializer()
		}
	}
}
