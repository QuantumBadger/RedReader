package org.quantumbadger.redreader.reddit.kthings

import kotlinx.serialization.KSerializer
import kotlinx.serialization.Serializable
import kotlinx.serialization.descriptors.PrimitiveKind
import kotlinx.serialization.descriptors.PrimitiveSerialDescriptor
import kotlinx.serialization.descriptors.SerialDescriptor
import kotlinx.serialization.encoding.Decoder
import kotlinx.serialization.encoding.Encoder

@Serializable(with = RedditIdAndTypeSerializer::class)
data class RedditIdAndType(
	val value: String
)

object RedditIdAndTypeSerializer : KSerializer<RedditIdAndType> {
	override val descriptor: SerialDescriptor
		get() = PrimitiveSerialDescriptor("RedditIdAndType", PrimitiveKind.STRING)

	override fun deserialize(decoder: Decoder) = RedditIdAndType(decoder.decodeString())

	override fun serialize(encoder: Encoder, value: RedditIdAndType) {
		encoder.encodeString(value.value)
	}
}
