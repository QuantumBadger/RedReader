package org.quantumbadger.redreader.reddit.kthings

import kotlinx.serialization.KSerializer
import kotlinx.serialization.Serializable
import kotlinx.serialization.descriptors.PrimitiveKind
import kotlinx.serialization.descriptors.PrimitiveSerialDescriptor
import kotlinx.serialization.descriptors.SerialDescriptor
import kotlinx.serialization.encoding.Decoder
import kotlinx.serialization.encoding.Encoder
import org.quantumbadger.redreader.common.time.TimestampUTC

@Serializable(with = RedditTimestampUTCSerializer::class)
data class RedditTimestampUTC(
	val value: TimestampUTC
)

object RedditTimestampUTCSerializer : KSerializer<RedditTimestampUTC> {
	override val descriptor: SerialDescriptor
		get() = PrimitiveSerialDescriptor("RedditTimestampUTC", PrimitiveKind.DOUBLE)

	override fun deserialize(decoder: Decoder) =
		RedditTimestampUTC(value = TimestampUTC(utcMs = (decoder.decodeDouble().toLong()) * 1000))

	override fun serialize(encoder: Encoder, value: RedditTimestampUTC) {
		encoder.encodeDouble((value.value.utcMs / 1000).toDouble())
	}
}
