package org.quantumbadger.redreader.reddit.kthings

import android.os.Parcelable
import kotlinx.parcelize.Parcelize
import kotlinx.serialization.KSerializer
import kotlinx.serialization.Serializable
import kotlinx.serialization.descriptors.PrimitiveKind
import kotlinx.serialization.descriptors.PrimitiveSerialDescriptor
import kotlinx.serialization.descriptors.SerialDescriptor
import kotlinx.serialization.encoding.Decoder
import kotlinx.serialization.encoding.Encoder
import org.quantumbadger.redreader.common.time.TimestampUTC

@Serializable(with = RedditTimestampUTCSerializer::class)
@Parcelize
data class RedditTimestampUTC(
	val value: TimestampUTC
) : Parcelable

object RedditTimestampUTCSerializer : KSerializer<RedditTimestampUTC> {
	override val descriptor: SerialDescriptor
		get() = PrimitiveSerialDescriptor("RedditTimestampUTC", PrimitiveKind.DOUBLE)

	override fun deserialize(decoder: Decoder) =
		RedditTimestampUTC(value = TimestampUTC.fromUtcSecs((decoder.decodeDouble().toLong())))

	override fun serialize(encoder: Encoder, value: RedditTimestampUTC) {
		encoder.encodeDouble(value.value.toUtcSecs().toDouble())
	}
}
