package org.quantumbadger.redreader.reddit.kthings

import android.os.Parcel
import android.os.Parcelable
import kotlinx.parcelize.Parceler
import kotlinx.parcelize.Parcelize
import kotlinx.parcelize.WriteWith
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
	val value: @WriteWith<TimestampUTCParceler> TimestampUTC
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

object TimestampUTCParceler : Parceler<TimestampUTC> {
	override fun create(parcel: Parcel)
			= TimestampUTC.fromUtcMs(parcel.readLong())

	override fun TimestampUTC.write(parcel: Parcel, flags: Int) {
		parcel.writeLong(toUtcMs())
	}

}
