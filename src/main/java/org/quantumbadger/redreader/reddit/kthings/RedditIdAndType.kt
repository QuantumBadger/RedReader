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

@Serializable(with = RedditIdAndTypeSerializer::class)
@Parcelize
data class RedditIdAndType(
	val value: String
) : Parcelable {
	override fun toString() = value
}

object RedditIdAndTypeSerializer : KSerializer<RedditIdAndType> {
	override val descriptor: SerialDescriptor
		get() = PrimitiveSerialDescriptor("RedditIdAndType", PrimitiveKind.STRING)

	override fun deserialize(decoder: Decoder) = RedditIdAndType(decoder.decodeString())

	override fun serialize(encoder: Encoder, value: RedditIdAndType) {
		encoder.encodeString(value.value)
	}
}
