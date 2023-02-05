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
import org.apache.commons.text.StringEscapeUtils

@Serializable(with = UrlEncodedStringSerializer::class)
@Parcelize
data class UrlEncodedString(
	val decoded: String
) : Parcelable

object UrlEncodedStringSerializer : KSerializer<UrlEncodedString> {
	override val descriptor: SerialDescriptor
		get() = PrimitiveSerialDescriptor("UrlEncodedString", PrimitiveKind.STRING)

	override fun deserialize(decoder: Decoder) =
		UrlEncodedString(decoded = StringEscapeUtils.unescapeHtml4(decoder.decodeString()))

	override fun serialize(encoder: Encoder, value: UrlEncodedString) {
		encoder.encodeString(StringEscapeUtils.escapeHtml4(value.decoded))
	}
}
