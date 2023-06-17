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
