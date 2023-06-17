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
