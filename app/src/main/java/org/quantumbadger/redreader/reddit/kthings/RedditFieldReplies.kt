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
import kotlinx.serialization.ExperimentalSerializationApi
import kotlinx.serialization.KSerializer
import kotlinx.serialization.Serializable
import kotlinx.serialization.descriptors.PrimitiveKind
import kotlinx.serialization.descriptors.PrimitiveSerialDescriptor
import kotlinx.serialization.descriptors.SerialDescriptor
import kotlinx.serialization.encoding.Decoder
import kotlinx.serialization.encoding.Encoder
import kotlinx.serialization.json.*
import java.io.IOException

@Serializable(with = RedditFieldRepliesSerializer::class)
@Parcelize
sealed class RedditFieldReplies : Parcelable {

	@Serializable(with = RedditFieldRepliesNoneStringSerializer::class)
	@Parcelize
	object None : RedditFieldReplies()

	@Serializable(with = RedditFieldRepliesSomeSerializer::class)
	@Parcelize
	data class Some(val value: RedditThing) : RedditFieldReplies()
}

object RedditFieldRepliesSerializer : JsonContentPolymorphicSerializer<RedditFieldReplies>(
	RedditFieldReplies::class
) {
	override fun selectDeserializer(element: JsonElement): KSerializer<out RedditFieldReplies> {

		return if (element is JsonObject) {
			RedditFieldRepliesSomeSerializer
		} else if (element is JsonPrimitive) {
			RedditFieldRepliesNoneStringSerializer
		} else if (element is JsonNull) {
			RedditFieldRepliesNoneNullSerializer
		} else {
			throw IOException("Unexpected replies type $element");
		}
	}
}

object RedditFieldRepliesNoneStringSerializer : KSerializer<RedditFieldReplies.None> {
	override val descriptor: SerialDescriptor
		get() = PrimitiveSerialDescriptor("RedditFieldReplies.None", PrimitiveKind.BOOLEAN)

	override fun deserialize(decoder: Decoder): RedditFieldReplies.None {
		decoder.decodeString()
		return RedditFieldReplies.None
	}

	override fun serialize(encoder: Encoder, value: RedditFieldReplies.None) {
		encoder.encodeString("")
	}
}

object RedditFieldRepliesNoneNullSerializer : KSerializer<RedditFieldReplies.None> {
	override val descriptor: SerialDescriptor
		get() = PrimitiveSerialDescriptor("RedditFieldReplies.None", PrimitiveKind.BOOLEAN)

	@OptIn(ExperimentalSerializationApi::class)
	override fun deserialize(decoder: Decoder): RedditFieldReplies.None {
		decoder.decodeNull()
		return RedditFieldReplies.None
	}

	override fun serialize(encoder: Encoder, value: RedditFieldReplies.None) {
		encoder.encodeString("")
	}
}

object RedditFieldRepliesSomeSerializer : KSerializer<RedditFieldReplies.Some> {
	override val descriptor: SerialDescriptor
		get() = PrimitiveSerialDescriptor("RedditFieldReplies.Some", PrimitiveKind.BOOLEAN)

	override fun deserialize(decoder: Decoder) = RedditFieldReplies.Some(
		decoder.decodeSerializableValue(RedditThing.serializer())
	)

	override fun serialize(encoder: Encoder, value: RedditFieldReplies.Some) {
		encoder.encodeSerializableValue(RedditThing.serializer(), value.value)
	}
}
