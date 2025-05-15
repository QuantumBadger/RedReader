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

import kotlinx.serialization.KSerializer
import kotlinx.serialization.Serializable
import kotlinx.serialization.builtins.ListSerializer
import kotlinx.serialization.descriptors.SerialDescriptor
import kotlinx.serialization.encoding.Decoder
import kotlinx.serialization.encoding.Encoder
import kotlinx.serialization.json.JsonArray
import kotlinx.serialization.json.JsonContentPolymorphicSerializer
import kotlinx.serialization.json.JsonElement

@Serializable(with = RedditThingResponseSerializer::class)
sealed class RedditThingResponse {

	@Serializable(with = RedditThingResponseSingleSerializer::class)
	data class Single(val thing: RedditThing) : RedditThingResponse()

	@Serializable(with = RedditThingResponseMultipleSerializer::class)
	data class Multiple(val things: List<RedditThing>) : RedditThingResponse()
}

object RedditThingResponseSerializer : JsonContentPolymorphicSerializer<RedditThingResponse>(
	RedditThingResponse::class
) {
	override fun selectDeserializer(element: JsonElement): KSerializer<out RedditThingResponse> {

		return if (element is JsonArray) {
			RedditThingResponseMultipleSerializer
		} else {
			RedditThingResponseSingleSerializer
		}
	}
}

object RedditThingResponseSingleSerializer : KSerializer<RedditThingResponse.Single> {
	override val descriptor: SerialDescriptor
		get() = RedditThing.serializer().descriptor

	override fun deserialize(decoder: Decoder) =
		RedditThingResponse.Single(decoder.decodeSerializableValue(RedditThing.serializer()))

	override fun serialize(encoder: Encoder, value: RedditThingResponse.Single) {
		encoder.encodeSerializableValue(RedditThing.serializer(), value.thing)
	}
}

object RedditThingResponseMultipleSerializer : KSerializer<RedditThingResponse.Multiple> {
	override val descriptor: SerialDescriptor
		get() = RedditThing.serializer().descriptor

	private val multipleThingSerializer = ListSerializer(RedditThing.serializer())

	override fun deserialize(decoder: Decoder) =
		RedditThingResponse.Multiple(decoder.decodeSerializableValue(multipleThingSerializer))

	override fun serialize(encoder: Encoder, value: RedditThingResponse.Multiple) {
		encoder.encodeSerializableValue(multipleThingSerializer, value.things)
	}
}
