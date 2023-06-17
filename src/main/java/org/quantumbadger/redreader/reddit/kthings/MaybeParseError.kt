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
import kotlinx.serialization.descriptors.SerialDescriptor
import kotlinx.serialization.encoding.Decoder
import kotlinx.serialization.encoding.Encoder

@Serializable(with = MaybeParseErrorSerializer::class)
@Parcelize // TODO test parcelize on sealed classes
sealed class MaybeParseError<E: Parcelable> private constructor() : Parcelable {

	@Parcelize
	data class Ok<E: Parcelable>(val value: E) : MaybeParseError<E>()

	@Parcelize
	data class Err<E: Parcelable>(val error: Exception) : MaybeParseError<E>()

	fun ok() = when (this) {
		is Ok -> value
		is Err -> throw error
	}
}

class MaybeParseErrorSerializer<E: Parcelable>(
	private val innerSerializer: KSerializer<E>
) : KSerializer<MaybeParseError<E>> {

	override val descriptor: SerialDescriptor
		get() = innerSerializer.descriptor

	override fun deserialize(decoder: Decoder): MaybeParseError<E> {
		return try {
			MaybeParseError.Ok(decoder.decodeSerializableValue(innerSerializer))
		} catch (e: Exception) {
			MaybeParseError.Err(e)
		}
	}

	override fun serialize(encoder: Encoder, value: MaybeParseError<E>) {

		if (!(value is MaybeParseError.Ok)) {
			throw RuntimeException("Cannot encode error type: $value")
		}

		encoder.encodeSerializableValue(innerSerializer, value.value)
	}

}
