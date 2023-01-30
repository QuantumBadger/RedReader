package org.quantumbadger.redreader.reddit.kthings

import kotlinx.serialization.KSerializer
import kotlinx.serialization.Serializable
import kotlinx.serialization.descriptors.SerialDescriptor
import kotlinx.serialization.encoding.Decoder
import kotlinx.serialization.encoding.Encoder
import java.lang.RuntimeException

@Serializable(with = MaybeParseErrorSerializer::class)
sealed class MaybeParseError<E> private constructor() {

	data class Ok<E>(val value: E) : MaybeParseError<E>()

	data class Err<E>(val error: Exception) : MaybeParseError<E>()
}

class MaybeParseErrorSerializer<E>(
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
