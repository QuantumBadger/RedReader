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
