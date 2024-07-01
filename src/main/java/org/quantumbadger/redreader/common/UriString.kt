package org.quantumbadger.redreader.common

import android.net.Uri
import android.os.Parcelable
import androidx.compose.runtime.Immutable
import kotlinx.parcelize.Parcelize

@Immutable
@Parcelize
data class UriString(
	@JvmField
	val value: String
) : Parcelable {
	override fun toString() = value

	fun toUri() = Uri.parse(value)

	companion object {
		@JvmStatic
		fun fromNullable(value: String?) = value?.let { UriString(it) }

		@JvmStatic
		fun from(value: Uri) = UriString(value.toString())

		@JvmStatic
		fun from(value: Uri.Builder) = UriString(value.toString())
	}
}
