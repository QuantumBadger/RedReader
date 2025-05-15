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
