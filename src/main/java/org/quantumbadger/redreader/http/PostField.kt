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

package org.quantumbadger.redreader.http

import java.io.UnsupportedEncodingException
import java.net.URLEncoder

data class PostField(val name: String, val value: String) {

	fun encode(): String {
		try {
			return (URLEncoder.encode(name, "UTF-8")
					+ "="
					+ URLEncoder.encode(value, "UTF-8"))
		} catch (e: UnsupportedEncodingException) {
			throw RuntimeException(e)
		}
	}

	companion object {
		fun encodeList(fields: List<PostField>): String {
			val result = StringBuilder()

			for (field in fields) {
				if (result.isNotEmpty()) {
					result.append('&')
				}

				result.append(field.encode())
			}

			return result.toString()
		}
	}
}
