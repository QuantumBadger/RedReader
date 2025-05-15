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

package org.quantumbadger.redreader.http.body

import org.quantumbadger.redreader.common.General.listFromArray
import org.quantumbadger.redreader.http.PostField
import org.quantumbadger.redreader.http.body.multipart.Part

sealed interface HTTPRequestBody {

	class Multipart : HTTPRequestBody {
		private val parts: ArrayList<Part> = ArrayList()

		fun addPart(part: Part): Multipart {
			parts.add(part)
			return this
		}

		fun forEachPart(consumer: (Part) -> Unit) {
			parts.forEach(consumer)
		}
	}

	class PostFields : HTTPRequestBody {
		private val postFields: ArrayList<PostField>

		constructor() : this(emptyList())

		constructor(vararg fields: PostField) : this(listFromArray<PostField>(*fields))

		constructor(postFields: Collection<PostField>) {
			this.postFields = ArrayList(postFields)
		}

		fun addField(field: PostField): PostFields {
			postFields.add(field)
			return this
		}

		fun encodeFields() = PostField.encodeList(postFields)
	}
}
