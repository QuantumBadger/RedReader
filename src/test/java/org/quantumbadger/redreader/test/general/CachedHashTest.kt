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
package org.quantumbadger.redreader.test.general

import org.junit.Assert
import org.junit.Test
import org.quantumbadger.redreader.common.CachedStringHash
import java.util.concurrent.atomic.AtomicReference

class CachedHashTest {
    @Test
    fun testHash() {

		val data = AtomicReference("Hello")

		val hash = CachedStringHash { data.get() }

		Assert.assertEquals(
			hash.hash,
			"185F8DB32271FE25F561A6FC938B2E264306EC304EDA518007D1764826381969")

		data.set("World")

		Assert.assertEquals(
			hash.hash,
			"78AE647DC5544D227130A0682A51E30BC7777FBB6D8A8F17007463A3ECD1D524")

    }
}
