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

import android.util.Base64

object ConfigProviders {

	fun interface ConfigProvider {
		fun config(): String
	}

	private val providers: MutableList<ConfigProvider> = mutableListOf()

	@JvmStatic
	fun register(provider: ConfigProvider) = providers.add(provider)

	fun apply(action: (ConfigProvider) -> Unit) = providers.forEach(action)

	fun read(action: (ByteArray) -> Unit) = apply {
		action(Base64.decode(it.config(), 0))
	}
}
