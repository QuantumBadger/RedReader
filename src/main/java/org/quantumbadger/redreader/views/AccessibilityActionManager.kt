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

package org.quantumbadger.redreader.views

import android.content.res.Resources
import android.view.View
import androidx.annotation.StringRes
import androidx.core.view.ViewCompat

class AccessibilityActionManager(
	private val view: View,
	private val resources: Resources
) {
	private val existingActions = ArrayList<Int>()

	fun addAction(@StringRes label: Int, action: Runnable) {
		existingActions.add(ViewCompat.addAccessibilityAction(
			view,
			resources.getString(label)
		) { _, _ ->
			action.run()
			true
		})
	}

	fun removeAllActions() {
		existingActions.forEach {
			ViewCompat.removeAccessibilityAction(view, it)
		}
		existingActions.clear()
	}
}
