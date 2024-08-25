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

package org.quantumbadger.redreader.compose.activity

import android.os.Bundle
import androidx.activity.enableEdgeToEdge
import androidx.compose.runtime.Composable
import androidx.compose.ui.platform.ComposeView
import org.quantumbadger.redreader.activities.BaseActivity
import org.quantumbadger.redreader.common.PrefsUtility
import org.quantumbadger.redreader.compose.ctx.RRComposeContext

open class ComposeBaseActivity: BaseActivity() {
	override fun onCreate(savedInstanceState: Bundle?) {
		enableEdgeToEdge()
		PrefsUtility.applyTheme(this)
		super.onCreate(savedInstanceState)
	}

	protected fun setContentCompose(content: @Composable () -> Unit) {
		setContentView(ComposeView(this).also { view ->
			view.setContent {
				RRComposeContext(this) {
					content()
				}
			}
		})
	}
}
