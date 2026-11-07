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

import android.graphics.Color
import android.os.Bundle
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import androidx.appcompat.app.AppCompatDialogFragment
import androidx.compose.runtime.Composable
import androidx.compose.ui.platform.ComposeView
import androidx.core.graphics.drawable.toDrawable
import org.quantumbadger.redreader.activities.BaseActivity
import org.quantumbadger.redreader.compose.ctx.RRComposeContext

/**
 * A dialog whose contents are rendered with Compose, for use from any
 * [BaseActivity]. The window background is made transparent, so [DialogContent]
 * is responsible for drawing its own themed surface.
 */
abstract class ComposeDialogFragment : AppCompatDialogFragment() {

	protected lateinit var baseActivity: BaseActivity
		private set

	@Composable
	protected abstract fun DialogContent()

	override fun onCreateView(
		inflater: LayoutInflater,
		container: ViewGroup?,
		savedInstanceState: Bundle?
	): View {
		baseActivity = requireActivity() as BaseActivity

		return ComposeView(baseActivity).apply {
			setContent {
				RRComposeContext(baseActivity) {
					DialogContent()
				}
			}
		}
	}

	override fun onStart() {
		super.onStart()

		dialog?.window?.apply {
			setLayout(
				ViewGroup.LayoutParams.MATCH_PARENT,
				ViewGroup.LayoutParams.WRAP_CONTENT
			)
			setBackgroundDrawable(Color.TRANSPARENT.toDrawable())
		}
	}
}
