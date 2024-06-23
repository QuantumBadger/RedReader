package org.quantumbadger.redreader.compose.activity

import androidx.compose.runtime.Composable
import androidx.compose.ui.platform.ComposeView
import org.quantumbadger.redreader.activities.BaseActivity
import org.quantumbadger.redreader.compose.ctx.RRComposeContextTest

open class ComposeBaseActivity: BaseActivity() {

	protected fun setContentCompose(content: @Composable () -> Unit) {
		setContentView(ComposeView(this).also { view ->
			view.setContent {
				// TODO replace with non-test context
				RRComposeContextTest {
					content()
				}
			}
		})
	}
}
