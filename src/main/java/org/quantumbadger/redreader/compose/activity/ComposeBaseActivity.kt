package org.quantumbadger.redreader.compose.activity

import android.os.Bundle
import androidx.activity.enableEdgeToEdge
import androidx.compose.runtime.Composable
import androidx.compose.ui.platform.ComposeView
import org.quantumbadger.redreader.activities.BaseActivity
import org.quantumbadger.redreader.compose.ctx.RRComposeContextTest

open class ComposeBaseActivity: BaseActivity() {
	override fun onCreate(savedInstanceState: Bundle?) {
		super.onCreate(savedInstanceState)
		enableEdgeToEdge()
	}

	override fun baseActivityIsToolbarActionBarEnabled(): Boolean {
		return false;
	}

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
