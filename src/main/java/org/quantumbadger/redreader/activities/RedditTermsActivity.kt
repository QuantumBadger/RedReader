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

package org.quantumbadger.redreader.activities

import android.content.Intent
import android.graphics.Color
import android.os.Build
import android.os.Bundle
import androidx.annotation.IdRes
import androidx.appcompat.app.AppCompatActivity
import com.google.android.material.button.MaterialButton
import org.quantumbadger.redreader.R
import org.quantumbadger.redreader.common.LinkHandler
import org.quantumbadger.redreader.common.PrefsUtility

class RedditTermsActivity : BaseActivity() {

	companion object {

		private const val extraLaunchMain = "launch_main"

		@JvmStatic
		fun launch(activity: AppCompatActivity, launchMainOnClose: Boolean) {
			val intent = Intent(activity, RedditTermsActivity::class.java)
			intent.putExtra(extraLaunchMain, launchMainOnClose)
			activity.startActivity(intent)
		}
	}

	override fun baseActivityIsActionBarBackEnabled() = false

	override fun onCreate(savedInstanceState: Bundle?) {

		PrefsUtility.applySettingsTheme(this)

		super.onCreate(savedInstanceState)

		if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP) {
			window.navigationBarColor = Color.rgb(0x55, 0x55, 0x55)
		}

		setBaseActivityListing(R.layout.reddit_terms_activity)

		val launchMainOnClose = intent.getBooleanExtra(extraLaunchMain, false)

		fun onClick(@IdRes id: Int, action: () -> Unit) {
			findViewById<MaterialButton>(id).setOnClickListener { action() }
		}

		fun onDone() {
			if (launchMainOnClose) {
				startActivity(Intent(this, MainActivity::class.java))
			}
			finish()
		}

		onClick(R.id.terms_button_view) {
			LinkHandler.onLinkClicked(
				this,
				"https://www.redditinc.com/policies/user-agreement-april-18-2023"
			)
		}

		onClick(R.id.terms_button_decline) {
			PrefsUtility.declineRedditUserAgreement()
			onDone()
		}

		onClick(R.id.terms_button_accept) {
			PrefsUtility.acceptRedditUserAgreement()
			onDone()
		}
	}
}
