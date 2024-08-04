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

import android.os.Bundle
import android.util.Log
import org.quantumbadger.redreader.R
import org.quantumbadger.redreader.common.General
import org.quantumbadger.redreader.common.General.isSensitiveDebugLoggingEnabled
import org.quantumbadger.redreader.common.PrefsUtility
import org.quantumbadger.redreader.common.UriString
import org.quantumbadger.redreader.compose.activity.ComposeBaseActivity
import org.quantumbadger.redreader.compose.ui.AlbumScreen

class AlbumListingActivity : ComposeBaseActivity() {

	override fun onCreate(savedInstanceState: Bundle?) {

		// Keep this here for dialogs
		PrefsUtility.applyTheme(this)

		super.onCreate(savedInstanceState)

		setTitle(R.string.image_gallery)

		val url = UriString.fromNullable(intent.dataString)

		if (url == null) {
			finish()
			return
		}

		if (isSensitiveDebugLoggingEnabled) {
			Log.i("AlbumListingActivity", "Loading URL $url")
		}

		setContentCompose {
			AlbumScreen(
				onBackPressed = this::onBackPressed,
				albumUrl = url
			)
		}
	}

	override fun onBackPressed() {
		if (General.onBackPressed()) {
			super.onBackPressed()
		}
	}
}
