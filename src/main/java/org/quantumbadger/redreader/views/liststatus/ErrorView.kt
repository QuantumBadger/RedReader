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
package org.quantumbadger.redreader.views.liststatus

import android.graphics.Color
import android.view.LayoutInflater
import androidx.appcompat.app.AppCompatActivity
import androidx.appcompat.widget.AppCompatButton
import com.google.android.material.textview.MaterialTextView
import org.quantumbadger.redreader.R
import org.quantumbadger.redreader.activities.RedditTermsActivity
import org.quantumbadger.redreader.common.RRError
import org.quantumbadger.redreader.fragments.AccountListDialog
import org.quantumbadger.redreader.fragments.ErrorPropertiesDialog

class ErrorView(activity: AppCompatActivity, error: RRError) : StatusListItemView(activity) {

	init {
		val view = LayoutInflater.from(activity).inflate(
			R.layout.error_view,
			null
		)

		val title = view.findViewById<MaterialTextView>(R.id.error_text_title)
		val message = view.findViewById<MaterialTextView>(R.id.error_text_message)

		val resolveButton = view.findViewById<AppCompatButton>(R.id.error_button_resolve)
		val detailsButton = view.findViewById<AppCompatButton>(R.id.error_button_details)

		title.text = error.title
		message.text = error.message

		error.resolution?.apply {
			when (this) {
				RRError.Resolution.ACCEPT_REDDIT_TERMS -> {
					resolveButton.setText(R.string.reddit_terms_error_resolution_button)
					resolveButton.setOnClickListener {
						RedditTermsActivity.launch(activity, false)
					}
				}
				RRError.Resolution.ACCOUNTS_LIST -> {
					resolveButton.setText(R.string.options_accounts)
					resolveButton.setOnClickListener {
						AccountListDialog.show(activity)
					}
				}
			}

		} ?: resolveButton.setVisibility(GONE)

		detailsButton.setOnClickListener {
			ErrorPropertiesDialog.newInstance(error)
				.show(
					activity.supportFragmentManager,
					null
				)
		}


		setContents(view)
		setBackgroundColor(Color.rgb(0xCC, 0x00, 0x00))
	}
}
