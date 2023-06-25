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
package org.quantumbadger.redreader.adapters

import android.content.Context
import android.content.DialogInterface
import android.content.Intent
import android.graphics.Color
import android.graphics.drawable.Drawable
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import androidx.annotation.StringRes
import androidx.core.content.ContextCompat
import androidx.fragment.app.Fragment
import androidx.recyclerview.widget.RecyclerView
import com.google.android.material.dialog.MaterialAlertDialogBuilder
import org.quantumbadger.redreader.R
import org.quantumbadger.redreader.account.RedditAccountManager
import org.quantumbadger.redreader.activities.OAuthLoginActivity
import org.quantumbadger.redreader.common.BetterSSB
import org.quantumbadger.redreader.reddit.api.RedditOAuth.needsRelogin
import org.quantumbadger.redreader.viewholders.VH1Text
import java.util.*

class AccountListAdapter(private val context: Context, private val fragment: Fragment) :
    HeaderRecyclerAdapter<RecyclerView.ViewHolder?>() {
    private val accounts = RedditAccountManager.getInstance(context).accounts
	private val rrIconAdd: Drawable?

    init {
		val attr = context.obtainStyledAttributes(intArrayOf(R.attr.rrIconAdd))
        rrIconAdd = ContextCompat.getDrawable(context, attr.getResourceId(0, 0))
        attr.recycle()
    }

    override fun onCreateHeaderItemViewHolder(parent: ViewGroup): RecyclerView.ViewHolder {
        val v = LayoutInflater.from(parent.context)
            .inflate(R.layout.list_item_1_text, parent, false)
        return VH1Text(v)
    }

    override fun onCreateContentItemViewHolder(parent: ViewGroup): RecyclerView.ViewHolder {
        val v = LayoutInflater.from(parent.context)
            .inflate(R.layout.list_item_1_text, parent, false)
        return VH1Text(v)
    }

	override fun onBindHeaderItemViewHolder(
        holder: RecyclerView.ViewHolder?,
        position: Int
    ) {
        val vh = holder as VH1Text
        vh.text.text = context.getString(R.string.accounts_add)
        vh.text.setCompoundDrawablesWithIntrinsicBounds(rrIconAdd, null, null, null)
        holder.itemView.setOnClickListener { v: View? -> showLoginWarningDialog() }
    }

    private fun showLoginWarningDialog() {
        MaterialAlertDialogBuilder(context)
            .setMessage(
                String.format(
                    Locale.US,
                    "%s\n\n%s",
                    context.getString(
                        R.string.reddit_login_browser_popup_line_1
                    ),
                    context.getString(
                        R.string.reddit_login_browser_popup_line_2_internal_browser_only
                    )
                )
            )
            .setCancelable(true)
            .setPositiveButton(
                R.string.dialog_continue
            ) { dialog: DialogInterface?, which: Int -> launchLogin() }
            .setNegativeButton(
                R.string.dialog_close
            ) { dialog: DialogInterface?, which: Int -> }
            .show()
    }

    private fun launchLogin() {
        val loginIntent = Intent(context, OAuthLoginActivity::class.java)
        fragment.startActivityForResult(loginIntent, 123)
    }

    override fun onBindContentItemViewHolder(
        holder: RecyclerView.ViewHolder?,
        position: Int
    ) {
		val accountManager = RedditAccountManager.getInstance(context)

		val vh = holder as VH1Text
        val account = accounts[position]
        val username = BetterSSB()
        if (account.isAnonymous) {
            username.append(context.getString(R.string.accounts_anon), 0)
        } else {
            username.append(account.username, 0)
        }
        if (account == accountManager.defaultAccount) {
            val attr = context.obtainStyledAttributes(intArrayOf(R.attr.rrListSubtitleCol))
            val col = attr.getColor(0, 0)
            attr.recycle()
            username.append(
                "  (" + context.getString(R.string.accounts_active) + ")",
                BetterSSB.FOREGROUND_COLOR or BetterSSB.SIZE,
                col,
                0,
                0.8f
            )
        }
        if (needsRelogin(account)) {
            username.append(
                "  (" + context.getString(R.string.reddit_relogin_error_title) + ")",
                BetterSSB.FOREGROUND_COLOR or BetterSSB.SIZE,
                Color.rgb(200, 50, 50),
                0,
                0.8f
            )
        }
        vh.text.text = username.get()
        vh.itemView.setOnClickListener {

			val actions = ArrayList<AccountAction>()

			if(account != accountManager.defaultAccount) {
				actions.add(AccountAction(R.string.accounts_setactive) {
					accountManager.defaultAccount = account
				})
			}

			if(account.isNotAnonymous) {
				actions.add(AccountAction(R.string.accounts_delete) {
					MaterialAlertDialogBuilder(context)
						.setTitle(R.string.accounts_delete)
						.setMessage(R.string.accounts_delete_sure)
						.setPositiveButton(
							R.string.accounts_delete
						) { _, _ -> accountManager.deleteAccount(account) }
						.setNegativeButton(R.string.dialog_cancel, null)
						.show()
				})
			}

			if (needsRelogin(account)) {
				actions.add(AccountAction(R.string.accounts_reauth) {
					showLoginWarningDialog()
				})
			}

			val items = actions.map { context.getString(it.message) }.toTypedArray()

			if (items.isNotEmpty()) {
				val builder = MaterialAlertDialogBuilder(context)
				builder.setItems(items) { dialog, which ->
					actions[which].action()
				}

				builder.setNeutralButton(R.string.dialog_cancel, null)
				val alert = builder.create()
				alert.setTitle(if (account.isAnonymous) context.getString(R.string.accounts_anon) else account.username)
				alert.setCanceledOnTouchOutside(true)
				alert.show()
			}
        }
    }

    override fun getContentItemCount(): Int {
        return accounts.size
    }

	private class AccountAction(
		@StringRes val message: Int,
		val action: () -> Unit
	)
}
