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

package org.quantumbadger.redreader.fragments;

import android.content.Context;
import android.content.DialogInterface;
import android.os.Bundle;
import android.os.Handler;
import android.os.Looper;
import android.view.View;
import android.widget.AdapterView;
import org.holoeverywhere.app.AlertDialog;
import org.holoeverywhere.app.Dialog;
import org.holoeverywhere.app.DialogFragment;
import org.holoeverywhere.widget.ListView;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.account.RedditAccountChangeListener;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.adapters.AccountListAdapter;

public class AccountListDialog extends DialogFragment implements RedditAccountChangeListener {

	// Workaround for HoloEverywhere bug?
	private volatile boolean alreadyCreated = false;

	private ListView lv;

	@Override
	public Dialog onCreateDialog(final Bundle savedInstanceState) {

		super.onCreateDialog(savedInstanceState);

		if(alreadyCreated) return getDialog();
		alreadyCreated = true;

		final Context context = getSupportActivity();

		final AlertDialog.Builder builder = new AlertDialog.Builder(getActivity());
		builder.setTitle(context.getString(R.string.options_accounts_long));

		lv = new ListView(context);
		builder.setView(lv);

		lv.setAdapter(new AccountListAdapter(context));

		RedditAccountManager.getInstance(context).addUpdateListener(this);

		lv.setOnItemClickListener(new AdapterView.OnItemClickListener() {
			public void onItemClick(AdapterView<?> adapterView, View view, int position, final long id) {

				if(position == 0) {
					new AddAccountDialog().show(getSupportActivity());
					dismiss();
				} else {

					final RedditAccount account = (RedditAccount)lv.getAdapter().getItem(position);

					final String[] items = account.isAnonymous()
							? new String[] {getString(R.string.accounts_setactive)}
							: new String[] {
								getString(R.string.accounts_setactive),
								getString(R.string.accounts_delete)
							};

					final AlertDialog.Builder builder = new AlertDialog.Builder(context);

					builder.setItems(items, new DialogInterface.OnClickListener() {
						public void onClick(DialogInterface dialog, int which) {

							final String selected = items[which];

							if(selected.equals(getString(R.string.accounts_setactive))) {
								RedditAccountManager.getInstance(context).setDefaultAccount(account);

							} else if(selected.equals(getString(R.string.accounts_delete))) {
								new AlertDialog.Builder(context)
										.setTitle(R.string.accounts_delete)
										.setMessage(R.string.accounts_delete_sure)
										.setPositiveButton(R.string.accounts_delete,
												new DialogInterface.OnClickListener() {
													public void onClick(final DialogInterface dialog, final int which) {
														RedditAccountManager.getInstance(context).deleteAccount(account);
													}
												})
										.setNegativeButton(R.string.dialog_cancel, null)
										.show();

							}
						}
					});

					builder.setNeutralButton(R.string.dialog_cancel, null);

					final AlertDialog alert = builder.create();
					alert.setTitle(account.isAnonymous() ? getString(R.string.accounts_anon) : account.username);
					alert.setCanceledOnTouchOutside(true);
					alert.show();

				}
			}
		});

		builder.setNeutralButton(getSupportActivity().getString(R.string.dialog_close), null);

		return builder.create();
	}

	public void onRedditAccountChanged() {
		new Handler(Looper.getMainLooper()).post(new Runnable() {
			public void run() {
				lv.setAdapter(new AccountListAdapter(getSupportActivity()));
			}
		});
	}
}
