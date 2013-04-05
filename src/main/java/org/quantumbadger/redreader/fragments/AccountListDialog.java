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
import org.quantumbadger.redreader.account.RedditAccountChangeListener;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.adapters.AccountListAdapter;

public class AccountListDialog extends DialogFragment {

	// Workaround for HoloEverywhere bug?
	private volatile boolean alreadyCreated = false;

	@Override
	public Dialog onCreateDialog(final Bundle savedInstanceState) {

		super.onCreateDialog(savedInstanceState);

		if(alreadyCreated) return getDialog();
		alreadyCreated = true;

		final Context context = getSupportActivity();

		final AlertDialog.Builder builder = new AlertDialog.Builder(getActivity());
		builder.setTitle(context.getString(R.string.options_accounts));

		final ListView lv = new ListView(context);
		builder.setView(lv);

		lv.setAdapter(new AccountListAdapter(context));

		RedditAccountManager.getInstance(context).addUpdateListener(new RedditAccountChangeListener() {
			public void onRedditAccountChanged() {
				new Handler(Looper.getMainLooper()).post(new Runnable() {
					public void run() {
						lv.setAdapter(new AccountListAdapter(context));
					}
				});
			}
		});

		lv.setOnItemClickListener(new AdapterView.OnItemClickListener() {
			public void onItemClick(AdapterView<?> adapterView, View view, int position, final long id) {

				if(position == 0) {
					new AddAccountDialog().show(getSupportActivity());
					dismiss();
				} else {
					view.showContextMenu();
				}
			}
		});

		builder.setNeutralButton(getSupportActivity().getString(R.string.dialog_close), null);

		return builder.create();
	}
}
