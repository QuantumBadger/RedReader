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

import android.app.AlertDialog;
import android.app.Dialog;
import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import androidx.annotation.NonNull;
import androidx.appcompat.app.AppCompatActivity;
import androidx.appcompat.app.AppCompatDialogFragment;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccountChangeListener;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.adapters.AccountListAdapter;
import org.quantumbadger.redreader.common.AndroidCommon;
import org.quantumbadger.redreader.common.RunnableOnce;
import org.quantumbadger.redreader.reddit.api.RedditOAuth;

public class AccountListDialog extends AppCompatDialogFragment
		implements RedditAccountChangeListener {

	private AppCompatActivity mActivity;

	// Workaround for HoloEverywhere bug?
	private volatile boolean alreadyCreated = false;

	private RecyclerView rv;

	@Override
	public void onActivityResult(
			final int requestCode,
			final int resultCode,
			final Intent data) {
		if(requestCode == 123 && requestCode == resultCode && data.hasExtra("url")) {
			final Uri uri = Uri.parse(data.getStringExtra("url"));
			RedditOAuth.completeLogin(mActivity, uri, RunnableOnce.DO_NOTHING);
		}
	}

	@NonNull
	@Override
	public Dialog onCreateDialog(final Bundle savedInstanceState) {
		super.onCreateDialog(savedInstanceState);

		if(alreadyCreated) {
			return getDialog();
		}
		alreadyCreated = true;

		mActivity = (AppCompatActivity)getActivity();

		final AlertDialog.Builder builder = new AlertDialog.Builder(mActivity);
		builder.setTitle(mActivity.getString(R.string.options_accounts_long));

		rv = new RecyclerView(mActivity);
		builder.setView(rv);

		rv.setLayoutManager(new LinearLayoutManager(mActivity));
		rv.setAdapter(new AccountListAdapter(mActivity, this));
		rv.setHasFixedSize(true);

		RedditAccountManager.getInstance(mActivity).addUpdateListener(this);

		builder.setNeutralButton(mActivity.getString(R.string.dialog_close), null);

		return builder.create();
	}

	@Override
	public void onRedditAccountChanged() {
		AndroidCommon.UI_THREAD_HANDLER.post(() -> rv.setAdapter(
				new AccountListAdapter(mActivity, this)));
	}
}
