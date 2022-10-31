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

package org.saiditnet.redreader.fragments;

import android.app.AlertDialog;
import android.app.Dialog;
import android.app.ProgressDialog;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.support.annotation.NonNull;
import android.support.v7.app.AppCompatActivity;
import android.support.v7.app.AppCompatDialogFragment;
import android.support.v7.widget.LinearLayoutManager;
import android.support.v7.widget.RecyclerView;
import android.view.KeyEvent;
import org.saiditnet.redreader.R;
import org.saiditnet.redreader.account.RedditAccount;
import org.saiditnet.redreader.account.RedditAccountChangeListener;
import org.saiditnet.redreader.account.RedditAccountManager;
import org.saiditnet.redreader.adapters.AccountListAdapter;
import org.saiditnet.redreader.common.AndroidCommon;
import org.saiditnet.redreader.common.General;
import org.saiditnet.redreader.common.RRError;
import org.saiditnet.redreader.reddit.api.RedditOAuth;

import java.util.concurrent.atomic.AtomicBoolean;

public class AccountListDialog extends AppCompatDialogFragment
	implements RedditAccountChangeListener {

	private AppCompatActivity mActivity;

	// Workaround for HoloEverywhere bug?
	private volatile boolean alreadyCreated = false;

	private RecyclerView rv;

	@Override
	public void onActivityResult(final int requestCode, final int resultCode, final Intent data) {
		if (requestCode == 123 && requestCode == resultCode && data.hasExtra("url")) {
			final ProgressDialog progressDialog = new ProgressDialog(getActivity());
			progressDialog.setTitle(R.string.accounts_loggingin);
			progressDialog.setMessage(getString(R.string.accounts_loggingin_msg));
			progressDialog.setIndeterminate(true);
			progressDialog.setCancelable(true);
			progressDialog.setCanceledOnTouchOutside(false);

			final AtomicBoolean cancelled = new AtomicBoolean(false);

			progressDialog.setOnCancelListener(new DialogInterface.OnCancelListener() {
				@Override
				public void onCancel(final DialogInterface dialogInterface) {
					cancelled.set(true);
					General.safeDismissDialog(progressDialog);
				}
			});

			progressDialog.setOnKeyListener(new DialogInterface.OnKeyListener() {
				@Override
				public boolean onKey(final DialogInterface dialogInterface, final int keyCode, final KeyEvent keyEvent) {
					if (keyCode == KeyEvent.KEYCODE_BACK) {
						cancelled.set(true);
						General.safeDismissDialog(progressDialog);
					}
					return true;
				}
			});

			progressDialog.show();

			RedditOAuth.loginAsynchronous(
				mActivity.getApplicationContext(),
				Uri.parse(data.getStringExtra("url")),

				new RedditOAuth.LoginListener() {
					@Override
					public void onLoginSuccess(final RedditAccount account) {
						AndroidCommon.UI_THREAD_HANDLER.post(new Runnable() {
							@Override
							public void run() {
								General.safeDismissDialog(progressDialog);
								if (cancelled.get()) return;

								final AlertDialog.Builder alertBuilder = new AlertDialog.Builder(mActivity);
								alertBuilder.setNeutralButton(R.string.dialog_close, new DialogInterface.OnClickListener() {
									@Override
									public void onClick(DialogInterface dialog, int which) {
									}
								});

								final Context context = mActivity.getApplicationContext();
								alertBuilder.setTitle(context.getString(R.string.general_success));
								alertBuilder.setMessage(context.getString(R.string.message_nowloggedin));

								final AlertDialog alertDialog = alertBuilder.create();
								alertDialog.show();
							}
						});
					}

					@Override
					public void onLoginFailure(final RedditOAuth.LoginError error, final RRError details) {
						AndroidCommon.UI_THREAD_HANDLER.post(new Runnable() {
							@Override
							public void run() {
								General.safeDismissDialog(progressDialog);
								if (!cancelled.get()) General.showResultDialog(mActivity, details);
							}
						});
					}
				});
		}
	}

	@NonNull
	@Override
	public Dialog onCreateDialog(final Bundle savedInstanceState) {
		super.onCreateDialog(savedInstanceState);

		if (alreadyCreated) return getDialog();
		alreadyCreated = true;

		mActivity = (AppCompatActivity) getActivity();

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
		AndroidCommon.UI_THREAD_HANDLER.post(new Runnable() {
			@Override
			public void run() {
				rv.setAdapter(new AccountListAdapter(mActivity, AccountListDialog.this));
			}
		});
	}
}
