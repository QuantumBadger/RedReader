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

import android.content.DialogInterface;
import android.os.Bundle;
import android.os.Handler;
import android.os.Looper;
import android.view.KeyEvent;
import android.view.View;
import org.apache.http.impl.client.DefaultHttpClient;
import org.holoeverywhere.app.AlertDialog;
import org.holoeverywhere.app.Dialog;
import org.holoeverywhere.app.DialogFragment;
import org.holoeverywhere.app.ProgressDialog;
import org.holoeverywhere.widget.EditText;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.account.RedditAccountManager;

import java.util.concurrent.atomic.AtomicBoolean;

public class AddAccountDialog extends DialogFragment {

	private static String lastUsername = "";

	// Workaround for HoloEverywhere bug?
	private volatile boolean alreadyCreated = false;

	@Override
	public Dialog onCreateDialog(final Bundle savedInstanceState) {

		if(alreadyCreated) return getDialog();
		alreadyCreated = true;

		super.onCreateDialog(savedInstanceState);

		final AlertDialog.Builder builder = new AlertDialog.Builder(getSupportActivity());
		builder.setTitle(R.string.accounts_add);

		final View view = getSupportActivity().getLayoutInflater().inflate(R.layout.dialog_login, null);
		builder.setView(view);
		builder.setCancelable(true);

		final EditText usernameBox = ((EditText)view.findViewById(R.id.login_username));
		usernameBox.setText(lastUsername);
		usernameBox.requestFocus();
		usernameBox.requestFocusFromTouch();

		builder.setPositiveButton(R.string.accounts_login, new DialogInterface.OnClickListener() {
			public void onClick(final DialogInterface dialogInterface, final int i) {

				final String username = ((EditText) getDialog().findViewById(R.id.login_username)).getText().toString().trim();
				final String password = ((EditText) getDialog().findViewById(R.id.login_password)).getText().toString();

				lastUsername = username;

				final ProgressDialog progressDialog = new ProgressDialog(getSupportActivity());
				final Thread thread;
				progressDialog.setTitle(R.string.accounts_loggingin);
				progressDialog.setMessage(getString(R.string.accounts_loggingin_msg));
				progressDialog.setIndeterminate(true);

				final AtomicBoolean cancelled = new AtomicBoolean(false);

				progressDialog.setCancelable(true);
				progressDialog.setCanceledOnTouchOutside(false);

				progressDialog.show();

				progressDialog.setOnCancelListener(new DialogInterface.OnCancelListener() {
					public void onCancel(final DialogInterface dialogInterface) {
						cancelled.set(true);
						progressDialog.dismiss();
					}
				});

				progressDialog.setOnKeyListener(new DialogInterface.OnKeyListener() {
					public boolean onKey(final DialogInterface dialogInterface, final int keyCode, final KeyEvent keyEvent) {

						if(keyCode == KeyEvent.KEYCODE_BACK) {
							cancelled.set(true);
							progressDialog.dismiss();
						}

						return true;
					}
				});

				thread = new Thread() {
					@Override
					public void run() {

						// TODO better HTTP client
						final RedditAccount.LoginResultPair result = RedditAccount.login(getSupportActivity(), username, password, new DefaultHttpClient());

						new Handler(Looper.getMainLooper()).post(new Runnable() {
							public void run() {

								if(cancelled.get()) return; // safe, since we're in the UI thread

								progressDialog.dismiss();

								final AlertDialog.Builder alertBuilder = new AlertDialog.Builder(getSupportActivity());
								alertBuilder.setNeutralButton(R.string.dialog_close, new DialogInterface.OnClickListener() {
									public void onClick(DialogInterface dialog, int which) {
										new AccountListDialog().show(getSupportActivity());
									}
								});

								// TODO handle errors better
								switch(result.result) {
									case CONNECTION_ERROR:
										alertBuilder.setTitle(R.string.error_connection_title);
										alertBuilder.setMessage(R.string.message_cannotlogin);
										break;
									case INTERNAL_ERROR:
										alertBuilder.setTitle(R.string.error_unknown_title);
										alertBuilder.setMessage(R.string.message_cannotlogin);
										break;
									case JSON_ERROR:
										alertBuilder.setTitle(R.string.error_parse_title);
										alertBuilder.setMessage(R.string.message_cannotlogin);
										break;
									case REQUEST_ERROR:
										alertBuilder.setTitle(R.string.error_connection_title);
										alertBuilder.setMessage(R.string.message_cannotlogin);
										break;
									case UNKNOWN_REDDIT_ERROR:
										alertBuilder.setTitle(R.string.error_unknown_title);
										alertBuilder.setMessage(R.string.message_cannotlogin);
										break;
									case WRONG_PASSWORD:
										alertBuilder.setTitle(R.string.error_invalid_password_title);
										alertBuilder.setMessage(R.string.error_invalid_password_message);
										break;
									case RATELIMIT:
										alertBuilder.setTitle(R.string.error_ratelimit_title);
										alertBuilder.setMessage(String.format("%s \"%s\"", getString(R.string.error_ratelimit_message), result.extraMessage));
										break;
									case SUCCESS:
										RedditAccountManager.getInstance(getSupportActivity()).addAccount(result.account);
										RedditAccountManager.getInstance(getSupportActivity()).setDefaultAccount(result.account);
										alertBuilder.setTitle(R.string.general_success);
										alertBuilder.setMessage(R.string.message_nowloggedin);
										lastUsername = "";
										break;
									default:
										throw new RuntimeException();
								}

								final AlertDialog alertDialog = alertBuilder.create();
								alertDialog.show();
							}
						});
					}
				};

				thread.start();
			}
		});

		builder.setNegativeButton(R.string.dialog_cancel, null);

		return builder.create();
	}
}
