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

package org.quantumbadger.redreader.activities;

import android.accounts.Account;
import android.accounts.AccountAuthenticatorActivity;
import android.accounts.AccountManager;
import android.content.DialogInterface;
import android.content.Intent;
import android.os.Bundle;
import android.os.Handler;
import android.os.Looper;
import android.view.KeyEvent;
import android.view.View;
import android.widget.EditText;
import org.apache.http.impl.client.DefaultHttpClient;
import org.holoeverywhere.app.AlertDialog;
import org.holoeverywhere.app.ProgressDialog;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.account.RedditAccountAuthenticator;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.fragments.AccountListDialog;

import java.util.concurrent.atomic.AtomicBoolean;

public class RedditAuthenticatorActivity extends AccountAuthenticatorActivity{

    public final static String ARG_ACCOUNT_TYPE = "ACCOUNT_TYPE";
    public final static String ARG_AUTH_TYPE = "AUTH_TYPE";
    public final static String ARG_ACCOUNT_NAME = "ACCOUNT_NAME";

    public final static String ARG_CREATE_ACCOUNT = "CREATE_ACCOUNT";

    private static String lastUsername = "";

    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.account_login);

        String accountName = getIntent().getStringExtra(ARG_ACCOUNT_NAME);
        String mAuthTokenType = getIntent().getStringExtra(ARG_AUTH_TYPE);

        if (mAuthTokenType == null)
            mAuthTokenType = RedditAccountAuthenticator.TOKENTYPE_MODHASH;

        if (accountName != null) {
            ((EditText)findViewById(R.id.login_username)).setText(accountName);
        }
        else
            ((EditText)findViewById(R.id.login_username)).setText(lastUsername);

        findViewById(R.id.login_ok).setOnClickListener(new View.OnClickListener() {
            public void onClick(View v) {
                submit();
            }
        });

        findViewById(R.id.login_cancel).setOnClickListener(new View.OnClickListener() {
            public void onClick(View v) {
                setResult(RESULT_CANCELED);
                finish();
            }
        });
    }

    private void submit() {
        final String username = ((EditText) findViewById(R.id.login_username)).getText().toString().trim();
        final String password = ((EditText) findViewById(R.id.login_password)).getText().toString();

        lastUsername = username;

        final ProgressDialog progressDialog = new ProgressDialog(this);
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
                final RedditAccount.LoginResultPair result = RedditAccount.login(getApplicationContext(), username, password, new DefaultHttpClient());

                new Handler(Looper.getMainLooper()).post(new Runnable() {
                    public void run() {

                        if(cancelled.get()) return; // safe, since we're in the UI thread

                        progressDialog.dismiss();

                        final AlertDialog.Builder alertBuilder = new AlertDialog.Builder(getApplicationContext());
                        alertBuilder.setNeutralButton(R.string.dialog_close, new DialogInterface.OnClickListener() {
                            public void onClick(DialogInterface dialog, int which) {
                                new AccountListDialog().show();
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
                                RedditAccountManager.getInstance(getApplicationContext()).addAccount(result.account);
                                RedditAccountManager.getInstance(getApplicationContext()).setDefaultAccount(result.account);
                                alertBuilder.setTitle(R.string.general_success);
                                alertBuilder.setMessage(R.string.message_nowloggedin);
                                lastUsername = "";
                                break;
                            default:
                                throw new RuntimeException();
                        }
                    }
                });

                finishLogin(result);
            }
        };

        thread.start();
    }

    private void finishLogin(final RedditAccount.LoginResultPair result) {

        if (getIntent().getBooleanExtra(ARG_CREATE_ACCOUNT, false)) {
            RedditAccountManager.getInstance(this).addAccount(result.account);
        }

        Bundle data = new Bundle();
        data.putString(AccountManager.KEY_ACCOUNT_NAME, result.account.username);
        data.putString(AccountManager.KEY_ACCOUNT_TYPE, getIntent().getStringExtra(ARG_ACCOUNT_TYPE));
        data.putString(AccountManager.KEY_AUTHTOKEN, result.account.modhash);

        setAccountAuthenticatorResult(data);
        setResult(RESULT_OK, new Intent().putExtras(data));
        finish();
    }
}
