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

package org.saiditnet.redreader.activities;

import android.app.ProgressDialog;
import android.content.DialogInterface;
import android.content.Intent;
import android.os.Bundle;
import android.support.annotation.Nullable;
import android.view.KeyEvent;
import android.view.Menu;
import android.view.MenuItem;
import android.widget.ArrayAdapter;
import android.widget.EditText;
import android.widget.LinearLayout;
import android.widget.ScrollView;
import android.widget.Spinner;
import org.saiditnet.redreader.R;
import org.saiditnet.redreader.account.RedditAccount;
import org.saiditnet.redreader.account.RedditAccountManager;
import org.saiditnet.redreader.cache.CacheManager;
import org.saiditnet.redreader.cache.CacheRequest;
import org.saiditnet.redreader.common.AndroidCommon;
import org.saiditnet.redreader.common.General;
import org.saiditnet.redreader.common.PrefsUtility;
import org.saiditnet.redreader.common.RRError;
import org.saiditnet.redreader.fragments.MarkdownPreviewDialog;
import org.saiditnet.redreader.reddit.APIResponseHandler;
import org.saiditnet.redreader.reddit.RedditAPI;

import java.util.ArrayList;

public class PMSendActivity extends BaseActivity {

	public static final String EXTRA_RECIPIENT = "recipient";
	public static final String EXTRA_SUBJECT = "subject";

	private static final String SAVED_STATE_RECIPIENT = "recipient";
	private static final String SAVED_STATE_TEXT = "pm_text";
	private static final String SAVED_STATE_SUBJECT = "pm_subject";

	private Spinner usernameSpinner;
	private EditText recipientEdit;
	private EditText subjectEdit;
	private EditText textEdit;

	private boolean mSendSuccess;

	private static String lastText, lastRecipient, lastSubject;

	@Override
	protected void onCreate(Bundle savedInstanceState) {

		PrefsUtility.applyTheme(this);

		super.onCreate(savedInstanceState);

		setTitle(R.string.pm_send_actionbar);

		final LinearLayout layout = (LinearLayout) getLayoutInflater().inflate(R.layout.pm_send, null);

		usernameSpinner = (Spinner)layout.findViewById(R.id.pm_send_username);
		recipientEdit = (EditText)layout.findViewById(R.id.pm_send_recipient);
		subjectEdit = (EditText)layout.findViewById(R.id.pm_send_subject);
		textEdit = (EditText)layout.findViewById(R.id.pm_send_text);

		final String initialRecipient;
		final String initialSubject;
		final String initialText;

		if(savedInstanceState != null && savedInstanceState.containsKey(SAVED_STATE_TEXT)) {
			initialRecipient = savedInstanceState.getString(SAVED_STATE_RECIPIENT);
			initialSubject = savedInstanceState.getString(SAVED_STATE_SUBJECT);
			initialText = savedInstanceState.getString(SAVED_STATE_TEXT);

		} else {

			final Intent intent = getIntent();

			if(intent != null && intent.hasExtra(EXTRA_RECIPIENT)) {
				initialRecipient = intent.getStringExtra(EXTRA_RECIPIENT);
			} else {
				initialRecipient = lastRecipient;
			}

			if(intent != null && intent.hasExtra(EXTRA_SUBJECT)) {
				initialSubject = intent.getStringExtra(EXTRA_SUBJECT);
			} else {
				initialSubject = lastSubject;
			}

			initialText = lastText;
		}

		if(initialRecipient != null) {
			recipientEdit.setText(initialRecipient);
		}

		if(initialSubject != null) {
			subjectEdit.setText(initialSubject);
		}

		if(initialText != null) {
			textEdit.setText(initialText);
		}

		final ArrayList<RedditAccount> accounts = RedditAccountManager.getInstance(this).getAccounts();
		final ArrayList<String> usernames = new ArrayList<>();

		for(RedditAccount account : accounts) {
			if(!account.isAnonymous()) {
				usernames.add(account.username);
			}
		}

		if(usernames.size() == 0) {
			General.quickToast(this, getString(R.string.error_toast_notloggedin));
			finish();
		}

		usernameSpinner.setAdapter(new ArrayAdapter<>(this, android.R.layout.simple_list_item_1, usernames));

		final ScrollView sv = new ScrollView(this);
		sv.addView(layout);
		setBaseActivityContentView(sv);
	}

	@Override
	protected void onSaveInstanceState(Bundle outState) {
		super.onSaveInstanceState(outState);
		outState.putString(SAVED_STATE_RECIPIENT, recipientEdit.getText().toString());
		outState.putString(SAVED_STATE_SUBJECT, subjectEdit.getText().toString());
		outState.putString(SAVED_STATE_TEXT, textEdit.getText().toString());
	}

	@Override
	public boolean onCreateOptionsMenu(Menu menu) {

		final MenuItem send = menu.add(R.string.comment_reply_send);
		send.setIcon(R.drawable.ic_action_send_dark);
		send.setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS);

		menu.add(R.string.comment_reply_preview);

		return true;
	}

	@Override
	public boolean onOptionsItemSelected(MenuItem item) {

		if(item.getTitle().equals(getString(R.string.comment_reply_send))) {

			final ProgressDialog progressDialog = new ProgressDialog(this);
			progressDialog.setTitle(getString(R.string.comment_reply_submitting_title));
			progressDialog.setMessage(getString(R.string.comment_reply_submitting_message));
			progressDialog.setIndeterminate(true);
			progressDialog.setCancelable(true);
			progressDialog.setCanceledOnTouchOutside(false);

			progressDialog.setOnCancelListener(new DialogInterface.OnCancelListener() {
				public void onCancel(final DialogInterface dialogInterface) {
					General.quickToast(PMSendActivity.this, getString(R.string.comment_reply_oncancel));
					General.safeDismissDialog(progressDialog);
				}
			});

			progressDialog.setOnKeyListener(new DialogInterface.OnKeyListener() {
				public boolean onKey(final DialogInterface dialogInterface, final int keyCode, final KeyEvent keyEvent) {

					if(keyCode == KeyEvent.KEYCODE_BACK) {
						General.quickToast(PMSendActivity.this, getString(R.string.comment_reply_oncancel));
						General.safeDismissDialog(progressDialog);
					}

					return true;
				}
			});

			final APIResponseHandler.ActionResponseHandler handler = new APIResponseHandler.ActionResponseHandler(this) {
				@Override
				protected void onSuccess(@Nullable final String redirectUrl) {
					AndroidCommon.UI_THREAD_HANDLER.post(new Runnable() {
						@Override
						public void run() {

							General.safeDismissDialog(progressDialog);

							mSendSuccess = true;

							lastText = null;
							lastRecipient = null;
							lastSubject = null;

							General.quickToast(PMSendActivity.this, getString(R.string.pm_send_done));
							finish();
						}
					});
				}

				@Override
				protected void onCallbackException(Throwable t) {
					BugReportActivity.handleGlobalError(PMSendActivity.this, t);
				}

				@Override
				protected void onFailure(@CacheRequest.RequestFailureType int type, Throwable t, Integer status, String readableMessage) {

					final RRError error = General.getGeneralErrorForFailure(context, type, t, status, null);

					AndroidCommon.UI_THREAD_HANDLER.post(new Runnable() {
						@Override
						public void run() {
							General.showResultDialog(PMSendActivity.this, error);
							General.safeDismissDialog(progressDialog);
						}
					});
				}

				@Override
				protected void onFailure(final APIFailureType type) {

					final RRError error = General.getGeneralErrorForFailure(context, type);

					AndroidCommon.UI_THREAD_HANDLER.post(new Runnable() {
						@Override
						public void run() {
							General.showResultDialog(PMSendActivity.this, error);
							General.safeDismissDialog(progressDialog);
						}
					});
				}
			};

			final CacheManager cm = CacheManager.getInstance(this);

			final ArrayList<RedditAccount> accounts = RedditAccountManager.getInstance(this).getAccounts();
			RedditAccount selectedAccount = null;

			for(RedditAccount account : accounts) {
				if(!account.isAnonymous() && account.username.equalsIgnoreCase((String)usernameSpinner.getSelectedItem())) {
					selectedAccount = account;
					break;
				}
			}

			if(selectedAccount == null) {
				throw new RuntimeException("Selected account no longer present");
			}

			RedditAPI.compose(
					cm,
					handler,
					selectedAccount,
					recipientEdit.getText().toString(),
					subjectEdit.getText().toString(),
					textEdit.getText().toString(),
					this);

			progressDialog.show();

		} else if(item.getTitle().equals(getString(R.string.comment_reply_preview))) {
			MarkdownPreviewDialog.newInstance(textEdit.getText().toString()).show(getSupportFragmentManager(), "MarkdownPreviewDialog");
		}

		return true;
	}

	@Override
	protected void onDestroy() {
		super.onDestroy();

		if(!mSendSuccess && textEdit != null) {
			lastRecipient = recipientEdit.getText().toString();
			lastSubject = subjectEdit.getText().toString();
			lastText = textEdit.getText().toString();
		}
	}

	@Override
	public void onBackPressed() {
		if(General.onBackPressed()) super.onBackPressed();
	}
}
