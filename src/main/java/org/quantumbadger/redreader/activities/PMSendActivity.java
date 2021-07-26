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

import android.app.ProgressDialog;
import android.content.Intent;
import android.os.Bundle;
import android.view.KeyEvent;
import android.view.Menu;
import android.view.MenuItem;
import android.widget.ArrayAdapter;
import android.widget.EditText;
import android.widget.LinearLayout;
import android.widget.ScrollView;
import android.widget.Spinner;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.cache.CacheManager;
import org.quantumbadger.redreader.cache.CacheRequest;
import org.quantumbadger.redreader.common.AndroidCommon;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.Optional;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.common.RRError;
import org.quantumbadger.redreader.fragments.MarkdownPreviewDialog;
import org.quantumbadger.redreader.http.FailedRequestBody;
import org.quantumbadger.redreader.reddit.APIResponseHandler;
import org.quantumbadger.redreader.reddit.RedditAPI;

import java.util.ArrayList;

public class PMSendActivity extends BaseActivity {

	public static final String EXTRA_RECIPIENT = "recipient";
	public static final String EXTRA_SUBJECT = "subject";
	public static final String EXTRA_TEXT = "text";

	private static final String SAVED_STATE_RECIPIENT = "recipient";
	private static final String SAVED_STATE_TEXT = "pm_text";
	private static final String SAVED_STATE_SUBJECT = "pm_subject";

	private Spinner usernameSpinner;
	private EditText recipientEdit;
	private EditText subjectEdit;
	private EditText textEdit;

	private boolean mSendSuccess;

	private static String lastText;
	private static String lastRecipient;
	private static String lastSubject;

	@Override
	protected void onCreate(final Bundle savedInstanceState) {

		PrefsUtility.applyTheme(this);

		super.onCreate(savedInstanceState);

		setTitle(R.string.pm_send_actionbar);

		final LinearLayout layout
				= (LinearLayout)getLayoutInflater().inflate(R.layout.pm_send, null);

		usernameSpinner = layout.findViewById(R.id.pm_send_username);
		recipientEdit = layout.findViewById(R.id.pm_send_recipient);
		subjectEdit = layout.findViewById(R.id.pm_send_subject);
		textEdit = layout.findViewById(R.id.pm_send_text);

		final String initialRecipient;
		final String initialSubject;
		final String initialText;

		if(savedInstanceState != null
				&& savedInstanceState.containsKey(SAVED_STATE_TEXT)) {
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

			if(intent != null && intent.hasExtra(EXTRA_TEXT)) {
				initialText = intent.getStringExtra(EXTRA_TEXT);
			} else {
				initialText = lastText;
			}
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

		final ArrayList<RedditAccount> accounts = RedditAccountManager.getInstance(this)
				.getAccounts();
		final ArrayList<String> usernames = new ArrayList<>();

		for(final RedditAccount account : accounts) {
			if(!account.isAnonymous()) {
				usernames.add(account.username);
			}
		}

		if(usernames.isEmpty()) {
			General.quickToast(this, getString(R.string.error_toast_notloggedin));
			finish();
		}

		usernameSpinner.setAdapter(new ArrayAdapter<>(
				this,
				android.R.layout.simple_list_item_1,
				usernames));

		final ScrollView sv = new ScrollView(this);
		sv.addView(layout);
		setBaseActivityListing(sv);
	}

	@Override
	protected void onSaveInstanceState(@NonNull final Bundle outState) {
		super.onSaveInstanceState(outState);
		outState.putString(SAVED_STATE_RECIPIENT, recipientEdit.getText().toString());
		outState.putString(SAVED_STATE_SUBJECT, subjectEdit.getText().toString());
		outState.putString(SAVED_STATE_TEXT, textEdit.getText().toString());
	}

	@Override
	public boolean onCreateOptionsMenu(final Menu menu) {

		final MenuItem send = menu.add(R.string.comment_reply_send);
		send.setIcon(R.drawable.ic_action_send_dark);
		send.setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS);

		menu.add(R.string.comment_reply_preview);

		return true;
	}

	@Override
	public boolean onOptionsItemSelected(final MenuItem item) {

		if(item.getTitle().equals(getString(R.string.comment_reply_send))) {

			final ProgressDialog progressDialog = new ProgressDialog(this);
			progressDialog.setTitle(getString(R.string.comment_reply_submitting_title));
			progressDialog.setMessage(getString(R.string.comment_reply_submitting_message));
			progressDialog.setIndeterminate(true);
			progressDialog.setCancelable(true);
			progressDialog.setCanceledOnTouchOutside(false);

			progressDialog.setOnCancelListener(dialogInterface -> {
				General.quickToast(this, getString(R.string.comment_reply_oncancel));
				General.safeDismissDialog(progressDialog);
			});

			progressDialog.setOnKeyListener((dialogInterface, keyCode, keyEvent) -> {

				if(keyCode == KeyEvent.KEYCODE_BACK) {
					General.quickToast(this, getString(R.string.comment_reply_oncancel));
					General.safeDismissDialog(progressDialog);
				}

				return true;
			});

			final APIResponseHandler.ActionResponseHandler handler
					= new APIResponseHandler.ActionResponseHandler(this) {
				@Override
				protected void onSuccess() {
					AndroidCommon.UI_THREAD_HANDLER.post(() -> {

						General.safeDismissDialog(progressDialog);

						mSendSuccess = true;

						lastText = null;
						lastRecipient = null;
						lastSubject = null;

						General.quickToast(
								PMSendActivity.this,
								getString(R.string.pm_send_done));
						finish();
					});
				}

				@Override
				protected void onCallbackException(final Throwable t) {
					BugReportActivity.handleGlobalError(PMSendActivity.this, t);
				}

				@Override
				protected void onFailure(
						@CacheRequest.RequestFailureType final int type,
						final Throwable t,
						final Integer status,
						final String readableMessage,
						@NonNull final Optional<FailedRequestBody> response) {

					final RRError error = General.getGeneralErrorForFailure(
							context,
							type,
							t,
							status,
							"PM send",
							response);

					General.showResultDialog(PMSendActivity.this, error);
					General.safeDismissDialog(progressDialog);
				}

				@Override
				protected void onFailure(
						@NonNull final APIFailureType type,
						@Nullable final String debuggingContext,
						@NonNull final Optional<FailedRequestBody> response) {

					final RRError error = General.getGeneralErrorForFailure(
							context,
							type,
							debuggingContext,
							response);

					General.showResultDialog(PMSendActivity.this, error);
					General.safeDismissDialog(progressDialog);
				}
			};

			final CacheManager cm = CacheManager.getInstance(this);

			final ArrayList<RedditAccount> accounts = RedditAccountManager.getInstance(
					this).getAccounts();
			RedditAccount selectedAccount = null;

			for(final RedditAccount account : accounts) {
				if(!account.isAnonymous()
						&& account.username.equalsIgnoreCase(
						(String)usernameSpinner.getSelectedItem())) {
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
			MarkdownPreviewDialog.newInstance(textEdit.getText().toString())
					.show(getSupportFragmentManager(), "MarkdownPreviewDialog");
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
		if(General.onBackPressed()) {
			super.onBackPressed();
		}
	}
}
