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
import android.content.DialogInterface;
import android.os.Bundle;
import android.view.KeyEvent;
import android.view.Menu;
import android.view.MenuItem;
import android.widget.*;
import org.apache.http.StatusLine;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.cache.CacheManager;
import org.quantumbadger.redreader.cache.RequestFailureType;
import org.quantumbadger.redreader.common.AndroidApi;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.common.RRError;
import org.quantumbadger.redreader.fragments.MarkdownPreviewDialog;
import org.quantumbadger.redreader.reddit.APIResponseHandler;
import org.quantumbadger.redreader.reddit.RedditAPI;

import java.util.ArrayList;

public class CommentReplyActivity extends BaseActivity {

	private Spinner usernameSpinner;
	private EditText textEdit;

	private String parentIdAndType = null;

	private static String lastText, lastParentIdAndType;

	@Override
	protected void onCreate(Bundle savedInstanceState) {

		PrefsUtility.applyTheme(this);

		super.onCreate(savedInstanceState);

		final LinearLayout layout = (LinearLayout) getLayoutInflater().inflate(R.layout.comment_reply, null);

		usernameSpinner = (Spinner)layout.findViewById(R.id.comment_reply_username);
		textEdit = (EditText)layout.findViewById(R.id.comment_reply_text);

		if(getIntent() != null && getIntent().hasExtra("parentIdAndType")) {
			parentIdAndType = getIntent().getStringExtra("parentIdAndType");

		} else if(savedInstanceState != null && savedInstanceState.containsKey("comment_text")) {
			textEdit.setText(savedInstanceState.getString("comment_text"));
			parentIdAndType = savedInstanceState.getString("parentIdAndType");

		} else if(lastText != null) {
			textEdit.setText(lastText);
			parentIdAndType = lastParentIdAndType;
		}

		final ArrayList<RedditAccount> accounts = RedditAccountManager.getInstance(this).getAccounts();
		final ArrayList<String> usernames = new ArrayList<String>();

		for(RedditAccount account : accounts) {
			if(!account.isAnonymous()) {
				usernames.add(account.username);
			}
		}

		if(usernames.size() == 0) {
			General.quickToast(this, "You must be logged in to do that.");
			finish();
		}

		usernameSpinner.setAdapter(new ArrayAdapter<String>(this, android.R.layout.simple_list_item_1, usernames));

		final ScrollView sv = new ScrollView(this);
		sv.addView(layout);
		setContentView(sv);
	}

	@Override
	protected void onSaveInstanceState(Bundle outState) {
		super.onSaveInstanceState(outState);
		outState.putString("comment_text", textEdit.getText().toString());
		outState.putString("parentIdAndType", parentIdAndType);
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
					General.quickToast(CommentReplyActivity.this, getString(R.string.comment_reply_oncancel));
					progressDialog.dismiss();
				}
			});

			progressDialog.setOnKeyListener(new DialogInterface.OnKeyListener() {
				public boolean onKey(final DialogInterface dialogInterface, final int keyCode, final KeyEvent keyEvent) {

					if(keyCode == KeyEvent.KEYCODE_BACK) {
						General.quickToast(CommentReplyActivity.this, getString(R.string.comment_reply_oncancel));
						progressDialog.dismiss();
					}

					return true;
				}
			});

			final APIResponseHandler.ActionResponseHandler handler = new APIResponseHandler.ActionResponseHandler(this) {
				@Override
				protected void onSuccess() {
					AndroidApi.UI_THREAD_HANDLER.post(new Runnable() {
						public void run() {
							if(progressDialog.isShowing()) progressDialog.dismiss();
							General.quickToast(CommentReplyActivity.this, getString(R.string.comment_reply_done));
							finish();
						}
					});
				}

				@Override
				protected void onCallbackException(Throwable t) {
					BugReportActivity.handleGlobalError(CommentReplyActivity.this, t);
				}

				@Override
				protected void onFailure(RequestFailureType type, Throwable t, StatusLine status, String readableMessage) {

					final RRError error = General.getGeneralErrorForFailure(context, type, t, status, null);

					AndroidApi.UI_THREAD_HANDLER.post(new Runnable() {
						public void run() {
							General.showResultDialog(CommentReplyActivity.this, error);
							if(progressDialog.isShowing()) progressDialog.dismiss();
						}
					});
				}

				@Override
				protected void onFailure(final APIFailureType type) {

					final RRError error = General.getGeneralErrorForFailure(context, type);

					AndroidApi.UI_THREAD_HANDLER.post(new Runnable() {
						public void run() {
							General.showResultDialog(CommentReplyActivity.this, error);
							if(progressDialog.isShowing()) progressDialog.dismiss();
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

			RedditAPI.comment(cm, handler, selectedAccount, parentIdAndType, textEdit.getText().toString(), this);

			progressDialog.show();

		} else if(item.getTitle().equals(getString(R.string.comment_reply_preview))) {
			MarkdownPreviewDialog.newInstance(textEdit.getText().toString()).show(getFragmentManager(), "MarkdownPreviewDialog");
		}

		return true;
	}

	@Override
	protected void onDestroy() {
		super.onDestroy();

		if(textEdit != null) {
			lastText = textEdit.getText().toString();
			lastParentIdAndType = parentIdAndType;
		}
	}

	@Override
	public void onBackPressed() {
		if(General.onBackPressed()) super.onBackPressed();
	}
}
