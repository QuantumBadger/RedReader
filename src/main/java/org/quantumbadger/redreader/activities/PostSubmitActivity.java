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

import android.content.DialogInterface;
import android.content.Intent;
import android.os.Bundle;
import android.os.Handler;
import android.os.Looper;
import android.text.InputType;
import android.view.KeyEvent;
import android.view.View;
import android.widget.ScrollView;
import com.actionbarsherlock.view.Menu;
import com.actionbarsherlock.view.MenuItem;
import org.apache.http.StatusLine;
import org.holoeverywhere.ArrayAdapter;
import org.holoeverywhere.app.Activity;
import org.holoeverywhere.app.ProgressDialog;
import org.holoeverywhere.widget.AdapterView;
import org.holoeverywhere.widget.EditText;
import org.holoeverywhere.widget.LinearLayout;
import org.holoeverywhere.widget.Spinner;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.cache.CacheManager;
import org.quantumbadger.redreader.cache.RequestFailureType;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.common.RRError;
import org.quantumbadger.redreader.fragments.MarkdownPreviewDialog;
import org.quantumbadger.redreader.reddit.APIResponseHandler;
import org.quantumbadger.redreader.reddit.RedditAPI;

import java.util.ArrayList;

// TODO save draft as static var (as in comments)
public class PostSubmitActivity extends Activity {

	private Spinner typeSpinner, usernameSpinner;
	private EditText subredditEdit, titleEdit, textEdit;

	private static final String[] postTypes = {"Link", "Self"};

	@Override
	protected void onCreate(Bundle savedInstanceState) {

		PrefsUtility.applyTheme(this);

		super.onCreate(savedInstanceState);

		final LinearLayout layout = (LinearLayout) getLayoutInflater().inflate(R.layout.post_submit);

		typeSpinner = (Spinner)layout.findViewById(R.id.post_submit_type);
		usernameSpinner = (Spinner)layout.findViewById(R.id.post_submit_username);
		subredditEdit = (EditText)layout.findViewById(R.id.post_submit_subreddit);
		titleEdit = (EditText)layout.findViewById(R.id.post_submit_title);
		textEdit = (EditText)layout.findViewById(R.id.post_submit_body);

		if(getIntent() != null && getIntent().hasExtra("subreddit")) {
			final String subreddit = getIntent().getStringExtra("subreddit");
			if(subreddit != null && subreddit.length() > 0 && !subreddit.equals("all") && subreddit.matches("\\w+")) {
				subredditEdit.setText(subreddit);
			}

		} else if(savedInstanceState != null && savedInstanceState.containsKey("post_title")) {
			titleEdit.setText(savedInstanceState.getString("post_title"));
			textEdit.setText(savedInstanceState.getString("post_body"));
			subredditEdit.setText(savedInstanceState.getString("subreddit"));
			typeSpinner.setSelection(savedInstanceState.getInt("post_type"));
		}

		final ArrayList<RedditAccount> accounts = RedditAccountManager.getInstance(this).getAccounts();
		final ArrayList<String> usernames = new ArrayList<String>();

		for(RedditAccount account : accounts) {
			if(!account.isAnonymous()) {
				usernames.add(account.username);
			}
		}

		if(usernames.size() == 0) {
			General.quickToast(this, "You must be logged in to do that."); // TODO string
			finish();
		}

		usernameSpinner.setAdapter(new ArrayAdapter<String>(this, android.R.layout.simple_list_item_1, usernames));
		typeSpinner.setAdapter(new ArrayAdapter<String>(this, android.R.layout.simple_list_item_1, postTypes));

		// TODO remove the duplicate code here
		if(typeSpinner.getSelectedItem().equals("Link")) {
			textEdit.setHint("URL"); // TODO string
			textEdit.setInputType(android.text.InputType.TYPE_CLASS_TEXT | InputType.TYPE_TEXT_VARIATION_URI);
		} else {
			textEdit.setHint("Self Text"); // TODO string
			textEdit.setInputType(android.text.InputType.TYPE_CLASS_TEXT | InputType.TYPE_TEXT_VARIATION_LONG_MESSAGE);
		}

		typeSpinner.setOnItemSelectedListener(new AdapterView.OnItemSelectedListener() {
			public void onItemSelected(AdapterView<?> parent, View view, int position, long id) {
				if(typeSpinner.getSelectedItem().equals("Link")) {
					textEdit.setHint("URL"); // TODO string
					textEdit.setInputType(android.text.InputType.TYPE_CLASS_TEXT | InputType.TYPE_TEXT_VARIATION_URI);
				} else {
					textEdit.setHint("Self Text"); // TODO string
					textEdit.setInputType(android.text.InputType.TYPE_CLASS_TEXT | InputType.TYPE_TEXT_VARIATION_LONG_MESSAGE);
				}
			}

			public void onNothingSelected(AdapterView<?> parent) {
			}
		});

		final ScrollView sv = new ScrollView(this);
		sv.addView(layout);
		setContentView(sv);
	}

	@Override
	protected void onSaveInstanceState(Bundle outState) {
		super.onSaveInstanceState(outState);
		outState.putString("post_title", titleEdit.getText().toString());
		outState.putString("post_body", textEdit.getText().toString());
		outState.putString("subreddit", subredditEdit.getText().toString());
		outState.putInt("post_type", typeSpinner.getSelectedItemPosition());
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
			final Intent captchaIntent = new Intent(this, CaptchaActivity.class);
			captchaIntent.putExtra("username", (String)usernameSpinner.getSelectedItem());
			startActivityForResult(captchaIntent, 0);

		} else if(item.getTitle().equals(getString(R.string.comment_reply_preview))) {
			MarkdownPreviewDialog.newInstance(textEdit.getText().toString()).show(getSupportFragmentManager());
		}

		return true;
	}

	@Override
	protected void onActivityResult(int requestCode, int resultCode, final Intent data) {
		super.onActivityResult(requestCode, resultCode, data);

		if(resultCode != RESULT_OK) return;

		final ProgressDialog progressDialog = new ProgressDialog(this);
		progressDialog.setTitle(getString(R.string.comment_reply_submitting_title));
		progressDialog.setMessage(getString(R.string.comment_reply_submitting_message));
		progressDialog.setIndeterminate(true);
		progressDialog.setCancelable(true);
		progressDialog.setCanceledOnTouchOutside(false);

		progressDialog.setOnCancelListener(new DialogInterface.OnCancelListener() {
			public void onCancel(final DialogInterface dialogInterface) {
				General.quickToast(PostSubmitActivity.this, getString(R.string.comment_reply_oncancel));
				progressDialog.dismiss();
			}
		});

		progressDialog.setOnKeyListener(new DialogInterface.OnKeyListener() {
			public boolean onKey(final DialogInterface dialogInterface, final int keyCode, final KeyEvent keyEvent) {

				if(keyCode == KeyEvent.KEYCODE_BACK) {
					General.quickToast(PostSubmitActivity.this, getString(R.string.comment_reply_oncancel));
					progressDialog.dismiss();
				}

				return true;
			}
		});

		final CacheManager cm = CacheManager.getInstance(this);

		final APIResponseHandler.ActionResponseHandler handler = new APIResponseHandler.ActionResponseHandler(this) {
			@Override
			protected void onSuccess() {
				new Handler(Looper.getMainLooper()).post(new Runnable() {
					public void run() {
						if(progressDialog.isShowing()) progressDialog.dismiss();
						General.quickToast(PostSubmitActivity.this, getString(R.string.post_submit_done));
						finish();
					}
				});
			}

			@Override
			protected void onCallbackException(Throwable t) {
				BugReportActivity.handleGlobalError(PostSubmitActivity.this, t);
			}

			@Override
			protected void onFailure(RequestFailureType type, Throwable t, StatusLine status, String readableMessage) {

				final RRError error = General.getGeneralErrorForFailure(context, type, t, status);

				new Handler(Looper.getMainLooper()).post(new Runnable() {
					public void run() {
						General.showResultDialog(PostSubmitActivity.this, error);
						if(progressDialog.isShowing()) progressDialog.dismiss();
					}
				});
			}

			@Override
			protected void onFailure(final APIFailureType type) {

				final RRError error = General.getGeneralErrorForFailure(context, type);

				new Handler(Looper.getMainLooper()).post(new Runnable() {
					public void run() {
						General.showResultDialog(PostSubmitActivity.this, error);
						if(progressDialog.isShowing()) progressDialog.dismiss();
					}
				});
			}
		};

		final boolean is_self = !typeSpinner.getSelectedItem().equals("Link");

		final RedditAccount selectedAccount = RedditAccountManager.getInstance(this).getAccount((String) usernameSpinner.getSelectedItem());

		final String subreddit = subredditEdit.getText().toString();
		final String title = titleEdit.getText().toString();
		final String text = textEdit.getText().toString();
		final String captchaId = data.getStringExtra("captchaId");
		final String captchaText = data.getStringExtra("captchaText");

		RedditAPI.submit(cm, handler, selectedAccount, is_self, subreddit, title, text, captchaId, captchaText, this);

		progressDialog.show();
	}
}
