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
import android.os.Bundle;
import android.view.KeyEvent;
import android.view.Menu;
import android.view.MenuItem;
import android.widget.EditText;
import android.widget.ScrollView;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.cache.CacheManager;
import org.quantumbadger.redreader.common.AndroidCommon;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.Optional;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.common.RRError;
import org.quantumbadger.redreader.fragments.MarkdownPreviewDialog;
import org.quantumbadger.redreader.http.FailedRequestBody;
import org.quantumbadger.redreader.reddit.APIResponseHandler;
import org.quantumbadger.redreader.reddit.RedditAPI;

public class CommentEditActivity extends BaseActivity {

	private EditText textEdit;

	private String commentIdAndType = null;
	private boolean isSelfPost = false;

	@Override
	protected void onCreate(final Bundle savedInstanceState) {

		PrefsUtility.applyTheme(this);

		super.onCreate(savedInstanceState);

		if(getIntent() != null && getIntent().hasExtra("isSelfPost")
				&& getIntent().getBooleanExtra("isSelfPost", false)) {
			setTitle(R.string.edit_post_actionbar);
			isSelfPost = true;
		} else {
			setTitle(R.string.edit_comment_actionbar);
		}
		textEdit = (EditText)getLayoutInflater().inflate(R.layout.comment_edit, null);

		if(getIntent() != null && getIntent().hasExtra("commentIdAndType")) {
			commentIdAndType = getIntent().getStringExtra("commentIdAndType");
			textEdit.setText(getIntent().getStringExtra("commentText"));

		} else if(savedInstanceState != null && savedInstanceState.containsKey(
				"commentIdAndType")) {
			textEdit.setText(savedInstanceState.getString("commentText"));
			commentIdAndType = savedInstanceState.getString("commentIdAndType");
		}

		final ScrollView sv = new ScrollView(this);
		sv.addView(textEdit);
		setBaseActivityListing(sv);
	}

	@Override
	protected void onSaveInstanceState(@NonNull final Bundle outState) {
		super.onSaveInstanceState(outState);
		outState.putString("commentText", textEdit.getText().toString());
		outState.putString("commentIdAndType", commentIdAndType);
	}

	@Override
	public boolean onCreateOptionsMenu(final Menu menu) {

		final MenuItem send = menu.add(R.string.comment_edit_save);
		send.setIcon(R.drawable.ic_action_save_dark);
		send.setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS);

		menu.add(R.string.comment_reply_preview);

		return true;
	}

	@Override
	public boolean onOptionsItemSelected(final MenuItem item) {

		if(item.getTitle().equals(getString(R.string.comment_edit_save))) {

			final ProgressDialog progressDialog = new ProgressDialog(this);
			progressDialog.setTitle(getString(R.string.comment_reply_submitting_title));
			progressDialog.setMessage(getString(R.string.comment_reply_submitting_message));
			progressDialog.setIndeterminate(true);
			progressDialog.setCancelable(true);
			progressDialog.setCanceledOnTouchOutside(false);

			progressDialog.setOnCancelListener(dialogInterface -> {
				General.quickToast(this, R.string.comment_reply_oncancel);
				General.safeDismissDialog(progressDialog);
			});

			progressDialog.setOnKeyListener((dialogInterface, keyCode, keyEvent) -> {

				if(keyCode == KeyEvent.KEYCODE_BACK) {
					General.quickToast(this, R.string.comment_reply_oncancel);
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

						if(isSelfPost) {
							General.quickToast(
									CommentEditActivity.this,
									R.string.post_edit_done);
						} else {
							General.quickToast(
									CommentEditActivity.this,
									R.string.comment_edit_done);
						}

						finish();
					});
				}

				@Override
				protected void onCallbackException(final Throwable t) {
					BugReportActivity.handleGlobalError(CommentEditActivity.this, t);
				}

				@Override
				protected void onFailure(
						final int type,
						@Nullable final Throwable t,
						@Nullable final Integer httpStatus,
						@Nullable final String readableMessage,
						@NonNull final Optional<FailedRequestBody> response) {

					final RRError error = General.getGeneralErrorForFailure(
							context,
							type,
							t,
							httpStatus,
							"Comment edit",
							response);

					General.showResultDialog(CommentEditActivity.this, error);
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

					General.showResultDialog(CommentEditActivity.this, error);
					General.safeDismissDialog(progressDialog);
				}
			};

			final CacheManager cm = CacheManager.getInstance(this);
			final RedditAccount selectedAccount = RedditAccountManager.getInstance(this)
					.getDefaultAccount();

			RedditAPI.editComment(
					cm,
					handler,
					selectedAccount,
					commentIdAndType,
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
	public void onBackPressed() {
		if(General.onBackPressed()) {
			super.onBackPressed();
		}
	}
}
