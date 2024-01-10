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
import android.net.Uri;
import android.os.Bundle;
import android.view.KeyEvent;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.ArrayAdapter;
import android.widget.CheckBox;
import android.widget.EditText;
import android.widget.LinearLayout;
import android.widget.ScrollView;
import android.widget.Spinner;
import android.widget.TextView;
import android.widget.Toast;
import android.widget.Button;
import androidx.annotation.NonNull;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.cache.CacheManager;
import org.quantumbadger.redreader.common.AndroidCommon;
import org.quantumbadger.redreader.common.DialogUtils;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.LinkHandler;
import org.quantumbadger.redreader.common.Optional;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.common.RRError;
import org.quantumbadger.redreader.common.StringUtils;
import org.quantumbadger.redreader.fragments.MarkdownPreviewDialog;
import org.quantumbadger.redreader.reddit.APIResponseHandler;
import org.quantumbadger.redreader.reddit.RedditAPI;
import org.quantumbadger.redreader.reddit.kthings.RedditIdAndType;

import java.util.ArrayList;
import java.util.Objects;

public class CommentReplyActivity extends BaseActivity {

	private enum ParentType {
		MESSAGE, COMMENT_OR_POST
	}

	private Spinner usernameSpinner;
	private EditText textEdit;
	private CheckBox inboxReplies;
	private Button uploadPicture;
	private RedditIdAndType parentIdAndType = null;

	private ParentType mParentType;

	private boolean mDraftReset = false;
	private static String lastText;
	private static RedditIdAndType lastParentIdAndType;

	public static final String PARENT_TYPE = "parentType";
	public static final String PARENT_TYPE_MESSAGE = "parentTypeMessage";

	public static final String PARENT_ID_AND_TYPE_KEY = "parentIdAndType";
	public static final String PARENT_MARKDOWN_KEY = "parent_markdown";
	private static final String COMMENT_TEXT_KEY = "comment_text";

	@Override
	protected void onCreate(final Bundle savedInstanceState) {

		PrefsUtility.applyTheme(this);

		super.onCreate(savedInstanceState);

		final Intent intent = getIntent();

		if(intent != null
				&& intent.hasExtra(PARENT_TYPE)
				&& PARENT_TYPE_MESSAGE.equals(intent.getStringExtra(PARENT_TYPE))) {

			mParentType = ParentType.MESSAGE;
			setTitle(R.string.submit_pmreply_actionbar);

		} else {
			mParentType = ParentType.COMMENT_OR_POST;
			setTitle(R.string.submit_comment_actionbar);
		}

		final LinearLayout layout
				= (LinearLayout)getLayoutInflater().inflate(R.layout.comment_reply, null);

		usernameSpinner = layout.findViewById(R.id.comment_reply_username);
		inboxReplies = layout.findViewById(R.id.comment_reply_inbox);
		textEdit = layout.findViewById(R.id.comment_reply_text);

		uploadPicture = layout.findViewById(R.id.comment_reply_picture);

		uploadPicture.setOnClickListener(v -> uploadPicture());

		if(mParentType == ParentType.COMMENT_OR_POST) {
			inboxReplies.setVisibility(View.VISIBLE);
		}

		if(intent != null && intent.hasExtra(PARENT_ID_AND_TYPE_KEY)) {
			//noinspection deprecation
			parentIdAndType = Objects.requireNonNull(
					intent.getParcelableExtra(PARENT_ID_AND_TYPE_KEY));

		} else if(savedInstanceState != null
				&& savedInstanceState.containsKey(PARENT_ID_AND_TYPE_KEY)) {
			//noinspection deprecation
			parentIdAndType = Objects.requireNonNull(
					savedInstanceState.getParcelable(PARENT_ID_AND_TYPE_KEY));

		} else {
			throw new RuntimeException("No parent ID in CommentReplyActivity");
		}

		final String existingCommentText;

		if(savedInstanceState != null
				&& savedInstanceState.containsKey(COMMENT_TEXT_KEY)) {
			existingCommentText = savedInstanceState.getString(COMMENT_TEXT_KEY);

		} else if(lastText != null && parentIdAndType.equals(lastParentIdAndType)) {
			existingCommentText = lastText;

		} else {
			existingCommentText = null;
		}

		if(existingCommentText != null) {
			textEdit.setText(existingCommentText);
		}

		if(intent != null && intent.hasExtra(PARENT_MARKDOWN_KEY)) {
			final TextView parentMarkdown = layout.findViewById(R.id.comment_parent_text);
			parentMarkdown.setText(intent.getStringExtra(PARENT_MARKDOWN_KEY));
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
		outState.putString(COMMENT_TEXT_KEY, textEdit.getText().toString());
		outState.putParcelable(PARENT_ID_AND_TYPE_KEY, parentIdAndType);
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

			final APIResponseHandler.SubmitResponseHandler handler
					= new APIResponseHandler.SubmitResponseHandler(this) {

				@Override
				public void onSubmitErrors(@NonNull final ArrayList<String> errors) {

					AndroidCommon.UI_THREAD_HANDLER.post(() -> {

						final String errorsJoined = StringUtils.join(errors, " ");

						DialogUtils.showDialog(
								CommentReplyActivity.this,
								getString(R.string.error_comment_submit_title),
								errorsJoined);

						General.safeDismissDialog(progressDialog);
					});
				}

				@Override
				public void onSuccess(
						@NonNull final Optional<String> redirectUrl,
						@NonNull final Optional<String> thingId) {

					AndroidCommon.UI_THREAD_HANDLER.post(() -> {
						General.safeDismissDialog(progressDialog);

						if(mParentType == ParentType.MESSAGE) {
							General.quickToast(
									CommentReplyActivity.this,
									getString(R.string.pm_reply_done));
						} else {
							General.quickToast(
									CommentReplyActivity.this,
									getString(R.string.comment_reply_done_norefresh));
						}

						mDraftReset = true;
						lastText = null;
						lastParentIdAndType = null;

						redirectUrl.ifPresent(url -> LinkHandler.onLinkClicked(
								CommentReplyActivity.this,
								Uri.parse(url)
										.buildUpon()
										.appendQueryParameter("context", "1")
										.build()
										.toString()));

						finish();
					});
				}

				@Override
				protected void onCallbackException(final Throwable t) {
					BugReportActivity.handleGlobalError(CommentReplyActivity.this, t);
				}

				@Override
				protected void onFailure(@NonNull final RRError error) {
					General.showResultDialog(CommentReplyActivity.this, error);
					General.safeDismissDialog(progressDialog);
				}
			};

			final APIResponseHandler.ActionResponseHandler inboxHandler
					= new APIResponseHandler.ActionResponseHandler(this) {
				@Override
				protected void onSuccess() {
					// Do nothing (result expected)
				}

				@Override
				protected void onCallbackException(final Throwable t) {
					BugReportActivity.handleGlobalError(CommentReplyActivity.this, t);
				}

				@Override
				protected void onFailure(@NonNull final RRError error) {
					Toast.makeText(
							context,
							getString(R.string.disable_replies_to_infobox_failed),
							Toast.LENGTH_SHORT).show();
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
			final boolean sendRepliesToInbox;
			if(mParentType == ParentType.COMMENT_OR_POST) {
				sendRepliesToInbox = inboxReplies.isChecked();
			} else {
				sendRepliesToInbox = true;
			}
			RedditAPI.comment(
					cm,
					handler,
					inboxHandler,
					selectedAccount,
					parentIdAndType,
					textEdit.getText().toString(),
					sendRepliesToInbox,
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

		if(textEdit != null && !mDraftReset) {
			lastText = textEdit.getText().toString();
			lastParentIdAndType = parentIdAndType;
		}
	}

	@Override
	public void onBackPressed() {
		if(General.onBackPressed()) {
			super.onBackPressed();
		}
	}

	private void uploadPicture() {
		final Intent intent = new Intent(this, ImgurUploadActivity.class);
		startActivityForResultWithCallback(intent, (resultCode, data) -> {
			if (resultCode == 0 && data != null) {
				final Uri uploadedImageUrl = data.getData();
				if (uploadedImageUrl != null) {
					// set the picture into textedit as a link: [Picture](PictureURL)
					final String existingText = textEdit.getText().toString();
					final String picturePretext = getString(R.string.comment_picture_pretext);
					final String linkText =
						"["+ picturePretext +"](" + uploadedImageUrl + ")";
					final String combinedText = existingText + " " + linkText;
					textEdit.setText(combinedText);
				}
			}
		});
	}

}
