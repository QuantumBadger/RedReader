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

package org.quantumbadger.redreader.fragments.postsubmit;

import android.app.ProgressDialog;
import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.text.InputType;
import android.util.Log;
import android.view.KeyEvent;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.widget.CheckBox;
import android.widget.TextView;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.annotation.StringRes;
import androidx.appcompat.app.AppCompatActivity;
import androidx.core.os.BundleCompat;
import androidx.fragment.app.Fragment;
import androidx.fragment.app.FragmentActivity;

import com.google.android.material.textfield.MaterialAutoCompleteTextView;
import com.google.android.material.textfield.TextInputEditText;
import com.google.android.material.textfield.TextInputLayout;

import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.activities.BaseActivity;
import org.quantumbadger.redreader.activities.BugReportActivity;
import org.quantumbadger.redreader.activities.ImgurUploadActivity;
import org.quantumbadger.redreader.cache.CacheManager;
import org.quantumbadger.redreader.common.AndroidCommon;
import org.quantumbadger.redreader.common.Consumer;
import org.quantumbadger.redreader.common.DialogUtils;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.Optional;
import org.quantumbadger.redreader.common.RRError;
import org.quantumbadger.redreader.common.StringUtils;
import org.quantumbadger.redreader.common.UriString;
import org.quantumbadger.redreader.fragments.MarkdownPreviewDialog;
import org.quantumbadger.redreader.reddit.APIResponseHandler;
import org.quantumbadger.redreader.reddit.RedditAPI;
import org.quantumbadger.redreader.reddit.RedditFlairChoice;
import org.quantumbadger.redreader.reddit.things.SubredditCanonicalId;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Locale;
import java.util.Objects;

public class PostSubmitContentFragment extends Fragment {

	@NonNull private static final String TAG = "PostSubmitContentFrag";

	public static class Args {

		@NonNull private static final String KEY_USER = "user";
		@NonNull private static final String KEY_SUBREDDIT = "subreddit";
		@NonNull private static final String KEY_URL = "url";

		@NonNull public final String username;
		@NonNull public final SubredditCanonicalId subreddit;
		@Nullable public final String url;

		public Args(
				@NonNull final String username,
				@NonNull final SubredditCanonicalId subreddit,
				@Nullable final String url) {
			this.username = username;
			this.subreddit = subreddit;
			this.url = url;
		}

		@NonNull
		public Bundle toBundle() {
			final Bundle result = new Bundle(3);
			result.putString(KEY_USER, username);
			result.putParcelable(KEY_SUBREDDIT, subreddit);
			result.putString(KEY_URL, url);

			return result;
		}

		@NonNull
		public static Args fromBundle(@NonNull final Bundle bundle) {
			return new Args(
					Objects.requireNonNull(bundle.getString(KEY_USER)),
					Objects.requireNonNull(BundleCompat.getParcelable(bundle,
							KEY_SUBREDDIT,
							SubredditCanonicalId.class)),
					bundle.getString(KEY_URL));
		}
	}

	public interface Listener {
		void onContentFragmentSubmissionSuccess(@Nullable UriString redirectUrl);
		void onContentFragmentSubredditDoesNotExist();
		void onContentFragmentSubredditPermissionDenied();
		void onContentFragmentFlairRequestError(@NonNull RRError error);
	}

	private static final String POST_TYPE_LINK = "Link";
	private static final String POST_TYPE_SELF = "Text";
	private static final String POST_TYPE_IMGUR = "Upload to Imgur";

	private static final String[] POST_TYPES = {POST_TYPE_LINK, POST_TYPE_SELF, POST_TYPE_IMGUR};

	private boolean mDraftReset = false;

	private static String lastType;
	private static String lastTitle;
	private static String lastText;
	private static boolean lastNsfw;
	private static boolean lastSpoiler;
	private static boolean lastInbox;

	private boolean mActive = true;

	private Context mContext;

	private RedditAccount mSelectedAccount;
	private SubredditCanonicalId mSelectedSubreddit;

	private View mLoadingSpinnerView;
	private View mMainControls;

	private MaterialAutoCompleteTextView mTypeSpinner;
	private MaterialAutoCompleteTextView mFlairSpinner;
	private TextInputLayout mFlairSpinnerLayout;
	private TextInputEditText mTitleEdit;
	private TextInputEditText mTextEdit;
	private TextInputLayout mTextEditLayout;
	private CheckBox mSendRepliesToInboxCheckbox;
	private CheckBox mMarkAsNsfwCheckbox;
	private CheckBox mMarkAsSpoilerCheckbox;

	private final HashMap<String, String> mFlairIds = new HashMap<>();

	@Override
	public void onResume() {
		super.onResume();

		final FragmentActivity activity = getActivity();

		if(activity != null) {
			activity.setTitle(R.string.submit_post_actionbar);
		}
	}

	@Override
	public void onDestroy() {
		super.onDestroy();
		mActive = false;

		// Store information for draft
		if(mTitleEdit != null && mTextEdit != null && !mDraftReset) {
			lastType = mTypeSpinner.getText().toString();
			lastTitle = mTitleEdit.getText().toString();
			lastText = mTextEdit.getText().toString();
			lastInbox = mSendRepliesToInboxCheckbox.isChecked();
			lastNsfw = mMarkAsNsfwCheckbox.isChecked();
			lastSpoiler = mMarkAsSpoilerCheckbox.isChecked();
		}
	}

	@Nullable
	@Override
	public View onCreateView(
			@NonNull final LayoutInflater inflater,
			@Nullable final ViewGroup container,
			@Nullable final Bundle savedInstanceState) {

		setHasOptionsMenu(true);

		mContext = Objects.requireNonNull(container).getContext();

		final RedditAccountManager accountManager = RedditAccountManager.getInstance(mContext);

		final Args args = Args.fromBundle(requireArguments());

		mSelectedAccount = accountManager.getAccount(args.username);

		if(mSelectedAccount == null) {

			BugReportActivity.handleGlobalError(mContext, new RuntimeException(
					"Selected account is not in the account manager"));

			return null;
		}

		mSelectedSubreddit = args.subreddit;

		final View root = inflater.inflate(R.layout.post_submit, container, false);

		mMainControls = Objects.requireNonNull(
				root.findViewById(R.id.post_submit_main_controls));

		mLoadingSpinnerView = Objects.requireNonNull(
				General.findViewById(root, R.id.post_submit_loading_spinner_view));

		mTypeSpinner = Objects.requireNonNull(root.findViewById(R.id.post_submit_type));
		mFlairSpinner = Objects.requireNonNull(root.findViewById(R.id.post_submit_flair));
		mFlairSpinnerLayout = Objects.requireNonNull(
				root.findViewById(R.id.post_submit_flair_layout));
		mTitleEdit = Objects.requireNonNull(root.findViewById(R.id.post_submit_title));
		mTextEdit = Objects.requireNonNull(root.findViewById(R.id.post_submit_body));
		mTextEditLayout = Objects.requireNonNull(root.findViewById(R.id.post_submit_body_layout));

		mSendRepliesToInboxCheckbox = Objects.requireNonNull(
				root.findViewById(R.id.post_submit_send_replies_to_inbox));

		mMarkAsNsfwCheckbox = Objects.requireNonNull(
				root.findViewById(R.id.post_submit_mark_nsfw));

		mMarkAsSpoilerCheckbox = Objects.requireNonNull(
				root.findViewById(R.id.post_submit_mark_spoiler));

		final TextView heading = root.findViewById(R.id.post_submit_heading);

		heading.setText(String.format(
				Locale.US,
				getString(R.string.post_submit_heading),
				args.subreddit,
				args.username));

		mTypeSpinner.setText(POST_TYPE_LINK);

		AndroidCommon.setAutoCompleteTextViewItemsNoFilter(mTypeSpinner, POST_TYPES);

		if(args.url != null) {
			mTextEdit.setText(args.url);
		}

		// Fetch information from draft if a draft exists
		if(args.url == null && (lastTitle != null || lastText != null)) {
			mTypeSpinner.setText(lastType);
			mTitleEdit.setText(lastTitle);
			mTextEdit.setText(lastText);
			mSendRepliesToInboxCheckbox.setChecked(lastInbox);
			mMarkAsSpoilerCheckbox.setChecked(lastSpoiler);
			mMarkAsNsfwCheckbox.setChecked(lastNsfw);
		}

		setHint();

		AndroidCommon.onTextChanged(mTypeSpinner, this::setHint);

		requestSubredditDetails();

		return root;
	}

	private void disableFlairSpinner(@StringRes final int message) {

		final Context appContext = mContext.getApplicationContext();

		mFlairSpinner.setAdapter(null);
		mFlairSpinner.setText(appContext.getString(message));

		mFlairSpinnerLayout.setEnabled(false);

		mFlairSpinner.setEnabled(false);
		mFlairSpinner.setAlpha(0.5f);
	}

	private void enableFlairSpinner(@NonNull final Collection<RedditFlairChoice> choices) {

		final Context appContext = mContext.getApplicationContext();

		mFlairSpinner.setEnabled(true);
		mFlairSpinner.setAlpha(1.0f);

		final ArrayList<String> choiceStrings = new ArrayList<>(choices.size() + 1);
		mFlairIds.clear();

		final String noneSelected = appContext.getString(R.string.post_submit_flair_none_selected);

		choiceStrings.add(noneSelected);

		for(final RedditFlairChoice choice : choices) {
			choiceStrings.add(choice.text);
			mFlairIds.put(choice.text, choice.templateId);
		}

		AndroidCommon.setAutoCompleteTextViewItemsNoFilter(mFlairSpinner, choiceStrings);

		mFlairSpinner.setText(noneSelected);
	}

	private void setHint() {

		final Object selected = mTypeSpinner.getText().toString();

		if(selected.equals(POST_TYPE_LINK) || selected.equals(POST_TYPE_IMGUR)) {
			mTextEditLayout.setHint(getString(R.string.submit_post_url_hint));
			mTextEdit.setInputType(InputType.TYPE_CLASS_TEXT
					| InputType.TYPE_TEXT_VARIATION_URI);
			mTextEdit.setSingleLine(true);
		} else if(selected.equals(POST_TYPE_SELF)) {
			mTextEditLayout.setHint(getString(R.string.submit_post_self_text_hint));
			mTextEdit.setInputType(InputType.TYPE_CLASS_TEXT
					| InputType.TYPE_TEXT_VARIATION_LONG_MESSAGE
					| InputType.TYPE_TEXT_FLAG_MULTI_LINE);
			mTextEdit.setSingleLine(false);
		} else {
			throw new RuntimeException("Unknown selection " + selected.toString());
		}

		if(selected.equals(POST_TYPE_IMGUR)) {

			mTypeSpinner.setSelection(0); // Link

			final FragmentActivity activity = getActivity();

			if(activity == null) {
				return;
			}

			final Intent intent = new Intent(activity, ImgurUploadActivity.class);

			((BaseActivity)activity).startActivityForResultWithCallback(
					intent,
					(resultCode, data) -> {
				if(data != null && data.getData() != null) {
					mTextEdit.setText(data.getData().toString());
				}
			});
		}
	}

	private void requestSubredditDetails() {

		RedditAPI.flairSelectorForNewLink(
				mContext,
				CacheManager.getInstance(mContext),
				mSelectedAccount,
				mSelectedSubreddit,
				new RedditAPI.FlairSelectorResponseHandler() {
					@Override
					public void onSuccess(@NonNull final Collection<RedditFlairChoice> choices) {

						AndroidCommon.runOnUiThread(() -> {

							if(!mActive) {
								return;
							}

							mLoadingSpinnerView.setVisibility(View.GONE);
							mMainControls.setVisibility(View.VISIBLE);

							if(choices.isEmpty()) {
								disableFlairSpinner(R.string.post_submit_flair_none_available);

							} else {
								enableFlairSpinner(choices);
							}
						});
					}

					@Override
					public void onSubredditDoesNotExist() {

						AndroidCommon.runOnUiThread(() -> {

							if(!mActive) {
								return;
							}

							ifActivityNotNull(Listener::onContentFragmentSubredditDoesNotExist);
						});
					}

					@Override
					public void onSubredditPermissionDenied() {

						AndroidCommon.runOnUiThread(() -> {

							if(!mActive) {
								return;
							}

							ifActivityNotNull(Listener::onContentFragmentSubredditPermissionDenied);
						});
					}

					@Override
					public void onFailure(@NonNull final RRError error) {

						AndroidCommon.runOnUiThread(() -> {

							if(!mActive) {
								return;
							}

							ifActivityNotNull(listener -> {
								listener.onContentFragmentFlairRequestError(error);
							});
						});
					}
				});
	}

	@Override
	public void onCreateOptionsMenu(
			@NonNull final Menu menu,
			@NonNull final MenuInflater inflater) {

		final MenuItem send = menu.add(R.string.comment_reply_send);
		send.setIcon(R.drawable.ic_action_send_dark);
		send.setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS);

		menu.add(R.string.comment_reply_preview);
	}

	@Override
	public boolean onOptionsItemSelected(final MenuItem item) {

		if(item.getTitle().equals(getString(R.string.comment_reply_send))) {

			String subreddit = mSelectedSubreddit.getDisplayNameLowercase();
			final String postTitle = mTitleEdit.getText().toString();
			final String text = mTextEdit.getText().toString();

			if(postTitle.isEmpty()) {

				Toast.makeText(mContext, R.string.submit_post_title_empty, Toast.LENGTH_SHORT)
						.show();
				mTitleEdit.requestFocus();

			} else if(text.isEmpty()
					&& getString(R.string.submit_post_url_hint).equals(
							mTextEdit.getHint().toString())) {

				Toast.makeText(mContext, R.string.submit_post_url_empty, Toast.LENGTH_SHORT)
						.show();
				mTextEdit.requestFocus();

			} else {

				final FragmentActivity activity = getActivity();

				if(activity == null) {
					Log.e(TAG, "Activity was null when sending");
					return true;
				}

				//noinspection deprecation
				final ProgressDialog progressDialog = new ProgressDialog(mContext);
				progressDialog.setTitle(getString(R.string.comment_reply_submitting_title));
				//noinspection deprecation
				progressDialog.setMessage(getString(R.string.comment_reply_submitting_message));
				//noinspection deprecation
				progressDialog.setIndeterminate(true);
				progressDialog.setCancelable(true);
				progressDialog.setCanceledOnTouchOutside(false);

				progressDialog.setOnCancelListener(dialogInterface -> {
					General.quickToast(mContext, getString(R.string.comment_reply_oncancel));
					General.safeDismissDialog(progressDialog);
				});

				progressDialog.setOnKeyListener((dialogInterface, keyCode, keyEvent) -> {

					if(keyCode == KeyEvent.KEYCODE_BACK) {
						General.quickToast(mContext, getString(R.string.comment_reply_oncancel));
						General.safeDismissDialog(progressDialog);
					}

					return true;
				});

				final CacheManager cm = CacheManager.getInstance(mContext);

				final APIResponseHandler.SubmitResponseHandler handler
						= new APIResponseHandler.SubmitResponseHandler(
								(AppCompatActivity)activity) {

					@Override
					public void onSubmitErrors(@NonNull final ArrayList<String> errors) {

						final FragmentActivity activity = getActivity();

						if(activity != null) {

							final String errorsJoined = StringUtils.join(errors, " ");

							DialogUtils.showDialog(
									activity,
									activity.getString(R.string.error_post_submit_title),
									errorsJoined);
						}

						General.safeDismissDialog(progressDialog);
					}

					@Override
					public void onSuccess(
							@NonNull final Optional<String> redirectUrl,
							@NonNull final Optional<String> thingId) {

						AndroidCommon.UI_THREAD_HANDLER.post(() -> {
							General.safeDismissDialog(progressDialog);
							General.quickToast(
									mContext,
									getString(R.string.post_submit_done));

							resetDraft();

							final FragmentActivity activity = getActivity();

							if(activity != null) {
								((Listener)activity).onContentFragmentSubmissionSuccess(
										UriString.fromNullable(redirectUrl.orElseNull()));
							}
						});
					}

					@Override
					protected void onCallbackException(final Throwable t) {
						BugReportActivity.handleGlobalError(mContext, t);
					}

					@Override
					protected void onFailure(@NonNull final RRError error) {

						final FragmentActivity activity = getActivity();

						if(activity != null) {
							General.showResultDialog((AppCompatActivity)activity, error);
						}

						General.safeDismissDialog(progressDialog);
					}
				};

				final boolean isSelfPost = mTypeSpinner.getText().toString().equals(POST_TYPE_SELF);

				while(subreddit.startsWith("/")) {
					subreddit = subreddit.substring(1);
				}
				while(subreddit.startsWith("r/")) {
					subreddit = subreddit.substring(2);
				}
				while(subreddit.endsWith("/")) {
					subreddit = subreddit.substring(0, subreddit.length() - 1);
				}

				final boolean sendRepliesToInbox = mSendRepliesToInboxCheckbox.isChecked();
				final boolean markAsNsfw = mMarkAsNsfwCheckbox.isChecked();
				final boolean markAsSpoiler = mMarkAsSpoilerCheckbox.isChecked();

				final String flairId = mFlairIds.get(mFlairSpinner.getText().toString());

				RedditAPI.submit(
						cm,
						handler,
						mSelectedAccount,
						isSelfPost,
						subreddit,
						postTitle,
						text,
						sendRepliesToInbox,
						markAsNsfw,
						markAsSpoiler,
						flairId,
						mContext);

				progressDialog.show();
			}
			return true;

		} else if(item.getTitle().equals(getString(R.string.comment_reply_preview))) {
			MarkdownPreviewDialog.newInstance(mTextEdit.getText().toString())
					.show(getParentFragmentManager(), null);
			return true;

		} else {
			return super.onOptionsItemSelected(item);
		}
	}

	private void resetDraft() {
		mDraftReset = true;
		lastType = null;
		lastTitle = null;
		lastText = null;
		lastInbox = true;
		lastNsfw = false;
		lastSpoiler = false;
	}

	private void ifActivityNotNull(@NonNull final Consumer<Listener> action) {

		final FragmentActivity activity = getActivity();

		if(activity != null) {
			action.consume((Listener)activity);
		}
	}
}
