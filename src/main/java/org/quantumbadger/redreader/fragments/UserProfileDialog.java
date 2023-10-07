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

import android.content.Intent;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.util.Log;
import android.view.View;
import android.widget.FrameLayout;
import android.widget.ImageView;
import android.widget.ScrollView;
import androidx.annotation.NonNull;
import androidx.appcompat.app.AlertDialog;
import androidx.appcompat.app.AppCompatActivity;
import androidx.appcompat.widget.AppCompatImageView;
import com.google.android.material.card.MaterialCardView;
import com.google.android.material.chip.Chip;
import com.google.android.material.dialog.MaterialAlertDialogBuilder;
import com.google.android.material.textview.MaterialTextView;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.activities.BugReportActivity;
import org.quantumbadger.redreader.activities.PMSendActivity;
import org.quantumbadger.redreader.cache.CacheManager;
import org.quantumbadger.redreader.cache.CacheRequest;
import org.quantumbadger.redreader.cache.CacheRequestCallbacks;
import org.quantumbadger.redreader.cache.downloadstrategy.DownloadStrategyAlways;
import org.quantumbadger.redreader.cache.downloadstrategy.DownloadStrategyIfNotCached;
import org.quantumbadger.redreader.common.AndroidCommon;
import org.quantumbadger.redreader.common.Constants;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.GenericFactory;
import org.quantumbadger.redreader.common.LinkHandler;
import org.quantumbadger.redreader.common.Optional;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.common.Priority;
import org.quantumbadger.redreader.common.RRError;
import org.quantumbadger.redreader.common.StringUtils;
import org.quantumbadger.redreader.common.datastream.SeekableInputStream;
import org.quantumbadger.redreader.common.time.TimeFormatHelper;
import org.quantumbadger.redreader.common.time.TimestampUTC;
import org.quantumbadger.redreader.reddit.APIResponseHandler;
import org.quantumbadger.redreader.reddit.RedditAPI;
import org.quantumbadger.redreader.reddit.things.RedditUser;
import org.quantumbadger.redreader.reddit.url.UserPostListingURL;
import org.quantumbadger.redreader.views.LoadingSpinnerView;
import org.quantumbadger.redreader.views.liststatus.ErrorView;

import java.io.IOException;
import java.io.InputStream;
import java.net.URISyntaxException;
import java.text.NumberFormat;
import java.util.Objects;
import java.util.UUID;

public class UserProfileDialog {

	public static void show(
			@NonNull final AppCompatActivity activity,
			@NonNull final String username) {

		final MaterialAlertDialogBuilder builder = new MaterialAlertDialogBuilder(activity);

		builder.setView(R.layout.user_profile_dialog);

		final AlertDialog dialog = builder.show();

		final LoadingSpinnerView loadingView = Objects.requireNonNull(
				dialog.findViewById(R.id.user_profile_loading));

		final ScrollView scrollView = Objects.requireNonNull(
				dialog.findViewById(R.id.user_profile_scrollview));

		final MaterialTextView textviewUsername = Objects.requireNonNull(
				dialog.findViewById(R.id.user_profile_name));

		final MaterialTextView textviewAccountAge = Objects.requireNonNull(
				dialog.findViewById(R.id.user_profile_account_age));

		final Chip chipYou = Objects.requireNonNull(
				dialog.findViewById(R.id.user_profile_chip_you));

		final Chip chipFriend = Objects.requireNonNull(
				dialog.findViewById(R.id.user_profile_chip_friend));

		final Chip chipAdmin = Objects.requireNonNull(
				dialog.findViewById(R.id.user_profile_chip_admin));

		final Chip chipMod = Objects.requireNonNull(
				dialog.findViewById(R.id.user_profile_chip_moderator));

		final Chip chipGold = Objects.requireNonNull(
				dialog.findViewById(R.id.user_profile_chip_gold));

		final MaterialCardView postsCard = Objects.requireNonNull(
				dialog.findViewById(R.id.user_profile_posts));

		final MaterialCardView commentsCard = Objects.requireNonNull(
				dialog.findViewById(R.id.user_profile_comments));

		final MaterialCardView messageCard = Objects.requireNonNull(
				dialog.findViewById(R.id.user_profile_send_message));

		final MaterialTextView postsKarma = Objects.requireNonNull(
				dialog.findViewById(R.id.user_profile_posts_karma));

		final MaterialTextView commentsKarma = Objects.requireNonNull(
				dialog.findViewById(R.id.user_profile_comments_karma));

		final Chip chipMoreInfo = Objects.requireNonNull(
				dialog.findViewById(R.id.user_profile_chip_more_info));

		final CacheManager cm = CacheManager.getInstance(activity);

		final RedditAccountManager accountManager = RedditAccountManager.getInstance(activity);

		RedditAPI.getUser(
				cm,
				username,
				new APIResponseHandler.UserResponseHandler(activity) {
					@Override
					protected void onDownloadStarted() {
					}

					@Override
					protected void onSuccess(final RedditUser user, final TimestampUTC timestamp) {
						AndroidCommon.UI_THREAD_HANDLER.post(() -> {

							if (!dialog.isShowing()) {
								return;
							}

							loadingView.setVisibility(View.GONE);
							scrollView.setVisibility(View.VISIBLE);

							textviewUsername.setText(user.name);

							textviewAccountAge.setText(TimeFormatHelper.format(
									TimestampUTC.now().elapsedPeriodSince(
											TimestampUTC.fromUtcSecs(user.created_utc)),
									activity,
									R.string.user_profile_account_age,
									1));

							if (!StringUtils.asciiLowercase(user.name).equals(
									StringUtils.asciiLowercase(accountManager
											.getDefaultAccount().getCanonicalUsername()))) {
								chipYou.setVisibility(View.GONE);
							}

							if (!user.is_friend) {
								chipFriend.setVisibility(View.GONE);
							}

							if (!user.is_employee) {
								chipAdmin.setVisibility(View.GONE);
							}

							if (!user.is_mod) {
								chipMod.setVisibility(View.GONE);
							}

							if (!user.is_gold) {
								chipGold.setVisibility(View.GONE);
							}

							if (PrefsUtility.appearance_user_show_avatars()) {
								final String iconUrl = user.getIconUrl();

								if (iconUrl != null && !iconUrl.isEmpty()) {

									final AppCompatImageView avatarView
											= dialog.findViewById(R.id.avatar_image);

									final View avatarViewHolder
											= dialog.findViewById(R.id.avatar_image_holder);

									avatarViewHolder.setVisibility(View.VISIBLE);

									try {
										assignUserAvatar(iconUrl, avatarView, context);
									} catch (final URISyntaxException e) {
										Log.e("UserProfileDialog", "Error decoding uri", e);
									}
								}
							}

							postsKarma.setText(
									NumberFormat.getNumberInstance().format(user.link_karma));

							commentsKarma.setText(
									NumberFormat.getNumberInstance().format(user.comment_karma));

							postsKarma.setContentDescription(activity.getString(
									R.string.userprofile_accessibility_karma,
									user.link_karma
							));

							commentsKarma.setContentDescription(activity.getString(
									R.string.userprofile_accessibility_karma,
									user.comment_karma
							));

							postsCard.setOnClickListener(v -> LinkHandler.onLinkClicked(
									context,
									UserPostListingURL.getSubmitted(username)
											.generateJsonUri()
											.toString(),
									false));

							commentsCard.setOnClickListener(v -> LinkHandler.onLinkClicked(
									context,
									Constants.Reddit.getUri(
											"/user/" + username + "/comments.json").toString(),
									false));

							if (!RedditAccountManager.getInstance(context)
									.getDefaultAccount()
									.isAnonymous()) {
								messageCard.setOnClickListener(v -> {
									final Intent intent = new Intent(context, PMSendActivity.class);
									intent.putExtra(PMSendActivity.EXTRA_RECIPIENT, username);
									activity.startActivity(intent);
								});
							} else {
								messageCard.setVisibility(View.GONE);
							}

							chipMoreInfo.setOnClickListener(
									v -> UserPropertiesDialog.newInstance(user)
											.show(activity.getSupportFragmentManager(), null));
						});
					}

					@Override
					protected void onCallbackException(final Throwable t) {
						BugReportActivity.handleGlobalError(context, t);
					}

					@Override
					protected void onFailure(@NonNull final RRError error) {

						AndroidCommon.UI_THREAD_HANDLER.post(() -> {

							if (!dialog.isShowing()) {
								return;
							}

							final FrameLayout root = Objects.requireNonNull(
									dialog.findViewById(R.id.user_profile_root));

							root.removeAllViews();
							root.addView(new ErrorView(context, error));
						});
					}
				},
				accountManager.getDefaultAccount(),
				DownloadStrategyAlways.INSTANCE,
				activity);
	}

	private static void assignUserAvatar(
			final String url,
			final ImageView imageOutput,
			final AppCompatActivity context)
			throws URISyntaxException {
		CacheManager.getInstance(context).makeRequest(new CacheRequest(
				General.uriFromString(url),
				RedditAccountManager.getAnon(),
				null,
				new Priority(Constants.Priority.INLINE_IMAGE_PREVIEW),
				DownloadStrategyIfNotCached.INSTANCE,
				Constants.FileType.INLINE_IMAGE_PREVIEW,
				CacheRequest.DOWNLOAD_QUEUE_IMMEDIATE,
				context,
				new CacheRequestCallbacks() {
					@Override
					public void onDataStreamComplete(
							final GenericFactory<SeekableInputStream, IOException> streamFactory,
							final TimestampUTC timestamp,
							final UUID session,
							final boolean fromCache,
							final String mimetype) {
						try (InputStream is = streamFactory.create()) {
							final Bitmap data = BitmapFactory.decodeStream(is);
							if (data == null) {
								throw new IOException("Failed to decode bitmap");
							}
							AndroidCommon.runOnUiThread(() -> {
								imageOutput.setImageBitmap(data);
								imageOutput.setOnClickListener(
										v -> LinkHandler.onLinkClicked(context, url));
							});

						} catch (final Throwable t) {
							onFailure(General.getGeneralErrorForFailure(
									context,
									CacheRequest.REQUEST_FAILURE_CONNECTION,
									t,
									null,
									url,
									Optional.empty()));
						}
					}

					@Override
					public void onFailure(@NonNull final RRError error) {
						Log.d(
								"UserProfileDialog",
								"Failed to download user avatar: " + error);
					}
				}
		));
	}
}
