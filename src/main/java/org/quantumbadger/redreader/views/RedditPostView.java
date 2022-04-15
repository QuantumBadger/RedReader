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

package org.quantumbadger.redreader.views;

import android.content.Context;
import android.content.res.TypedArray;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.Rect;
import android.os.Build;
import android.os.Handler;
import android.os.Looper;
import android.os.Message;
import android.util.Log;
import android.util.TypedValue;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.FrameLayout;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.annotation.UiThread;
import androidx.constraintlayout.widget.ConstraintLayout;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.activities.BaseActivity;
import org.quantumbadger.redreader.cache.CacheManager;
import org.quantumbadger.redreader.cache.CacheRequest;
import org.quantumbadger.redreader.cache.CacheRequestCallbacks;
import org.quantumbadger.redreader.cache.downloadstrategy.DownloadStrategyIfNotCached;
import org.quantumbadger.redreader.common.AndroidCommon;
import org.quantumbadger.redreader.common.Constants;
import org.quantumbadger.redreader.common.DisplayUtils;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.GenericFactory;
import org.quantumbadger.redreader.common.Optional;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.common.Priority;
import org.quantumbadger.redreader.common.RRError;
import org.quantumbadger.redreader.common.SharedPrefsWrapper;
import org.quantumbadger.redreader.common.datastream.SeekableInputStream;
import org.quantumbadger.redreader.fragments.PostListingFragment;
import org.quantumbadger.redreader.http.FailedRequestBody;
import org.quantumbadger.redreader.reddit.prepared.RedditParsedPost;
import org.quantumbadger.redreader.reddit.prepared.RedditPreparedPost;
import org.quantumbadger.redreader.views.liststatus.ErrorView;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Objects;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicInteger;

public final class RedditPostView extends FlingableItemView
		implements RedditPreparedPost.ThumbnailLoadedCallback {

	private static final String TAG = "RedditPostView";

	private static final String PROMPT_PREF_KEY = "inline_image_prompt_accepted";

	private static final AtomicInteger sInlinePreviewsShownThisSession = new AtomicInteger(0);

	private RedditPreparedPost mPost = null;
	private final TextView title;
	private final TextView subtitle;

	@NonNull private final ImageView mThumbnailView;
	@NonNull private final ImageView mOverlayIcon;

	@NonNull private final LinearLayout mOuterView;
	@NonNull private final LinearLayout mInnerView;
	@NonNull private final LinearLayout mCommentsButton;
	@NonNull private final TextView mCommentsText;
	@NonNull private final LinearLayout mPostErrors;
	@NonNull private final FrameLayout mImagePreviewHolder;
	@NonNull private final ImageView mImagePreviewImageView;
	@NonNull private final ConstraintLayout mImagePreviewPlayOverlay;
	@NonNull private final LinearLayout mImagePreviewOuter;
	@NonNull private final LoadingSpinnerView mImagePreviewLoadingSpinner;
	@NonNull private final LinearLayout mFooter;

	private int mUsageId = 0;

	private final Handler thumbnailHandler;

	private final BaseActivity mActivity;

	private final PrefsUtility.PostFlingAction mLeftFlingPref;
	private final PrefsUtility.PostFlingAction mRightFlingPref;
	private ActionDescriptionPair mLeftFlingAction;
	private ActionDescriptionPair mRightFlingAction;

	private final boolean mCommentsButtonPref;

	private final int
			rrPostTitleReadCol;
	private final int rrPostTitleCol;
	private final int rrListItemBackgroundCol;
	private final int rrPostCommentsButtonBackCol;

	private final int mThumbnailSizePrefPixels;

	@Override
	protected void onSetItemFlingPosition(final float position) {
		mOuterView.setTranslationX(position);
	}

	@NonNull
	@Override
	protected String getFlingLeftText() {

		mLeftFlingAction = chooseFlingAction(mLeftFlingPref);

		if(mLeftFlingAction != null) {
			return mActivity.getString(mLeftFlingAction.descriptionRes);
		} else {
			return "Disabled";
		}
	}

	@NonNull
	@Override
	protected String getFlingRightText() {

		mRightFlingAction = chooseFlingAction(mRightFlingPref);

		if(mRightFlingAction != null) {
			return mActivity.getString(mRightFlingAction.descriptionRes);
		} else {
			return "Disabled";
		}
	}

	@Override
	protected boolean allowFlingingLeft() {
		return mLeftFlingAction != null;
	}

	@Override
	protected boolean allowFlingingRight() {
		return mRightFlingAction != null;
	}

	@Override
	protected void onFlungLeft() {
		RedditPreparedPost.onActionMenuItemSelected(
				mPost,
				mActivity,
				mLeftFlingAction.action);
	}

	@Override
	protected void onFlungRight() {
		RedditPreparedPost.onActionMenuItemSelected(
				mPost,
				mActivity,
				mRightFlingAction.action);
	}

	private static final class ActionDescriptionPair {
		public final RedditPreparedPost.Action action;
		public final int descriptionRes;

		private ActionDescriptionPair(
				final RedditPreparedPost.Action action,
				final int descriptionRes) {
			this.action = action;
			this.descriptionRes = descriptionRes;
		}
	}

	private ActionDescriptionPair chooseFlingAction(final PrefsUtility.PostFlingAction pref) {

		switch(pref) {

			case UPVOTE:
				if(mPost.isUpvoted()) {
					return new ActionDescriptionPair(
							RedditPreparedPost.Action.UNVOTE,
							R.string.action_vote_remove);
				} else {
					return new ActionDescriptionPair(
							RedditPreparedPost.Action.UPVOTE,
							R.string.action_upvote);
				}

			case DOWNVOTE:
				if(mPost.isDownvoted()) {
					return new ActionDescriptionPair(
							RedditPreparedPost.Action.UNVOTE,
							R.string.action_vote_remove);
				} else {
					return new ActionDescriptionPair(
							RedditPreparedPost.Action.DOWNVOTE,
							R.string.action_downvote);
				}

			case SAVE:
				if(mPost.isSaved()) {
					return new ActionDescriptionPair(
							RedditPreparedPost.Action.UNSAVE,
							R.string.action_unsave);
				} else {
					return new ActionDescriptionPair(
							RedditPreparedPost.Action.SAVE,
							R.string.action_save);
				}

			case HIDE:
				if(mPost.isHidden()) {
					return new ActionDescriptionPair(
							RedditPreparedPost.Action.UNHIDE,
							R.string.action_unhide);
				} else {
					return new ActionDescriptionPair(
							RedditPreparedPost.Action.HIDE,
							R.string.action_hide);
				}

			case COMMENTS:
				return new ActionDescriptionPair(
						RedditPreparedPost.Action.COMMENTS,
						R.string.action_comments_short);

			case LINK:
				return new ActionDescriptionPair(
						RedditPreparedPost.Action.LINK,
						R.string.action_link_short);

			case BROWSER:
				return new ActionDescriptionPair(
						RedditPreparedPost.Action.EXTERNAL,
						R.string.action_external_short);

			case REPORT:
				return new ActionDescriptionPair(
						RedditPreparedPost.Action.REPORT,
						R.string.action_report
				);

			case SAVE_IMAGE:
				return new ActionDescriptionPair(
						RedditPreparedPost.Action.SAVE_IMAGE,
						R.string.action_save_image
				);

			case GOTO_SUBREDDIT:
				return new ActionDescriptionPair(
						RedditPreparedPost.Action.GOTO_SUBREDDIT,
						R.string.action_gotosubreddit
				);

			case SHARE:
				return new ActionDescriptionPair(
						RedditPreparedPost.Action.SHARE,
						R.string.action_share
				);

			case SHARE_COMMENTS:
				return new ActionDescriptionPair(
						RedditPreparedPost.Action.SHARE_COMMENTS,
						R.string.action_share_comments
				);

			case SHARE_IMAGE:
				return new ActionDescriptionPair(
						RedditPreparedPost.Action.SHARE_IMAGE,
						R.string.action_share_image
				);

			case COPY:
				return new ActionDescriptionPair(
						RedditPreparedPost.Action.COPY,
						R.string.action_copy_link
				);

			case USER_PROFILE:
				return new ActionDescriptionPair(
						RedditPreparedPost.Action.USER_PROFILE,
						R.string.action_user_profile_short
				);

			case PROPERTIES:
				return new ActionDescriptionPair(
						RedditPreparedPost.Action.PROPERTIES,
						R.string.action_properties
				);

			case ACTION_MENU:
				return new ActionDescriptionPair(
						RedditPreparedPost.Action.ACTION_MENU,
						R.string.action_actionmenu_short);

			case BACK:
				return new ActionDescriptionPair(
						RedditPreparedPost.Action.BACK,
						R.string.action_back);
		}

		return null;
	}

	public RedditPostView(
			final Context context,
			final PostListingFragment fragmentParent,
			final BaseActivity activity,
			final boolean leftHandedMode) {

		super(context);
		mActivity = activity;

		thumbnailHandler = new Handler(Looper.getMainLooper()) {
			@Override
			public void handleMessage(@NonNull final Message msg) {
				if(mUsageId != msg.what) {
					return;
				}
				mThumbnailView.setImageBitmap((Bitmap)msg.obj);
			}
		};

		final float dpScale = context.getResources().getDisplayMetrics().density;

		final float titleFontScale = PrefsUtility.appearance_fontscale_posts();
		final float subtitleFontScale = PrefsUtility.appearance_fontscale_post_subtitles();

		final View rootView =
				LayoutInflater.from(context).inflate(R.layout.reddit_post, this, true);

		mOuterView = Objects.requireNonNull(rootView.findViewById(R.id.reddit_post_layout_outer));
		mInnerView = Objects.requireNonNull(rootView.findViewById(R.id.reddit_post_layout_inner));

		mPostErrors = Objects.requireNonNull(rootView.findViewById(R.id.reddit_post_errors));

		mImagePreviewHolder = Objects.requireNonNull(
				rootView.findViewById(R.id.reddit_post_image_preview_holder));

		mImagePreviewImageView = Objects.requireNonNull(
				rootView.findViewById(R.id.reddit_post_image_preview_imageview));

		mImagePreviewPlayOverlay = Objects.requireNonNull(
				rootView.findViewById(R.id.reddit_post_image_preview_play_overlay));

		mImagePreviewOuter = Objects.requireNonNull(
				rootView.findViewById(R.id.reddit_post_image_preview_outer));

		mFooter = Objects.requireNonNull(
				rootView.findViewById(R.id.reddit_post_footer));

		mImagePreviewLoadingSpinner = new LoadingSpinnerView(activity);
		mImagePreviewHolder.addView(mImagePreviewLoadingSpinner);

		mOuterView.setOnClickListener(v -> fragmentParent.onPostSelected(mPost));

		mOuterView.setOnLongClickListener(v -> {
			RedditPreparedPost.showActionMenu(mActivity, mPost);
			return true;
		});

		mThumbnailView = Objects.requireNonNull(
				rootView.findViewById(R.id.reddit_post_thumbnail_view));

		mOverlayIcon = Objects.requireNonNull(
				rootView.findViewById(R.id.reddit_post_overlay_icon));

		title = Objects.requireNonNull(rootView.findViewById(R.id.reddit_post_title));
		subtitle = Objects.requireNonNull(rootView.findViewById(R.id.reddit_post_subtitle));

		mCommentsButtonPref =
				PrefsUtility.appearance_post_show_comments_button();

		mCommentsButton = rootView.findViewById(R.id.reddit_post_comments_button);
		mCommentsText = mCommentsButton.findViewById(R.id.reddit_post_comments_text);

		if(!mCommentsButtonPref) {
			mInnerView.removeView(mCommentsButton);
		}

		if(leftHandedMode) {
			final ArrayList<View> innerViewElements = new ArrayList<>(3);
			for(int i = mInnerView.getChildCount() - 1; i >= 0; i--) {
				innerViewElements.add(mInnerView.getChildAt(i));
				mInnerView.removeViewAt(i);
			}

			for(int i = 0; i < innerViewElements.size(); i++) {
				mInnerView.addView(innerViewElements.get(i));
			}

			mInnerView.setNextFocusRightId(NO_ID);
			if (mCommentsButtonPref) {
				mInnerView.setNextFocusLeftId(mCommentsButton.getId());

				mCommentsButton.setNextFocusForwardId(R.id.reddit_post_layout_outer);
				mCommentsButton.setNextFocusRightId(R.id.reddit_post_layout_outer);
				mCommentsButton.setNextFocusLeftId(NO_ID);
			}
		}

		if(mCommentsButtonPref) {
			mCommentsButton.setOnClickListener(v -> fragmentParent.onPostCommentsSelected(mPost));
		}

		final boolean postTitleOpensPost = PrefsUtility.pref_behaviour_post_title_opens_comments();

		if(postTitleOpensPost) {
			title.setOnClickListener(v -> fragmentParent.onPostCommentsSelected(mPost));
		}

		title.setTextSize(
				TypedValue.COMPLEX_UNIT_PX,
				title.getTextSize() * titleFontScale);
		subtitle.setTextSize(
				TypedValue.COMPLEX_UNIT_PX,
				subtitle.getTextSize() * subtitleFontScale);

		mLeftFlingPref =
				PrefsUtility.pref_behaviour_fling_post_left();
		mRightFlingPref =
				PrefsUtility.pref_behaviour_fling_post_right();

		{
			final TypedArray attr = context.obtainStyledAttributes(new int[] {
					R.attr.rrPostTitleCol,
					R.attr.rrPostTitleReadCol,
					R.attr.rrListItemBackgroundCol,
					R.attr.rrPostCommentsButtonBackCol
			});

			rrPostTitleCol = attr.getColor(0, 0);
			rrPostTitleReadCol = attr.getColor(1, 0);
			rrListItemBackgroundCol = attr.getColor(2, 0);
			rrPostCommentsButtonBackCol = attr.getColor(3, 0);
			attr.recycle();
		}

		mThumbnailSizePrefPixels = (int)(dpScale * PrefsUtility.images_thumbnail_size_dp());
	}

	@UiThread
	public void reset(@NonNull final RedditPreparedPost newPost) {

		if(newPost != mPost) {

			mThumbnailView.setImageBitmap(null);
			mImagePreviewImageView.setImageBitmap(null);
			mImagePreviewPlayOverlay.setVisibility(GONE);
			mPostErrors.removeAllViews();
			mFooter.removeAllViews();

			mUsageId++;

			resetSwipeState();

			title.setText(newPost.src.getTitle());
			if(mCommentsButtonPref) {
				mCommentsText.setText(String.valueOf(newPost.src.getSrc().num_comments));
			}

			final boolean showInlinePreview = newPost.shouldShowInlinePreview();

			final boolean showThumbnail = !showInlinePreview && newPost.hasThumbnail;

			if(showInlinePreview) {
				downloadInlinePreview(newPost, mUsageId);

			} else {
				mImagePreviewLoadingSpinner.setVisibility(GONE);
				mImagePreviewOuter.setVisibility(GONE);
				setBottomMargin(false);
			}

			if(showThumbnail) {

				final Bitmap thumbnail = newPost.getThumbnail(this, mUsageId);
				mThumbnailView.setImageBitmap(thumbnail);

				mThumbnailView.setVisibility(VISIBLE);
				mThumbnailView.setMinimumWidth(mThumbnailSizePrefPixels);

				General.setLayoutWidthHeight(
						mThumbnailView,
						ViewGroup.LayoutParams.WRAP_CONTENT,
						ViewGroup.LayoutParams.MATCH_PARENT);

				mInnerView.setMinimumHeight(mThumbnailSizePrefPixels);

			} else {
				mThumbnailView.setMinimumWidth(0);
				mThumbnailView.setVisibility(GONE);
				mInnerView.setMinimumHeight(General.dpToPixels(mActivity, 64));
			}
		}

		if(mPost != null) {
			mPost.unbind(this);
		}

		newPost.bind(this);

		mPost = newPost;

		updateAppearance();
	}

	public void updateAppearance() {

		if(Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP) {

			mOuterView.setBackgroundResource(
					R.drawable.rr_postlist_item_selector_main);

			if(mCommentsButtonPref) {
				mCommentsButton.setBackgroundResource(
						R.drawable.rr_postlist_commentbutton_selector_main);
			}

		} else {
			// On KitKat and lower, we can't do easily themed highlighting
			mOuterView.setBackgroundColor(rrListItemBackgroundCol);
			if(mCommentsButtonPref) {
				mCommentsButton.setBackgroundColor(rrPostCommentsButtonBackCol);
			}
		}

		if(mPost.isRead()) {
			title.setTextColor(rrPostTitleReadCol);
		} else {
			title.setTextColor(rrPostTitleCol);
		}

		title.setContentDescription(mPost.buildAccessibilityTitle(mActivity, false));

		subtitle.setText(mPost.buildSubtitle(mActivity, false));
		subtitle.setContentDescription(mPost.buildAccessibilitySubtitle(mActivity, false));

		boolean overlayVisible = true;

		if(mPost.isSaved()) {
			mOverlayIcon.setImageResource(R.drawable.star_dark);

		} else if(mPost.isHidden()) {
			mOverlayIcon.setImageResource(R.drawable.ic_action_cross_dark);

		} else if(mPost.isUpvoted()) {
			mOverlayIcon.setImageResource(R.drawable.arrow_up_bold_orangered);

		} else if(mPost.isDownvoted()) {
			mOverlayIcon.setImageResource(R.drawable.arrow_down_bold_periwinkle);

		} else {
			overlayVisible = false;
		}

		if(overlayVisible) {
			mOverlayIcon.setVisibility(VISIBLE);
		} else {
			mOverlayIcon.setVisibility(GONE);
		}
	}

	@Override
	public void betterThumbnailAvailable(
			final Bitmap thumbnail,
			final int callbackUsageId) {
		final Message msg = Message.obtain();
		msg.obj = thumbnail;
		msg.what = callbackUsageId;
		thumbnailHandler.sendMessage(msg);
	}

	public interface PostSelectionListener {
		void onPostSelected(RedditPreparedPost post);

		void onPostCommentsSelected(RedditPreparedPost post);
	}

	private void setBottomMargin(final boolean enabled) {

		final MarginLayoutParams layoutParams
				= (MarginLayoutParams)mOuterView.getLayoutParams();

		if(enabled) {
			layoutParams.bottomMargin = General.dpToPixels(mActivity, 6);
		} else {
			layoutParams.bottomMargin = 0;
		}

		mOuterView.setLayoutParams(layoutParams);
	}

	private void downloadInlinePreview(
			@NonNull final RedditPreparedPost post,
			final int usageId) {

		final Rect windowVisibleDisplayFrame
				= DisplayUtils.getWindowVisibleDisplayFrame(mActivity);

		final int screenWidth = Math.min(1080, Math.max(720, windowVisibleDisplayFrame.width()));
		final int screenHeight = Math.min(2000, Math.max(400, windowVisibleDisplayFrame.height()));

		final RedditParsedPost.ImagePreviewDetails preview
				= post.src.getPreview(screenWidth, 0);

		if(preview == null || preview.width < 10 || preview.height < 10) {
			mImagePreviewOuter.setVisibility(GONE);
			mImagePreviewLoadingSpinner.setVisibility(GONE);
			setBottomMargin(false);
			return;
		}

		final int boundedImageHeight = Math.min(
				(screenHeight * 2) / 3,
				(int)(((long)preview.height * screenWidth) / preview.width));

		final ConstraintLayout.LayoutParams imagePreviewLayoutParams
				= (ConstraintLayout.LayoutParams)mImagePreviewHolder.getLayoutParams();

		imagePreviewLayoutParams.dimensionRatio = screenWidth + ":" + boundedImageHeight;
		mImagePreviewHolder.setLayoutParams(imagePreviewLayoutParams);

		mImagePreviewOuter.setVisibility(VISIBLE);
		mImagePreviewLoadingSpinner.setVisibility(VISIBLE);
		setBottomMargin(true);

		CacheManager.getInstance(mActivity).makeRequest(new CacheRequest(
				General.uriFromString(preview.url),
				RedditAccountManager.getAnon(),
				null,
				new Priority(Constants.Priority.INLINE_IMAGE_PREVIEW),
				DownloadStrategyIfNotCached.INSTANCE,
				Constants.FileType.INLINE_IMAGE_PREVIEW,
				CacheRequest.DOWNLOAD_QUEUE_IMMEDIATE,
				mActivity,
				new CacheRequestCallbacks() {
					@Override
					public void onDataStreamComplete(
							@NonNull final GenericFactory<SeekableInputStream, IOException> stream,
							final long timestamp,
							@NonNull final UUID session,
							final boolean fromCache,
							@Nullable final String mimetype) {

						if(usageId != mUsageId) {
							return;
						}

						try(InputStream is = stream.create()) {

							final Bitmap data = BitmapFactory.decodeStream(is);

							if(data == null) {
								throw new IOException("Failed to decode bitmap");
							}

							// Avoid a crash on badly behaving Android ROMs (where the ImageView
							// crashes if an image is too big)
							// Should never happen as we limit the preview size to 3000x3000
							if(data.getByteCount() > 50 * 1024 * 1024) {
								throw new RuntimeException("Image was too large: "
										+ data.getByteCount()
										+ ", preview URL was "
										+ preview.url
										+ " and post was "
										+ post.src.getIdAndType());
							}

							final boolean alreadyAcceptedPrompt = General.getSharedPrefs(mActivity)
									.getBoolean(PROMPT_PREF_KEY, false);

							final int totalPreviewsShown
									= sInlinePreviewsShownThisSession.incrementAndGet();

							final boolean isVideoPreview = post.isVideoPreview();

							AndroidCommon.runOnUiThread(() -> {
								mImagePreviewImageView.setImageBitmap(data);
								mImagePreviewLoadingSpinner.setVisibility(GONE);

								if(isVideoPreview) {
									mImagePreviewPlayOverlay.setVisibility(VISIBLE);
								}

								// Show every 8 previews, starting at the second one
								if(totalPreviewsShown % 8 == 2 && !alreadyAcceptedPrompt) {
									showPrefPrompt();
								}
							});

						} catch(final Throwable t) {
							onFailure(
									CacheRequest.REQUEST_FAILURE_CONNECTION,
									t,
									null,
									"Exception while downloading thumbnail",
									Optional.empty());
						}
					}

					@Override
					public void onFailure(
							final int type,
							@Nullable final Throwable t,
							@Nullable final Integer httpStatus,
							@Nullable final String readableMessage,
							@NonNull final Optional<FailedRequestBody> body) {

						Log.e(TAG, "Failed to download image preview", t);

						if(usageId != mUsageId) {
							return;
						}

						AndroidCommon.runOnUiThread(() -> {

							final Context context = mActivity.getApplicationContext();

							mImagePreviewLoadingSpinner.setVisibility(GONE);
							mImagePreviewOuter.setVisibility(GONE);

							final ErrorView errorView = new ErrorView(
									mActivity,
									new RRError(
											context.getString(
													R.string.error_inline_preview_failed_title),
											context.getString(
													R.string.error_inline_preview_failed_message),
											true,
											t,
											httpStatus,
											preview.url,
											null,
											body));

							mPostErrors.addView(errorView);
							General.setLayoutMatchWidthWrapHeight(errorView);
						});
					}
				}
		));
	}

	private void showPrefPrompt() {

		final SharedPrefsWrapper sharedPrefs
				= General.getSharedPrefs(mActivity);

		LayoutInflater.from(mActivity).inflate(
				R.layout.inline_images_question_view,
				mFooter,
				true);

		final FrameLayout promptView
				= mFooter.findViewById(R.id.inline_images_prompt_root);

		final Button keepShowing
				= mFooter.findViewById(R.id.inline_preview_prompt_keep_showing_button);

		final Button turnOff
				= mFooter.findViewById(R.id.inline_preview_prompt_turn_off_button);

		keepShowing.setOnClickListener(v -> {

			new RRAnimationShrinkHeight(promptView).start();

			sharedPrefs.edit()
					.putBoolean(PROMPT_PREF_KEY, true)
					.apply();
		});

		turnOff.setOnClickListener(v -> {

			final String prefPreview = mActivity.getApplicationContext()
					.getString(
							R.string.pref_images_inline_image_previews_key);

			sharedPrefs.edit()
					.putBoolean(PROMPT_PREF_KEY, true)
					.putString(prefPreview, "never")
					.apply();
		});
	}
}
