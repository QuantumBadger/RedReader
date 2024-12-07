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
import android.os.Handler;
import android.os.Looper;
import android.os.Message;
import android.util.TypedValue;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.FrameLayout;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.UiThread;
import androidx.constraintlayout.widget.ConstraintLayout;

import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.activities.BaseActivity;
import org.quantumbadger.redreader.common.AndroidCommon;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.fragments.PostListingFragment;
import org.quantumbadger.redreader.reddit.api.RedditPostActions;
import org.quantumbadger.redreader.reddit.prepared.RedditPreparedPost;
import org.quantumbadger.redreader.common.ImagePreviewUtils;
import org.quantumbadger.redreader.views.liststatus.ErrorView;

import java.util.ArrayList;
import java.util.Objects;

public final class RedditPostView extends FlingableItemView
		implements RedditPreparedPost.ThumbnailLoadedCallback {

	private final AccessibilityActionManager mAccessibilityActionManager;

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
	private RedditPostActions.ActionDescriptionPair mLeftFlingAction;
	private RedditPostActions.ActionDescriptionPair mRightFlingAction;

	private final boolean mCommentsButtonPref;

	private final int
			rrPostTitleReadCol;
	private final int rrPostTitleCol;

	private final int mThumbnailSizePrefPixels;

	@Override
	protected void onSetItemFlingPosition(final float position) {
		mOuterView.setTranslationX(position);
	}

	@NonNull
	@Override
	protected String getFlingLeftText() {

		mLeftFlingAction = RedditPostActions.ActionDescriptionPair.from(mPost, mLeftFlingPref);

		if(mLeftFlingAction != null) {
			return mActivity.getString(mLeftFlingAction.getDescriptionRes());
		} else {
			return "Disabled";
		}
	}

	@NonNull
	@Override
	protected String getFlingRightText() {

		mRightFlingAction = RedditPostActions.ActionDescriptionPair.from(mPost, mRightFlingPref);

		if(mRightFlingAction != null) {
			return mActivity.getString(mRightFlingAction.getDescriptionRes());
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
		RedditPostActions.INSTANCE.onActionMenuItemSelected(
				mPost,
				mActivity,
				mLeftFlingAction.getAction());
	}

	@Override
	protected void onFlungRight() {
		RedditPostActions.INSTANCE.onActionMenuItemSelected(
				mPost,
				mActivity,
				mRightFlingAction.getAction());
	}



	public RedditPostView(
			final Context context,
			final PostListingFragment fragmentParent,
			final BaseActivity activity,
			final boolean leftHandedMode) {

		super(context);
		mActivity = activity;

		mAccessibilityActionManager = new AccessibilityActionManager(
				this,
				context.getResources());

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

		final String layoutMode = PrefsUtility.getLayoutMode();

		int layout = R.layout.reddit_post;

		if (layoutMode.equals("card")) {
			layout = R.layout.reddit_post_card;
		}

		final View rootView = LayoutInflater.from(context).inflate(layout, this, true);

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

		final OnLongClickListener longClickListener = v -> {
			RedditPostActions.INSTANCE.showActionMenu(mActivity, mPost);
			return true;
		};

		switch(PrefsUtility.pref_behaviour_post_tap_action()) {
			case LINK:
				mOuterView.setOnClickListener(v -> fragmentParent.onPostSelected(mPost));
				AndroidCommon.removeClickListeners(mThumbnailView);
				AndroidCommon.removeClickListeners(mImagePreviewOuter);
				AndroidCommon.removeClickListeners(title);
				break;

			case COMMENTS:
				mOuterView.setOnClickListener(v -> fragmentParent.onPostCommentsSelected(mPost));

				mThumbnailView.setOnClickListener(v -> fragmentParent.onPostSelected(mPost));
				mThumbnailView.setOnLongClickListener(longClickListener);

				mImagePreviewOuter.setOnClickListener(v -> fragmentParent.onPostSelected(mPost));
				mImagePreviewOuter.setOnLongClickListener(longClickListener);

				AndroidCommon.removeClickListeners(title);

				break;

			case TITLE_COMMENTS:
				mOuterView.setOnClickListener(v -> fragmentParent.onPostSelected(mPost));

				AndroidCommon.removeClickListeners(mThumbnailView);
				AndroidCommon.removeClickListeners(mImagePreviewOuter);

				title.setOnClickListener(v -> fragmentParent.onPostCommentsSelected(mPost));
				title.setOnLongClickListener(longClickListener);
				break;
		}

		mOuterView.setOnLongClickListener(longClickListener);

		if(mCommentsButtonPref) {
			mCommentsButton.setOnClickListener(v -> fragmentParent.onPostCommentsSelected(mPost));
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
			});

			rrPostTitleCol = attr.getColor(0, 0);
			rrPostTitleReadCol = attr.getColor(1, 0);
			attr.recycle();
		}

		mThumbnailSizePrefPixels = (int)(dpScale * PrefsUtility.images_thumbnail_size_dp());
	}

	@UiThread
	public void reset(@NonNull final RedditPreparedPost newPost) {
		final String layoutMode = PrefsUtility.getLayoutMode();

		boolean alwaysPreviewMode = false;

		switch(layoutMode) {
			case "always_preview":
				alwaysPreviewMode = true;
				break;
			case "card":
				alwaysPreviewMode = true;
				break;
			default:
				alwaysPreviewMode = false;
				break;
		}

		final boolean showInlinePreview = alwaysPreviewMode ||
			newPost.shouldShowInlinePreview();

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
				mCommentsText.setText(String.valueOf(newPost.src.getSrc().getNum_comments()));
			}

			final boolean showThumbnail = !showInlinePreview && newPost.hasThumbnail;

			if(!showInlinePreview) {
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

		if(showInlinePreview) {
			ImagePreviewUtils.downloadInlinePreview(mActivity, mPost, previewListener, mUsageId);
		}

		updateAppearance();
	}

	public void updateAppearance() {

		mOuterView.setBackgroundResource(
				R.drawable.rr_postlist_item_selector_main);

		if(mCommentsButtonPref) {
			mCommentsButton.setBackgroundResource(
					R.drawable.rr_postlist_commentbutton_selector_main);
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

		RedditPostActions.INSTANCE.setupAccessibilityActions(
				mAccessibilityActionManager,
				mPost,
				mActivity,
				false);
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

	private final ImagePreviewUtils.ImagePreviewListener previewListener =
		new ImagePreviewUtils.ImagePreviewListener() {
		@Override
		public void setImageBitmap(final Bitmap bitmap) {
			mImagePreviewImageView.setImageBitmap(bitmap);
		}

		@Override
		public void setLoadingSpinnerVisible(final boolean visible) {
			mImagePreviewLoadingSpinner.setVisibility(visible ? VISIBLE : GONE);
		}

		@Override
		public void setOuterViewVisible(final boolean visible) {
			mImagePreviewOuter.setVisibility(visible ? VISIBLE : GONE);
		}

		@Override
		public void setPlayOverlayVisible(final boolean visible) {
			mImagePreviewPlayOverlay.setVisibility(visible ? VISIBLE : GONE);
		}

		@Override
		public void setPreviewDimensions(final String ratio) {
			final ConstraintLayout.LayoutParams params =
					(ConstraintLayout.LayoutParams)mImagePreviewHolder.getLayoutParams();
			params.dimensionRatio = ratio;
			mImagePreviewHolder.setLayoutParams(params);
		}

		@Override
		public LinearLayout getFooterView() {
			return mFooter;
		}

		@Override
		public BaseActivity getActivity() {
			return mActivity;
		}

		@Override
		public void addErrorView(final ErrorView errorView) {
			mPostErrors.addView(errorView);
		}

		@Override
		public void setErrorViewLayout(final View errorView) {
			General.setLayoutMatchWidthWrapHeight(errorView);
		}

		@Override
		public boolean isUsageIdValid(final int usageId) {
			return usageId == mUsageId;
		}
	};
}
