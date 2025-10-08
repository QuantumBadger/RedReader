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

import android.content.res.TypedArray;
import android.graphics.Color;
import android.widget.ImageButton;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.Nullable;
import androidx.appcompat.widget.TooltipCompat;

import android.view.View;
import android.widget.FrameLayout;
import android.widget.ImageView;
import androidx.constraintlayout.widget.ConstraintLayout;
import androidx.annotation.NonNull;
import android.graphics.Bitmap;

import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.account.RedditAccount;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.activities.BaseActivity;
import org.quantumbadger.redreader.common.Fonts;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.LinkHandler;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.reddit.api.RedditPostActions;
import org.quantumbadger.redreader.reddit.prepared.RedditChangeDataManager;
import org.quantumbadger.redreader.reddit.prepared.RedditPreparedPost;
import org.quantumbadger.redreader.common.ImagePreviewUtils;
import org.quantumbadger.redreader.views.liststatus.ErrorView;

public class RedditPostHeaderView extends LinearLayout {

	private final TextView subtitle;

	@Nullable private final Runnable mChangeListenerAddTask;
	@Nullable private final Runnable mChangeListenerRemoveTask;

	@NonNull private final FrameLayout mImagePreviewHolder;
	@NonNull private final ImageView mImagePreviewImageView;
	@NonNull private final ConstraintLayout mImagePreviewPlayOverlay;
	@NonNull private final LinearLayout mImagePreviewOuter;
	@NonNull private final LoadingSpinnerView mImagePreviewLoadingSpinner;
	@NonNull private final BaseActivity mActivity;
	@NonNull private final LinearLayout mPostErrors;

	static private int mUsageId = 0;

	public RedditPostHeaderView(
			final BaseActivity activity,
			final RedditPreparedPost post) {

		super(activity);

		mActivity = activity;

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
			post.shouldShowInlinePreview();

		final float dpScale = mActivity.getResources().getDisplayMetrics().density;

		setOrientation(LinearLayout.VERTICAL);

		final LinearLayout greyHeader = new LinearLayout(mActivity);

		RedditPostActions.INSTANCE.setupAccessibilityActions(
				new AccessibilityActionManager(
						greyHeader,
						mActivity.getResources()),
				post,
				mActivity,
				true);

		greyHeader.setOrientation(LinearLayout.VERTICAL);

		final int sidesPadding = (int)(15.0f * dpScale);
		final int topPadding = (int)(10.0f * dpScale);

		greyHeader.setPadding(sidesPadding, topPadding, sidesPadding, topPadding);

		final float titleFontScale = PrefsUtility.appearance_fontscale_post_header_titles();

		final TextView title = new TextView(mActivity);
		title.setTextSize(19.0f * titleFontScale);
		title.setTypeface(Fonts.getRobotoLightOrAlternative());
		title.setText(post.src.getTitle());
		title.setContentDescription(post.buildAccessibilityTitle(mActivity, true));
		title.setTextColor(Color.WHITE);
		greyHeader.addView(title);

		final View previewView = inflate(mActivity, R.layout.post_header_preview, null);

		mPostErrors = previewView.findViewById(R.id.reddit_post_errors);

		mImagePreviewOuter = previewView.findViewById(R.id.reddit_post_image_preview_outer);
		mImagePreviewHolder = previewView.findViewById(R.id.reddit_post_image_preview_holder);
		mImagePreviewImageView = previewView.findViewById(R.id.reddit_post_image_preview_imageview);
		mImagePreviewPlayOverlay = previewView.findViewById(
				R.id.reddit_post_image_preview_play_overlay);

		mImagePreviewLoadingSpinner = new LoadingSpinnerView(mActivity);
		mImagePreviewHolder.addView(mImagePreviewLoadingSpinner);

		greyHeader.addView(previewView, greyHeader.indexOfChild(title) + 1);

		if(showInlinePreview) {
			ImagePreviewUtils.downloadInlinePreview(activity, post, previewListener, mUsageId);
		}

		final float subtitleFontScale =
				PrefsUtility.appearance_fontscale_post_header_subtitles();

		subtitle = new TextView(mActivity);
		subtitle.setTextSize(13.0f * subtitleFontScale);
		subtitle.setText(post.buildSubtitle(mActivity, true));
		subtitle.setContentDescription(post.buildAccessibilitySubtitle(mActivity, true));

		subtitle.setTextColor(Color.rgb(200, 200, 200));
		greyHeader.addView(subtitle);

		{
			final TypedArray appearance = mActivity.obtainStyledAttributes(new int[] {
					R.attr.rrPostListHeaderBackgroundCol});

			greyHeader.setBackgroundColor(appearance.getColor(0, General.COLOR_INVALID));

			appearance.recycle();
		}

		greyHeader.setOnClickListener(v -> {
			if(!post.isSelf()) {
				LinkHandler.onLinkClicked(
						mActivity,
						post.src.getUrl(),
						false,
						post.src.getSrc());
			}
		});

		greyHeader.setOnLongClickListener(v -> {
			RedditPostActions.INSTANCE.showActionMenu(mActivity, post);
			return true;
		});

		addView(greyHeader);

		final RedditAccount currentUser =
				RedditAccountManager.getInstance(mActivity).getDefaultAccount();

		if(!currentUser.isAnonymous()) {

			// A user is logged in

			final RedditChangeDataManager changeDataManager
					= RedditChangeDataManager.getInstance(currentUser);
			final RedditChangeDataManager.Listener changeListener;

			if(!PrefsUtility.pref_appearance_hide_headertoolbar_commentlist()) {

				final LinearLayout buttons =
						inflate(mActivity, R.layout.post_header_toolbar, this)
								.findViewById(R.id.post_toolbar_layout);

				for(int i = 0; i < buttons.getChildCount(); i++) {
					final ImageButton button = (ImageButton)buttons.getChildAt(i);
					TooltipCompat.setTooltipText(button, button.getContentDescription());
				}

				final ImageButton buttonAddUpvote =
						buttons.findViewById(R.id.post_toolbar_botton_add_upvote);
				final ImageButton buttonRemoveUpvote =
						buttons.findViewById(R.id.post_toolbar_botton_remove_upvote);
				final ImageButton buttonAddDownvote =
						buttons.findViewById(R.id.post_toolbar_botton_add_downvote);
				final ImageButton buttonRemoveDownvote =
						buttons.findViewById(R.id.post_toolbar_botton_remove_downvote);
				final ImageButton buttonReply =
						buttons.findViewById(R.id.post_toolbar_botton_reply);
				final ImageButton buttonShare =
						buttons.findViewById(R.id.post_toolbar_botton_share);
				final ImageButton buttonMore =
						buttons.findViewById(R.id.post_toolbar_botton_more);

				buttonAddUpvote.setOnClickListener(v -> post.performAction(
						mActivity,
						RedditPostActions.Action.UPVOTE));
				buttonRemoveUpvote.setOnClickListener(v -> post.performAction(
						mActivity,
						RedditPostActions.Action.UNVOTE));
				buttonAddDownvote.setOnClickListener(v -> post.performAction(
						mActivity,
						RedditPostActions.Action.DOWNVOTE));
				buttonRemoveDownvote.setOnClickListener(v -> post.performAction(
						mActivity,
						RedditPostActions.Action.UNVOTE));
				buttonReply.setOnClickListener(v -> post.performAction(
						mActivity,
						RedditPostActions.Action.REPLY));
				buttonShare.setOnClickListener(v -> post.performAction(
						mActivity,
						RedditPostActions.Action.SHARE));
				buttonMore.setOnClickListener(v -> post.performAction(
						mActivity,
						RedditPostActions.Action.ACTION_MENU));

				changeListener = thingIdAndType -> {

					subtitle.setText(post.buildSubtitle(mActivity, true));
					subtitle.setContentDescription(
							post.buildAccessibilitySubtitle(mActivity, true));

					final boolean isUpvoted = changeDataManager.isUpvoted(
							post.src.getIdAndType());

					final boolean isDownvoted = changeDataManager.isDownvoted(
							post.src.getIdAndType());

					if(isUpvoted) {
						buttonAddUpvote.setVisibility(GONE);
						buttonRemoveUpvote.setVisibility(VISIBLE);
						buttonAddDownvote.setVisibility(VISIBLE);
						buttonRemoveDownvote.setVisibility(GONE);

					} else if(isDownvoted) {
						buttonAddUpvote.setVisibility(VISIBLE);
						buttonRemoveUpvote.setVisibility(GONE);
						buttonAddDownvote.setVisibility(GONE);
						buttonRemoveDownvote.setVisibility(VISIBLE);

					} else {
						buttonAddUpvote.setVisibility(VISIBLE);
						buttonRemoveUpvote.setVisibility(GONE);
						buttonAddDownvote.setVisibility(VISIBLE);
						buttonRemoveDownvote.setVisibility(GONE);
					}
				};

			} else {
				changeListener = thingIdAndType -> {
					subtitle.setText(post.buildSubtitle(mActivity, true));
					subtitle.setContentDescription(
							post.buildAccessibilitySubtitle(mActivity, true));
				};
			}

			mChangeListenerAddTask = () -> {
				changeDataManager.addListener(post.src.getIdAndType(), changeListener);
				changeListener.onRedditDataChange(post.src.getIdAndType());
			};

			mChangeListenerRemoveTask = () -> changeDataManager.removeListener(
					post.src.getIdAndType(),
					changeListener);

		} else {
			mChangeListenerAddTask = null;
			mChangeListenerRemoveTask = null;
		}
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
			return null;
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

	@Override
	protected void onAttachedToWindow() {
		super.onAttachedToWindow();

		if(mChangeListenerAddTask != null) {
			mChangeListenerAddTask.run();
		}
	}

	@Override
	protected void onDetachedFromWindow() {
		super.onDetachedFromWindow();

		if(mChangeListenerRemoveTask != null) {
			mChangeListenerRemoveTask.run();
		}
	}
}
