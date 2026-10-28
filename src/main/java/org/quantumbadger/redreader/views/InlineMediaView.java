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
import android.graphics.Bitmap;
import android.graphics.Color;
import android.graphics.Rect;
import android.graphics.Typeface;
import android.graphics.drawable.GradientDrawable;
import android.util.TypedValue;
import android.view.Gravity;
import android.widget.FrameLayout;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.activities.BaseActivity;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.LinkHandler;
import org.quantumbadger.redreader.common.RRError;
import org.quantumbadger.redreader.reddit.prepared.InlineMedia;
import org.quantumbadger.redreader.reddit.prepared.InlinePreviewLoader;
import org.quantumbadger.redreader.reddit.prepared.bodytext.BodyElementInlineMedia;
import org.quantumbadger.redreader.views.liststatus.ErrorView;

/**
 * Displays an image, or the first frame of a GIF, which is attached to a comment.
 *
 * <p>The media is displayed at its natural size, treating each of its pixels as one dp, so
 * that small media such as GIFs from Giphy aren't stretched to the full width of the
 * comment. Larger media is scaled down to fit the width of the comment, and to at most
 * {@link #getMaxHeightPx(int)}, so that a single image never takes up so much of the list
 * that it's hard to scroll past.
 *
 * <p>Tapping opens the media in the same way as tapping the link to it, and long pressing
 * shows the same menu as long pressing the link.
 */
public final class InlineMediaView extends FrameLayout implements InlinePreviewLoader.Listener {

	@NonNull private final BaseActivity mActivity;
	@NonNull private final InlineMedia mMedia;
	@NonNull private final InlinePreviewLoader mLoader;

	@NonNull private final ImageView mImageView;
	@NonNull private final LoadingSpinnerView mLoadingSpinner;
	@Nullable private final TextView mGifBadge;

	@Nullable private ErrorView mErrorView;
	private boolean mFailed;

	private final Rect mWindowVisibleDisplayFrame = new Rect();

	public static int getNaturalWidthPx(
			@NonNull final Context context,
			@NonNull final InlineMedia media) {

		return Math.max(1, General.dpToPixels(context, media.getWidth()));
	}

	/**
	 * The maximum height, in pixels, which inline media may take up, given the height of the
	 * area it is displayed in.
	 */
	public static int getMaxHeightPx(final int displayAreaHeightPx) {
		return Math.max(1, displayAreaHeightPx / 2);
	}

	public InlineMediaView(
			@NonNull final BaseActivity activity,
			@NonNull final BodyElementInlineMedia element) {

		super(activity);

		mActivity = activity;
		mMedia = element.getMedia();
		mLoader = element.getLoader(activity);

		mImageView = new ImageView(activity);
		mImageView.setScaleType(ImageView.ScaleType.FIT_CENTER);
		addView(mImageView, new LayoutParams(
				LayoutParams.MATCH_PARENT,
				LayoutParams.MATCH_PARENT));

		mLoadingSpinner = new LoadingSpinnerView(activity);
		addView(mLoadingSpinner, new LayoutParams(
				LayoutParams.MATCH_PARENT,
				LayoutParams.MATCH_PARENT));

		final boolean isAnimated = mMedia.getType() == InlineMedia.Type.ANIMATED;

		if(isAnimated) {
			mGifBadge = createGifBadge(activity);

			final LayoutParams badgeLayoutParams = new LayoutParams(
					LayoutParams.WRAP_CONTENT,
					LayoutParams.WRAP_CONTENT,
					Gravity.BOTTOM | Gravity.START);

			final int badgeMarginPx = General.dpToPixels(activity, 6);
			badgeLayoutParams.setMargins(
					badgeMarginPx,
					badgeMarginPx,
					badgeMarginPx,
					badgeMarginPx);

			addView(mGifBadge, badgeLayoutParams);

		} else {
			mGifBadge = null;
		}

		final String caption = element.getCaption();

		if(caption != null) {
			setContentDescription(caption);
		} else {
			setContentDescription(activity.getString(isAnimated
					? R.string.inline_media_description_gif
					: R.string.inline_media_description_image));
		}

		setOnClickListener(v -> LinkHandler.onLinkClicked(activity, element.getUrl()));

		setOnLongClickListener(v -> {
			LinkHandler.onLinkLongClicked(activity, element.getUrl());
			return true;
		});
	}

	@NonNull
	private static TextView createGifBadge(@NonNull final Context context) {

		final TextView badge = new TextView(context);

		badge.setText(R.string.inline_media_gif_badge);
		badge.setTextColor(Color.WHITE);
		badge.setTextSize(TypedValue.COMPLEX_UNIT_SP, 11);
		badge.setTypeface(Typeface.DEFAULT_BOLD);

		final int paddingHorizontalPx = General.dpToPixels(context, 5);
		final int paddingVerticalPx = General.dpToPixels(context, 1);
		badge.setPadding(
				paddingHorizontalPx,
				paddingVerticalPx,
				paddingHorizontalPx,
				paddingVerticalPx);

		final GradientDrawable background = new GradientDrawable();
		background.setColor(Color.argb(160, 0, 0, 0));
		background.setCornerRadius(General.dpToPixels(context, 3));
		badge.setBackground(background);

		// The view as a whole already describes itself as a GIF
		badge.setImportantForAccessibility(IMPORTANT_FOR_ACCESSIBILITY_NO);

		return badge;
	}

	@Override
	protected void onAttachedToWindow() {
		super.onAttachedToWindow();

		// This calls back into onInlinePreviewStateChanged with the current state, which
		// will be the fully loaded image if this comment has already been preloaded
		mLoader.setListener(this);
	}

	@Override
	protected void onDetachedFromWindow() {
		super.onDetachedFromWindow();

		// The comment may already have been bound to a new view, which has taken over
		mLoader.removeListener(this);
	}

	@Override
	public void onInlinePreviewStateChanged(@NonNull final InlinePreviewLoader loader) {

		if(mErrorView != null) {
			removeView(mErrorView);
			mErrorView = null;
		}

		switch(loader.getState()) {

			case LOADED:

				mFailed = false;
				mImageView.setImageBitmap(loader.getBitmap());
				mImageView.setVisibility(VISIBLE);
				mLoadingSpinner.setVisibility(GONE);
				setGifBadgeVisibility(VISIBLE);
				break;

			case FAILED:

				mFailed = true;
				mImageView.setImageBitmap(null);
				mImageView.setVisibility(GONE);
				mLoadingSpinner.setVisibility(GONE);
				setGifBadgeVisibility(GONE);

				final RRError error = loader.getError();

				if(error != null) {
					mErrorView = new ErrorView(mActivity, error);
					addView(mErrorView);
					General.setLayoutMatchWidthWrapHeight(mErrorView);
				}

				break;

			case NOT_LOADED:
			case LOADING:
			default:

				mFailed = false;
				mImageView.setImageBitmap(null);
				mImageView.setVisibility(VISIBLE);
				mLoadingSpinner.setVisibility(VISIBLE);
				setGifBadgeVisibility(VISIBLE);
				break;
		}

		requestLayout();
	}

	private void setGifBadgeVisibility(final int visibility) {
		if(mGifBadge != null) {
			mGifBadge.setVisibility(visibility);
		}
	}

	@Override
	protected void onMeasure(final int widthMeasureSpec, final int heightMeasureSpec) {

		if(mFailed) {
			// Only the error message is shown, at its own size
			super.onMeasure(widthMeasureSpec, heightMeasureSpec);
			return;
		}

		int sourceWidth = mMedia.getWidth();
		int sourceHeight = mMedia.getHeight();

		final Bitmap bitmap = mLoader.getBitmap();

		if(!mMedia.isSizeKnown() && bitmap != null) {
			// The placeholder size is replaced by that of the image, once it has loaded
			sourceWidth = bitmap.getWidth();
			sourceHeight = bitmap.getHeight();
		}

		final int naturalWidth = Math.max(1, General.dpToPixels(getContext(), sourceWidth));

		final int availableWidth;

		if(MeasureSpec.getMode(widthMeasureSpec) == MeasureSpec.UNSPECIFIED) {
			// For example, in a table, which scrolls horizontally
			availableWidth = getResources().getDisplayMetrics().widthPixels;
		} else {
			availableWidth = MeasureSpec.getSize(widthMeasureSpec);
		}

		int width = Math.max(1, Math.min(naturalWidth, availableWidth));
		int height = Math.max(1, (int)(((long)width * sourceHeight) / sourceWidth));

		final int maxHeight = getMaxHeightPx(InlinePreviewHolderView.getDisplayAreaHeightPx(
				this,
				mWindowVisibleDisplayFrame));

		if(height > maxHeight) {
			height = maxHeight;
			width = Math.max(1, (int)(((long)height * sourceWidth) / sourceHeight));
		}

		super.onMeasure(
				MeasureSpec.makeMeasureSpec(width, MeasureSpec.EXACTLY),
				MeasureSpec.makeMeasureSpec(height, MeasureSpec.EXACTLY));
	}
}
