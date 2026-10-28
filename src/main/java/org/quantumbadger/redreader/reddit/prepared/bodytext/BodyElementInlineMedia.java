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

package org.quantumbadger.redreader.reddit.prepared.bodytext;

import android.graphics.Rect;
import android.view.View;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.annotation.UiThread;

import org.quantumbadger.redreader.activities.BaseActivity;
import org.quantumbadger.redreader.common.DisplayUtils;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.UriString;
import org.quantumbadger.redreader.reddit.prepared.InlineMedia;
import org.quantumbadger.redreader.reddit.prepared.InlinePreviewLoader;
import org.quantumbadger.redreader.views.InlineMediaView;

/**
 * An image, or the first frame of a GIF, which is attached to a comment and displayed in its
 * body.
 *
 * <p>The views for a comment's body are regenerated every time the comment is bound, so the
 * image is held by the loader here rather than by the view.
 */
public class BodyElementInlineMedia extends BodyElement {

	// Bounded to keep the memory used by each image reasonable
	private static final int MAX_DECODE_WIDTH_PX = 1080;

	@NonNull private final UriString mUrl;
	@NonNull private final InlineMedia mMedia;
	@Nullable private final String mCaption;

	@Nullable private InlinePreviewLoader mLoader;

	public BodyElementInlineMedia(
			@NonNull final UriString url,
			@NonNull final InlineMedia media,
			@Nullable final String caption) {

		super(BlockType.INLINE_MEDIA);

		mUrl = url;
		mMedia = media;
		mCaption = caption;
	}

	/**
	 * The URL which the comment links to, which is opened when the media is tapped.
	 */
	@NonNull
	public UriString getUrl() {
		return mUrl;
	}

	@NonNull
	public InlineMedia getMedia() {
		return mMedia;
	}

	@Nullable
	public String getCaption() {
		return mCaption;
	}

	@NonNull
	@UiThread
	public InlinePreviewLoader getLoader(@NonNull final BaseActivity activity) {

		General.checkThisIsUIThread();

		if(mLoader == null) {
			mLoader = new InlinePreviewLoader(activity, this::calculatePreviewDetails);
		}

		return mLoader;
	}

	@NonNull
	private InlinePreviewLoader.PreviewDetails calculatePreviewDetails(
			@NonNull final BaseActivity activity) {

		final Rect windowVisibleDisplayFrame
				= DisplayUtils.getWindowVisibleDisplayFrame(activity);

		// This is an estimate of the width the media is displayed at (see InlineMediaView),
		// as the comment is narrower than the window when it is nested, or in the two pane
		// layout. That makes this an overestimate, which costs a little memory but never
		// quality.
		final int displayWidth = Math.max(1, Math.min(
				windowVisibleDisplayFrame.width(),
				InlineMediaView.getNaturalWidthPx(activity, mMedia)));

		final int boxWidth = Math.min(MAX_DECODE_WIDTH_PX, displayWidth);

		final int maxBoxHeight = Math.max(1, (int)(
				((long)InlineMediaView.getMaxHeightPx(windowVisibleDisplayFrame.height())
						* boxWidth)
						/ displayWidth));

		final int boxHeight = Math.max(1, Math.min(
				maxBoxHeight,
				(int)(((long)mMedia.getHeight() * boxWidth) / mMedia.getWidth())));

		return new InlinePreviewLoader.PreviewDetails(
				mMedia.chooseStill(boxWidth).getUrl(),
				mMedia.getWidth(),
				mMedia.getHeight(),
				boxWidth,
				boxHeight);
	}

	@Override
	public View generateView(
			@NonNull final BaseActivity activity,
			@Nullable final Integer textColor,
			@Nullable final Float textSize,
			final boolean showLinkButtons) {

		return new InlineMediaView(activity, this);
	}
}
