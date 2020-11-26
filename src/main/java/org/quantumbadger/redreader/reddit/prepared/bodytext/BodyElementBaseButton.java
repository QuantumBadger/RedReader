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

import android.view.View;
import android.view.ViewGroup;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import org.quantumbadger.redreader.activities.BaseActivity;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.views.LinkDetailsView;

public abstract class BodyElementBaseButton extends BodyElement {

	@NonNull private final String mText;
	@Nullable private final String mSubtitle;

	private final boolean mIsLinkButton;

	@NonNull
	protected abstract View.OnClickListener generateOnClickListener(
			@NonNull final BaseActivity activity,
			@Nullable final Integer textColor,
			@Nullable final Float textSize,
			final boolean showLinkButtons);

	@Nullable
	protected abstract View.OnLongClickListener generateOnLongClickListener(
			@NonNull final BaseActivity activity,
			@Nullable final Integer textColor,
			@Nullable final Float textSize,
			final boolean showLinkButtons);

	public BodyElementBaseButton(
			@NonNull final String text,
			@Nullable final String subtitle, final boolean isLinkButton) {

		super(BlockType.BUTTON);
		mText = text;
		mSubtitle = subtitle;
		mIsLinkButton = isLinkButton;
	}

	@Override
	public final View generateView(
			@NonNull final BaseActivity activity,
			@Nullable final Integer textColor,
			@Nullable final Float textSize,
			final boolean showLinkButtons) {

		if(mIsLinkButton && !showLinkButtons) {
			// Don't show
			final View result = new View(activity);
			result.setVisibility(View.GONE);
			return result;
		}

		final LinkDetailsView ldv = new LinkDetailsView(
				activity,
				mText,
				mSubtitle);

		final int linkMarginPx = General.dpToPixels(activity, 8);

		final ViewGroup.MarginLayoutParams layoutParams
				= new ViewGroup.MarginLayoutParams(
						ViewGroup.LayoutParams.MATCH_PARENT,
						ViewGroup.LayoutParams.WRAP_CONTENT);

		layoutParams.setMargins(0, linkMarginPx, 0, linkMarginPx);
		ldv.setLayoutParams(layoutParams);

		ldv.setOnClickListener(
				generateOnClickListener(activity, textColor, textSize, showLinkButtons));

		final View.OnLongClickListener longClickListener
				= generateOnLongClickListener(
				activity,
				textColor,
				textSize,
				showLinkButtons);

		if(longClickListener != null) {
			ldv.setOnLongClickListener(longClickListener);
		}

		return ldv;
	}
}
