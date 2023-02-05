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

import android.text.SpannableStringBuilder;
import android.view.View;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import org.quantumbadger.redreader.activities.BaseActivity;
import org.quantumbadger.redreader.common.AndroidCommon;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.views.LinkifiedTextView;

public class BodyElementTextSpanned extends BodyElement implements DynamicSpanned {

	@NonNull private final SpannableStringBuilder mSpanned;

	private LinkifiedTextView mTextView;

	public BodyElementTextSpanned(
			@NonNull final BlockType blockType,
			@NonNull final SpannableStringBuilder spanned) {
		super(blockType);
		mSpanned = spanned;
	}

	@Override
	public void addSpanDynamic(final Object what, final int start, final int end, final int flags) {
		AndroidCommon.runOnUiThread(() -> {
			mSpanned.setSpan(what, start, end, flags);
			if(mTextView != null) {
				mTextView.setText(mSpanned);
			}
		});
	}

	@Override
	public View generateView(
			@NonNull final BaseActivity activity,
			@Nullable final Integer textColor,
			@Nullable final Float textSize,
			final boolean showLinkButtons) {

		mTextView = new LinkifiedTextView(activity);

		if(textColor != null) {
			mTextView.setTextColor(textColor);
		}
		if(textSize != null) {
			mTextView.setTextSize(textSize);
		}

		mTextView.setText(mSpanned, LinkifiedTextView.BufferType.SPANNABLE);

		if(PrefsUtility.pref_accessibility_separate_body_text_lines()) {

			mTextView.setFocusable(true);
		}

		return mTextView;
	}
}
