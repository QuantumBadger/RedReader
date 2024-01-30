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

package org.quantumbadger.redreader.common;

import android.content.Context;
import android.content.res.TypedArray;
import org.quantumbadger.redreader.R;

import java.util.EnumSet;

public class RRThemeAttributes {

	public final int rrCommentHeaderBoldCol;
	public final int rrCommentHeaderAuthorCol;
	public final int rrPostSubtitleUpvoteCol;
	public final int rrPostSubtitleDownvoteCol;
	public final int rrFlairBackCol;
	public final int rrFlairTextCol;
	public final int rrGoldBackCol;
	public final int rrGoldTextCol;
	public final int rrCommentHeaderCol;
	public final int rrCommentBodyCol;
	public final int rrMainTextCol;
	public final int colorAccent;
	public final int rrCrosspostBackCol;
	public final int rrCrosspostTextCol;

	private final EnumSet<PrefsUtility.AppearanceCommentHeaderItem> mCommentHeaderItems;

	public final float rrCommentFontScale;
	public final float rrCommentHeaderFontScale;

	public RRThemeAttributes(final Context context) {

		final TypedArray appearance = context.obtainStyledAttributes(new int[] {
				R.attr.rrCommentHeaderBoldCol,
				R.attr.rrCommentHeaderAuthorCol,
				R.attr.rrPostSubtitleUpvoteCol,
				R.attr.rrPostSubtitleDownvoteCol,
				R.attr.rrFlairBackCol,
				R.attr.rrFlairTextCol,
				R.attr.rrGoldBackCol,
				R.attr.rrGoldTextCol,
				R.attr.rrCommentHeaderCol,
				R.attr.rrCommentBodyCol,
				R.attr.rrMainTextCol,
				R.attr.colorAccent,
				R.attr.rrCrosspostBackCol,
				R.attr.rrCrosspostTextCol
		});

		rrCommentHeaderBoldCol = appearance.getColor(0, 255);
		rrCommentHeaderAuthorCol = appearance.getColor(1, 255);
		rrPostSubtitleUpvoteCol = appearance.getColor(2, 255);
		rrPostSubtitleDownvoteCol = appearance.getColor(3, 255);
		rrFlairBackCol = appearance.getColor(4, 0);
		rrFlairTextCol = appearance.getColor(5, 255);
		rrGoldBackCol = appearance.getColor(6, 0);
		rrGoldTextCol = appearance.getColor(7, 255);
		rrCommentHeaderCol = appearance.getColor(8, 255);
		rrCommentBodyCol = appearance.getColor(9, 255);
		rrMainTextCol = appearance.getColor(10, 255);
		colorAccent = appearance.getColor(11, 255);
		rrCrosspostBackCol = appearance.getColor(12, 255);
		rrCrosspostTextCol = appearance.getColor(13, 255);

		appearance.recycle();

		mCommentHeaderItems = PrefsUtility.appearance_comment_header_items();

		rrCommentFontScale = PrefsUtility.appearance_fontscale_bodytext();
		rrCommentHeaderFontScale = PrefsUtility.appearance_fontscale_comment_headers();
	}

	public boolean shouldShow(final PrefsUtility.AppearanceCommentHeaderItem type) {
		return mCommentHeaderItems.contains(type);
	}
}
