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

package org.saiditnet.redreader.common;

import android.content.Context;
import android.content.SharedPreferences;
import android.content.res.TypedArray;
import android.preference.PreferenceManager;
import org.saiditnet.redreader.R;

import java.util.EnumSet;

public class RRThemeAttributes {

	public final int
			rrCommentHeaderBoldCol,
			rrCommentHeaderAuthorCol,
			rrPostSubtitleUpvoteCol,
			rrPostSubtitleDownvoteCol,
			rrFlairBackCol,
			rrFlairTextCol,
			rrGoldBackCol,
			rrGoldTextCol,
			rrCommentHeaderCol,
			rrCommentBodyCol,
			rrMainTextCol,
			colorAccent;

	private final EnumSet<PrefsUtility.AppearanceCommentHeaderItem> mCommentHeaderItems;

	public final float rrCommentFontScale;

	public RRThemeAttributes(final Context context) {

		final TypedArray appearance = context.obtainStyledAttributes(new int[]{
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
				R.attr.colorAccent
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

		appearance.recycle();

		final SharedPreferences prefs = PreferenceManager.getDefaultSharedPreferences(context);

		mCommentHeaderItems = PrefsUtility.appearance_comment_header_items(context, prefs);

		rrCommentFontScale = PrefsUtility.appearance_fontscale_inbox(
				context,
				prefs);
	}

	public boolean shouldShow(final PrefsUtility.AppearanceCommentHeaderItem type) {
		return mCommentHeaderItems.contains(type);
	}
}
