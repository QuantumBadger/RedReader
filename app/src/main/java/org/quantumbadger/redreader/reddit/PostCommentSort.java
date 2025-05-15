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

package org.quantumbadger.redreader.reddit;

import androidx.annotation.StringRes;
import androidx.appcompat.app.AppCompatActivity;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.activities.OptionsMenuUtility;
import org.quantumbadger.redreader.common.StringUtils;

public enum PostCommentSort implements OptionsMenuUtility.Sort {

	BEST("confidence", R.string.sort_comments_best, R.string.sort_comments_best_suggested),
	HOT("hot", R.string.sort_comments_hot, R.string.sort_comments_hot_suggested),
	NEW("new", R.string.sort_comments_new, R.string.sort_comments_new_suggested),
	OLD("old", R.string.sort_comments_old, R.string.sort_comments_old_suggested),
	TOP("top", R.string.sort_comments_top, R.string.sort_comments_top_suggested),
	CONTROVERSIAL("controversial",
			R.string.sort_comments_controversial,
			R.string.sort_comments_controversial_suggested),
	QA("qa", R.string.sort_comments_qa, R.string.sort_comments_qa_suggested);

	public final String key;
	@StringRes private final int menuTitle;
	@StringRes private final int suggestedTitle;

	PostCommentSort(
			final String key,
			@StringRes final int menuTitle,
			@StringRes final int suggestedTitle) {
		this.key = key;
		this.menuTitle = menuTitle;
		this.suggestedTitle = suggestedTitle;
	}

	public static PostCommentSort lookup(String name) {

		name = StringUtils.asciiUppercase(name);

		if (name.equals("CONFIDENCE")) {
			return BEST; // oh, reddit...
		}

		try {
			return PostCommentSort.valueOf(name);
		} catch (final IllegalArgumentException e) {
			return null;
		}
	}

	@Override
	public int getMenuTitle() {
		return menuTitle;
	}

	public int getSuggestedTitle() {
		return suggestedTitle;
	}

	@Override
	public void onSortSelected(final AppCompatActivity activity) {
		((OptionsMenuUtility.OptionsMenuCommentsListener)activity).onSortSelected(this);
	}
}
