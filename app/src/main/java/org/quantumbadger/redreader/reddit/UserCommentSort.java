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

import android.net.Uri;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.annotation.StringRes;
import androidx.appcompat.app.AppCompatActivity;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.activities.OptionsMenuUtility;
import org.quantumbadger.redreader.common.StringUtils;

public enum UserCommentSort implements OptionsMenuUtility.Sort {
	NEW(R.string.sort_comments_new),
	HOT(R.string.sort_comments_hot),
	CONTROVERSIAL_HOUR(R.string.sort_posts_controversial_hour),
	CONTROVERSIAL_DAY(R.string.sort_posts_controversial_today),
	CONTROVERSIAL_WEEK(R.string.sort_posts_controversial_week),
	CONTROVERSIAL_MONTH(R.string.sort_posts_controversial_month),
	CONTROVERSIAL_YEAR(R.string.sort_posts_controversial_year),
	CONTROVERSIAL_ALL(R.string.sort_posts_controversial_all),
	TOP_HOUR(R.string.sort_posts_top_hour),
	TOP_DAY(R.string.sort_posts_top_today),
	TOP_WEEK(R.string.sort_posts_top_week),
	TOP_MONTH(R.string.sort_posts_top_month),
	TOP_YEAR(R.string.sort_posts_top_year),
	TOP_ALL(R.string.sort_posts_top_all);

	@StringRes
	private final int menuTitle;

	UserCommentSort(@StringRes final int menuTitle) {
		this.menuTitle = menuTitle;
	}

	@Nullable
	public static UserCommentSort parse(@Nullable String sort, @Nullable String t) {

		if (sort == null) {
			return null;
		}

		sort = StringUtils.asciiLowercase(sort);
		t = t != null ? StringUtils.asciiLowercase(t) : null;

		if (sort.equals("hot")) {
			return HOT;

		} else if (sort.equals("new")) {
			return NEW;

		} else if (sort.equals("controversial")) {
			if (t == null) {
				return CONTROVERSIAL_ALL;
			} else if (t.equals("all")) {
				return CONTROVERSIAL_ALL;
			} else if (t.equals("hour")) {
				return CONTROVERSIAL_HOUR;
			} else if (t.equals("day")) {
				return CONTROVERSIAL_DAY;
			} else if (t.equals("week")) {
				return CONTROVERSIAL_WEEK;
			} else if (t.equals("month")) {
				return CONTROVERSIAL_MONTH;
			} else if (t.equals("year")) {
				return CONTROVERSIAL_YEAR;
			} else {
				return CONTROVERSIAL_ALL;
			}

		} else if (sort.equals("top")) {

			if (t == null) {
				return TOP_ALL;
			} else if (t.equals("all")) {
				return TOP_ALL;
			} else if (t.equals("hour")) {
				return TOP_HOUR;
			} else if (t.equals("day")) {
				return TOP_DAY;
			} else if (t.equals("week")) {
				return TOP_WEEK;
			} else if (t.equals("month")) {
				return TOP_MONTH;
			} else if (t.equals("year")) {
				return TOP_YEAR;
			} else {
				return TOP_ALL;
			}

		} else {
			return null;
		}
	}

	public void addToUserCommentListingUri(@NonNull final Uri.Builder builder) {

		switch (this) {
			case HOT:
			case NEW:
				builder.appendQueryParameter("sort", StringUtils.asciiLowercase(name()));
				break;

			case CONTROVERSIAL_HOUR:
			case CONTROVERSIAL_DAY:
			case CONTROVERSIAL_WEEK:
			case CONTROVERSIAL_MONTH:
			case CONTROVERSIAL_YEAR:
			case CONTROVERSIAL_ALL:
			case TOP_HOUR:
			case TOP_DAY:
			case TOP_WEEK:
			case TOP_MONTH:
			case TOP_YEAR:
			case TOP_ALL:
				final String[] parts = name().split("_");
				builder.appendQueryParameter(
						"sort",
						StringUtils.asciiLowercase(parts[0]));
				builder.appendQueryParameter("t", StringUtils.asciiLowercase(parts[1]));
				break;
		}
	}

	@Override
	public int getMenuTitle() {
		return menuTitle;
	}

	@Override
	public void onSortSelected(final AppCompatActivity activity) {
		((OptionsMenuUtility.OptionsMenuCommentsListener)activity).onSortSelected(this);
	}
}
