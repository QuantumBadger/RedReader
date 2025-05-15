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

public enum PostSort implements OptionsMenuUtility.Sort {
	HOT(R.string.sort_posts_hot),
	NEW(R.string.sort_posts_new),
	RISING(R.string.sort_posts_rising),
	TOP_HOUR(R.string.sort_posts_top_hour),
	TOP_DAY(R.string.sort_posts_top_today),
	TOP_WEEK(R.string.sort_posts_top_week),
	TOP_MONTH(R.string.sort_posts_top_month),
	TOP_YEAR(R.string.sort_posts_top_year),
	TOP_ALL(R.string.sort_posts_top_all),
	CONTROVERSIAL_HOUR(R.string.sort_posts_controversial_hour),
	CONTROVERSIAL_DAY(R.string.sort_posts_controversial_today),
	CONTROVERSIAL_WEEK(R.string.sort_posts_controversial_week),
	CONTROVERSIAL_MONTH(R.string.sort_posts_controversial_month),
	CONTROVERSIAL_YEAR(R.string.sort_posts_controversial_year),
	CONTROVERSIAL_ALL(R.string.sort_posts_controversial_all),
	BEST(R.string.sort_posts_best),
	// Sorts related to Search Listings
	RELEVANCE_HOUR(R.string.sort_posts_relevance_hour),
	RELEVANCE_DAY(R.string.sort_posts_relevance_today),
	RELEVANCE_WEEK(R.string.sort_posts_relevance_week),
	RELEVANCE_MONTH(R.string.sort_posts_relevance_month),
	RELEVANCE_YEAR(R.string.sort_posts_relevance_year),
	RELEVANCE_ALL(R.string.sort_posts_relevance_all),
	NEW_HOUR(R.string.sort_posts_new_hour),
	NEW_DAY(R.string.sort_posts_new_today),
	NEW_WEEK(R.string.sort_posts_new_week),
	NEW_MONTH(R.string.sort_posts_new_month),
	NEW_YEAR(R.string.sort_posts_new_year),
	NEW_ALL(R.string.sort_posts_new_all),
	COMMENTS_HOUR(R.string.sort_posts_comments_hour),
	COMMENTS_DAY(R.string.sort_posts_comments_today),
	COMMENTS_WEEK(R.string.sort_posts_comments_week),
	COMMENTS_MONTH(R.string.sort_posts_comments_month),
	COMMENTS_YEAR(R.string.sort_posts_comments_year),
	COMMENTS_ALL(R.string.sort_posts_comments_all),
	HOT_HOUR(R.string.sort_posts_hot_hour),
	HOT_DAY(R.string.sort_posts_hot_today),
	HOT_WEEK(R.string.sort_posts_hot_week),
	HOT_MONTH(R.string.sort_posts_hot_month),
	HOT_YEAR(R.string.sort_posts_hot_year),
	HOT_ALL(R.string.sort_posts_hot_all);

	@StringRes private final int menuTitle;
	PostSort(@StringRes final int menuTitle) {
		this.menuTitle = menuTitle;
	}

	@Nullable
	public static PostSort valueOfOrNull(@NonNull final String string) {

		try {
			return valueOf(StringUtils.asciiUppercase(string));
		} catch(final IllegalArgumentException e) {
			return null;
		}
	}

	@Nullable
	public static PostSort parse(@Nullable String sort, @Nullable String t) {

		if(sort == null) {
			return null;
		}

		sort = StringUtils.asciiLowercase(sort);
		t = t != null ? StringUtils.asciiLowercase(t) : null;

		if(sort.equals("hot")) {
			return HOT;

		} else if(sort.equals("new")) {
			return NEW;

		} else if(sort.equals("best")) {
			return BEST;

		} else if(sort.equals("controversial")) {

			if(t == null) {
				return CONTROVERSIAL_ALL;
			} else if(t.equals("all")) {
				return CONTROVERSIAL_ALL;
			} else if(t.equals("hour")) {
				return CONTROVERSIAL_HOUR;
			} else if(t.equals("day")) {
				return CONTROVERSIAL_DAY;
			} else if(t.equals("week")) {
				return CONTROVERSIAL_WEEK;
			} else if(t.equals("month")) {
				return CONTROVERSIAL_MONTH;
			} else if(t.equals("year")) {
				return CONTROVERSIAL_YEAR;
			} else {
				return CONTROVERSIAL_ALL;
			}

		} else if(sort.equals("rising")) {
			return RISING;

		} else if(sort.equals("top")) {

			if(t == null) {
				return TOP_ALL;
			} else if(t.equals("all")) {
				return TOP_ALL;
			} else if(t.equals("hour")) {
				return TOP_HOUR;
			} else if(t.equals("day")) {
				return TOP_DAY;
			} else if(t.equals("week")) {
				return TOP_WEEK;
			} else if(t.equals("month")) {
				return TOP_MONTH;
			} else if(t.equals("year")) {
				return TOP_YEAR;
			} else {
				return TOP_ALL;
			}

		} else {
			return null;
		}
	}

	@Nullable
	public static PostSort parseSearch(@Nullable String sort, @Nullable String t) {

		if(sort == null) {
			return null;
		}

		sort = StringUtils.asciiLowercase(sort);
		t = t != null ? StringUtils.asciiLowercase(t) : null;

		if(sort.equals("relevance")) {

			if(t == null) {
				return RELEVANCE_ALL;
			} else if(t.equals("all")) {
				return RELEVANCE_ALL;
			} else if(t.equals("hour")) {
				return RELEVANCE_HOUR;
			} else if(t.equals("day")) {
				return RELEVANCE_DAY;
			} else if(t.equals("week")) {
				return RELEVANCE_WEEK;
			} else if(t.equals("month")) {
				return RELEVANCE_MONTH;
			} else if(t.equals("year")) {
				return RELEVANCE_YEAR;
			} else {
				return RELEVANCE_ALL;
			}

		} else if(sort.equals("new")) {

			if(t == null) {
				return NEW_ALL;
			} else if(t.equals("all")) {
				return NEW_ALL;
			} else if(t.equals("hour")) {
				return NEW_HOUR;
			} else if(t.equals("day")) {
				return NEW_DAY;
			} else if(t.equals("week")) {
				return NEW_WEEK;
			} else if(t.equals("month")) {
				return NEW_MONTH;
			} else if(t.equals("year")) {
				return NEW_YEAR;
			} else {
				return NEW_ALL;
			}

		} else if(sort.equals("hot")) {

			if(t == null) {
				return HOT_ALL;
			} else if(t.equals("all")) {
				return HOT_ALL;
			} else if(t.equals("hour")) {
				return HOT_HOUR;
			} else if(t.equals("day")) {
				return HOT_DAY;
			} else if(t.equals("week")) {
				return HOT_WEEK;
			} else if(t.equals("month")) {
				return HOT_MONTH;
			} else if(t.equals("year")) {
				return HOT_YEAR;
			} else {
				return HOT_ALL;
			}

		} else if(sort.equals("top")) {

			if(t == null) {
				return TOP_ALL;
			} else if(t.equals("all")) {
				return TOP_ALL;
			} else if(t.equals("hour")) {
				return TOP_HOUR;
			} else if(t.equals("day")) {
				return TOP_DAY;
			} else if(t.equals("week")) {
				return TOP_WEEK;
			} else if(t.equals("month")) {
				return TOP_MONTH;
			} else if(t.equals("year")) {
				return TOP_YEAR;
			} else {
				return TOP_ALL;
			}

		} else if(sort.equals("comments")) {

			if(t == null) {
				return COMMENTS_ALL;
			} else if(t.equals("all")) {
				return COMMENTS_ALL;
			} else if(t.equals("hour")) {
				return COMMENTS_HOUR;
			} else if(t.equals("day")) {
				return COMMENTS_DAY;
			} else if(t.equals("week")) {
				return COMMENTS_WEEK;
			} else if(t.equals("month")) {
				return COMMENTS_MONTH;
			} else if(t.equals("year")) {
				return COMMENTS_YEAR;
			} else {
				return COMMENTS_ALL;
			}

		} else {
			return null;
		}
	}

	public void addToUserPostListingUri(@NonNull final Uri.Builder builder) {

		switch(this) {
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
				builder.appendQueryParameter("sort", StringUtils.asciiLowercase(parts[0]));
				builder.appendQueryParameter("t", StringUtils.asciiLowercase(parts[1]));
				break;
		}
	}

	public void addToSubredditListingUri(@NonNull final Uri.Builder builder) {

		switch(this) {
			case HOT:
			case NEW:
			case RISING:
			case BEST:
				builder.appendEncodedPath(StringUtils.asciiLowercase(name()));
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
				builder.appendEncodedPath(StringUtils.asciiLowercase(name().split("_")[0]));
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
		((OptionsMenuUtility.OptionsMenuPostsListener)activity).onSortSelected(this);
	}
}
