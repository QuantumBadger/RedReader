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
import org.quantumbadger.redreader.common.StringUtils;

public enum PostSort {
	HOT,
	NEW,
	RISING,
	TOP_HOUR,
	TOP_DAY,
	TOP_WEEK,
	TOP_MONTH,
	TOP_YEAR,
	TOP_ALL,
	CONTROVERSIAL_HOUR,
	CONTROVERSIAL_DAY,
	CONTROVERSIAL_WEEK,
	CONTROVERSIAL_MONTH,
	CONTROVERSIAL_YEAR,
	CONTROVERSIAL_ALL,
	BEST,
	// Sorts related to Search Listings
	RELEVANCE_HOUR,
	RELEVANCE_DAY,
	RELEVANCE_WEEK,
	RELEVANCE_MONTH,
	RELEVANCE_YEAR,
	RELEVANCE_ALL,
	NEW_HOUR,
	NEW_DAY,
	NEW_WEEK,
	NEW_MONTH,
	NEW_YEAR,
	NEW_ALL,
	COMMENTS_HOUR,
	COMMENTS_DAY,
	COMMENTS_WEEK,
	COMMENTS_MONTH,
	COMMENTS_YEAR,
	COMMENTS_ALL,
	HOT_HOUR,
	HOT_DAY,
	HOT_WEEK,
	HOT_MONTH,
	HOT_YEAR,
	HOT_ALL;

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
}
