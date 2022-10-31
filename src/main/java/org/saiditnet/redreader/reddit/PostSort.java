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

package org.saiditnet.redreader.reddit;

import android.net.Uri;
import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import org.saiditnet.redreader.common.General;

public enum PostSort {
	HOT, NEW, RISING, TOP_HOUR, TOP_DAY, TOP_WEEK, TOP_MONTH, TOP_YEAR, TOP_ALL, CONTROVERSIAL, BEST,
	// Sorts related to Search Listings
	RELEVANCE, COMMENTS, TOP;

	@Nullable
	public static PostSort valueOfOrNull(@NonNull final String string) {

		try {
			return valueOf(General.asciiUppercase(string));
		} catch(final IllegalArgumentException e) {
			return null;
		}
	}

	@Nullable
	public static PostSort parse(@Nullable String sort, @Nullable String t) {

		if(sort == null) {
			return null;
		}

		sort = General.asciiLowercase(sort);
		t = t != null ? General.asciiLowercase(t) : null;

		if(sort.equals("hot")) {
			return HOT;

		} else if(sort.equals("new")) {
			return NEW;

		} else if(sort.equals("best")) {
			return BEST;

		} else if(sort.equals("controversial") || sort.equals("funny")) {
			return CONTROVERSIAL;

		} else if(sort.equals("rising")) {
			return RISING;

		} else if(sort.equals("top")) {

			if(t == null)				return TOP_ALL;
			else if(t.equals("all"))	return TOP_ALL;
			else if(t.equals("hour"))	return TOP_HOUR;
			else if(t.equals("day"))	return TOP_DAY;
			else if(t.equals("week"))	return TOP_WEEK;
			else if(t.equals("month"))	return TOP_MONTH;
			else if(t.equals("year"))	return TOP_YEAR;
			else						return TOP_ALL;

		} else {
			return null;
		}
	}

	public void addToUserPostListingUri(@NonNull final Uri.Builder builder) {

		switch(this) {
			case HOT:
			case NEW:
				builder.appendQueryParameter("sort", General.asciiLowercase(name()));
				break;
			case CONTROVERSIAL:
				builder.appendQueryParameter("sort", "funny");
				break;
			case TOP_HOUR:
			case TOP_DAY:
			case TOP_WEEK:
			case TOP_MONTH:
			case TOP_YEAR:
			case TOP_ALL:
				final String parts[] = name().split("_");
				builder.appendQueryParameter("sort", General.asciiLowercase(parts[0]));
				builder.appendQueryParameter("t", General.asciiLowercase(parts[1]));
				break;
		}
	}

	public void addToSubredditListingUri(@NonNull final Uri.Builder builder) {

		switch(this) {
			case HOT:
			case NEW:
			case RISING:
			case BEST:
				builder.appendEncodedPath(General.asciiLowercase(name()));
				break;
			case CONTROVERSIAL:
				builder.appendEncodedPath("funny");
				break;
			case TOP_HOUR:
			case TOP_DAY:
			case TOP_WEEK:
			case TOP_MONTH:
			case TOP_YEAR:
			case TOP_ALL:
				builder.appendEncodedPath("top");
				builder.appendQueryParameter("t", General.asciiLowercase(name().split("_")[1]));
				break;
		}
	}
}
