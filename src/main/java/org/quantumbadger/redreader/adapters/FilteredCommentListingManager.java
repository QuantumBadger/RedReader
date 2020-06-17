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

package org.quantumbadger.redreader.adapters;

import android.content.Context;
import androidx.annotation.Nullable;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.reddit.RedditCommentListItem;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;

public class FilteredCommentListingManager extends RedditListingManager {

	@Nullable
	private final String mSearchString;

	private int mCommentCount;

	public FilteredCommentListingManager(
			final Context context,
			@Nullable final String searchString) {

		super(context);
		mSearchString = searchString;
	}

	public void addComments(final Collection<RedditCommentListItem> comments) {
		final Collection<GroupedRecyclerViewAdapter.Item> filteredComments = filter(comments);
		addItems(filteredComments);
		mCommentCount += filteredComments.size();
	}

	private Collection<GroupedRecyclerViewAdapter.Item> filter(Collection<RedditCommentListItem> comments) {

		final Collection<RedditCommentListItem> searchComments;

		if (mSearchString == null) {
			searchComments = comments;

		} else {
		 	searchComments = new ArrayList<>();
			for (RedditCommentListItem comment : comments) {
				if (!comment.isComment()) continue;
				String commentStr = comment.asComment().getParsedComment().getRawComment().body;
				if (commentStr != null) {
					commentStr = General.asciiLowercase(commentStr);
					if (commentStr.contains(mSearchString)) {
						searchComments.add(comment);
					}
				}
			}
		}

		return Collections.<GroupedRecyclerViewAdapter.Item>unmodifiableCollection(searchComments);
	}

	public boolean isSearchListing() {
		return mSearchString != null;
	}

	public int getCommentCount() {
		return mCommentCount;
	}
}
