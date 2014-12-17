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

import org.quantumbadger.redreader.reddit.prepared.RedditPreparedComment;
import org.quantumbadger.redreader.reddit.url.PostCommentListingURL;

public class RedditCommentListItem {

	public static enum Type {
		COMMENT, LOAD_MORE
	}

	private final Type mType;
	private final RedditCommentListItem mParent;

	private final RedditPreparedComment mComment;

	private final PostCommentListingURL mLoadMoreUrl;

	public RedditCommentListItem(final RedditCommentListItem parent, final RedditPreparedComment comment) {
		mParent = parent;
		mType = Type.COMMENT;
		mComment = comment;
		mLoadMoreUrl = null;
	}

	public RedditCommentListItem(final RedditCommentListItem parent, final PostCommentListingURL loadMoreUrl) {
		mParent = parent;
		mType = Type.LOAD_MORE;
		mComment = null;
		mLoadMoreUrl = loadMoreUrl;
	}

	public boolean isComment() {
		return mType == Type.COMMENT;
	}

	public boolean isLoadMore() {
		return mType == Type.LOAD_MORE;
	}

	public RedditPreparedComment asComment() {
		return mComment;
	}

	public PostCommentListingURL asLoadMore() {
		return mLoadMoreUrl;
	}

	public boolean isVisible() {
		return (mParent == null) || (!mParent.isCollapsed() && mParent.isVisible());
	}

	public boolean isCollapsed() {
		return mType == Type.COMMENT && mComment.isCollapsed();
	}
}
