/*******************************************************************************
 * This file is part of Scroll.
 *
 * Scroll is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Scroll is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Scroll.  If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/

package org.quantumbadger.redreader.reddit;

import org.quantumbadger.redreader.reddit.prepared.RedditPreparedComment;
import org.quantumbadger.redreader.reddit.prepared.RedditPreparedMoreComments;

public class RedditCommentListItem {

	public static enum Type {
		COMMENT, LOAD_MORE
	}

	private final Type mType;
	private final RedditCommentListItem mParent;
	private final int mIndent;

	private final RedditPreparedComment mComment;

	private final RedditPreparedMoreComments mMoreComments;

	public RedditCommentListItem(final RedditCommentListItem parent, final RedditPreparedComment comment) {
		mParent = parent;
		mIndent = computeIndent(parent);
		mType = Type.COMMENT;
		mComment = comment;
		mMoreComments = null;
	}

	public RedditCommentListItem(final RedditCommentListItem parent, final RedditPreparedMoreComments moreComments) {
		mParent = parent;
		mIndent = computeIndent(parent);
		mType = Type.LOAD_MORE;
		mComment = null;
		mMoreComments = moreComments;
	}

	private static int computeIndent(RedditCommentListItem parent) {
		if(parent == null) {
			return 0;
		} else {
			return parent.getIndent() + 1;
		}
	}

	public int getIndent() {
		return mIndent;
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

	public RedditPreparedMoreComments asLoadMore() {
		return mMoreComments;
	}

	public boolean isVisible() {
		return (mParent == null) || (!mParent.isCollapsed() && mParent.isVisible());
	}

	public boolean isCollapsed() {
		return mType == Type.COMMENT && mComment.isCollapsed();
	}
}
