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

package org.quantumbadger.redreader.reddit.prepared;

import androidx.annotation.NonNull;

import org.quantumbadger.redreader.activities.BaseActivity;
import org.quantumbadger.redreader.reddit.kthings.RedditIdAndType;

public interface RedditRenderableInboxItem extends RedditRenderableCommentListItem {
	void handleInboxClick(BaseActivity activity);

	void handleInboxLongClick(BaseActivity activity);

	@NonNull
	RedditIdAndType getIdAndType();

	/**
	 * Whether the current user is able to reply to this item.
	 */
	boolean canReply(BaseActivity activity);

	void handleInboxReply(
			BaseActivity activity,
			RedditChangeDataManager changeDataManager);

	/**
	 * Whether this item has surrounding context (e.g. a comment thread) which
	 * can be opened.
	 */
	boolean hasContext();

	void handleInboxContext(BaseActivity activity);
}
