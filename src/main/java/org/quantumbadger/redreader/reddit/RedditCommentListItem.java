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

import android.content.Context;
import androidx.appcompat.app.AppCompatActivity;
import androidx.recyclerview.widget.RecyclerView;
import android.view.View;
import android.view.ViewGroup;
import org.quantumbadger.redreader.account.RedditAccountManager;
import org.quantumbadger.redreader.adapters.GroupedRecyclerViewAdapter;
import org.quantumbadger.redreader.common.RRThemeAttributes;
import org.quantumbadger.redreader.fragments.CommentListingFragment;
import org.quantumbadger.redreader.reddit.prepared.RedditChangeDataManager;
import org.quantumbadger.redreader.reddit.prepared.RedditRenderableComment;
import org.quantumbadger.redreader.reddit.things.RedditMoreComments;
import org.quantumbadger.redreader.reddit.url.RedditURLParser;
import org.quantumbadger.redreader.views.LoadMoreCommentsView;
import org.quantumbadger.redreader.views.RedditCommentView;

public class RedditCommentListItem extends GroupedRecyclerViewAdapter.Item {

	public enum Type {
		COMMENT, LOAD_MORE
	}

	private final Type mType;

	private final int mIndent;
	private final RedditCommentListItem mParent;
	private final CommentListingFragment mFragment;
	private final AppCompatActivity mActivity;
	private final RedditURLParser.RedditURL mCommentListingUrl;

	private final RedditRenderableComment mComment;
	private final RedditMoreComments mMoreComments;

	private final RedditChangeDataManager mChangeDataManager;

	public RedditCommentListItem(
			final RedditRenderableComment comment,
			final RedditCommentListItem parent,
			final CommentListingFragment fragment,
			final AppCompatActivity activity,
			final RedditURLParser.RedditURL commentListingUrl) {

		mParent = parent;
		mFragment = fragment;
		mActivity = activity;
		mCommentListingUrl = commentListingUrl;
		mType = Type.COMMENT;
		mComment = comment;
		mMoreComments = null;

		if(parent == null) {
			mIndent = 0;
		} else {
			mIndent = parent.getIndent() + 1;
		}

		mChangeDataManager = RedditChangeDataManager.getInstance(
				RedditAccountManager.getInstance(activity).getDefaultAccount());
	}

	public RedditCommentListItem(
			final RedditMoreComments moreComments,
			final RedditCommentListItem parent,
			final CommentListingFragment fragment,
			final AppCompatActivity activity,
			final RedditURLParser.RedditURL commentListingUrl) {

		mParent = parent;
		mFragment = fragment;
		mActivity = activity;
		mCommentListingUrl = commentListingUrl;
		mType = Type.LOAD_MORE;
		mComment = null;
		mMoreComments = moreComments;

		if(parent == null) {
			mIndent = 0;
		} else {
			mIndent = parent.getIndent() + 1;
		}

		mChangeDataManager = RedditChangeDataManager.getInstance(
				RedditAccountManager.getInstance(activity).getDefaultAccount());
	}

	public boolean isComment() {
		return mType == Type.COMMENT;
	}

	public boolean isLoadMore() {
		return mType == Type.LOAD_MORE;
	}

	public RedditRenderableComment asComment() {

		if(!isComment()) {
			throw new RuntimeException("Called asComment() on non-comment item");
		}

		return mComment;
	}

	public RedditMoreComments asLoadMore() {

		if(!isLoadMore()) {
			throw new RuntimeException("Called asLoadMore() on non-load-more item");
		}

		return mMoreComments;
	}

	public int getIndent() {
		return mIndent;
	}

	public RedditCommentListItem getParent() {
		return mParent;
	}

	public boolean isCollapsed(final RedditChangeDataManager changeDataManager) {

		if(!isComment()) {
			return false;
		}

		return mComment.isCollapsed(changeDataManager);

	}

	public boolean isHidden(final RedditChangeDataManager changeDataManager) {

		if(mParent != null) {
			return mParent.isCollapsed(changeDataManager) || mParent.isHidden(changeDataManager);
		}

		return false;
	}

	@Override
	public Class getViewType() {

		if(isComment()) {
			return RedditCommentView.class;
		}

		if(isLoadMore()) {
			return LoadMoreCommentsView.class;
		}

		throw new RuntimeException("Unknown item type");
	}

	@Override
	public RecyclerView.ViewHolder onCreateViewHolder(final ViewGroup viewGroup) {

		final Context context = viewGroup.getContext();
		final View view;

		if(isComment()) {
			view = new RedditCommentView(
					mActivity,
					new RRThemeAttributes(context),
					mFragment,
					mFragment);

		} else if(isLoadMore()) {
			view = new LoadMoreCommentsView(
					context,
					mCommentListingUrl);

		} else {
			throw new RuntimeException("Unknown item type");
		}

		return new RecyclerView.ViewHolder(view) {};
	}

	@Override
	public void onBindViewHolder(final RecyclerView.ViewHolder viewHolder) {

		if(isComment()) {
			((RedditCommentView)viewHolder.itemView).reset(mActivity, this);

		} else if(isLoadMore()) {
			((LoadMoreCommentsView)viewHolder.itemView).reset(this);

		} else {
			throw new RuntimeException("Unknown item type");
		}
	}

	@Override
	public boolean isHidden() {
		return isHidden(mChangeDataManager);
	}

}
