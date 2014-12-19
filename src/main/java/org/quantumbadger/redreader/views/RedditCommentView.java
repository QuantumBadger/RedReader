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

package org.quantumbadger.redreader.views;

import android.content.Context;
import android.view.ViewGroup;
import org.holoeverywhere.app.Activity;
import org.holoeverywhere.preference.PreferenceManager;
import org.holoeverywhere.widget.FrameLayout;
import org.holoeverywhere.widget.LinearLayout;
import org.holoeverywhere.widget.TextView;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.reddit.prepared.RedditPreparedComment;

public class RedditCommentView extends LinearLayout {

	private RedditPreparedComment mComment;

	private final TextView mHeader;
	private final FrameLayout mBodyHolder;

	private final IndentView mIndentView;

	private final int mBodyCol;
	private final float mFontScale;

	private final boolean mShowLinkButtons;

	private final CommentClickListener mClickListener;

	public static interface CommentClickListener {
		public void onCommentClicked(RedditCommentView view);
	}

	public RedditCommentView(final Context context, final int headerCol, final int bodyCol, final CommentClickListener clickListener) {

		super(context);
		this.mBodyCol = bodyCol;
		mClickListener = clickListener;

		setOrientation(HORIZONTAL);

		LinearLayout main = new LinearLayout(context);
		main.setOrientation(VERTICAL);

		mFontScale = PrefsUtility.appearance_fontscale_comments(context, PreferenceManager.getDefaultSharedPreferences(context));

		mHeader = new TextView(context);
		mHeader.setTextSize(11.0f * mFontScale);
		mHeader.setTextColor(headerCol);
		main.addView(mHeader);

		mBodyHolder = new FrameLayout(context);
		mBodyHolder.setPadding(0, General.dpToPixels(context, 2), 0, 0);
		main.addView(mBodyHolder);
		mBodyHolder.getLayoutParams().width = ViewGroup.LayoutParams.MATCH_PARENT;

		final int paddingPixelsVertical = General.dpToPixels(context, 8.0f);
		final int paddingPixelsHorizontal = General.dpToPixels(context, 12.0f);
		main.setPadding(paddingPixelsHorizontal, paddingPixelsVertical, paddingPixelsHorizontal, paddingPixelsVertical);

		setDescendantFocusability(FOCUS_BLOCK_DESCENDANTS);

		mIndentView = new IndentView(context);
		addView(mIndentView);
		mIndentView.getLayoutParams().height = ViewGroup.LayoutParams.MATCH_PARENT;

		addView(main);
		main.getLayoutParams().width = LinearLayout.LayoutParams.MATCH_PARENT;

		mShowLinkButtons = PrefsUtility.pref_appearance_linkbuttons(context, PreferenceManager.getDefaultSharedPreferences(context));
	}

	public void notifyClick() {
		mClickListener.onCommentClicked(this);
	}

	public void reset(final Activity activity, final RedditPreparedComment comment, final int indent) {

		if(comment == mComment) {
			comment.bind(this);
			return;
		}

		if(mComment != null) {
			mComment.unbind(this);
		}

		mComment = comment;
		comment.bind(this);

		mIndentView.setIndentation(indent);

		if(!comment.isCollapsed()) {
			mHeader.setText(comment.header);
		} else {
			mHeader.setText("[ + ]  " + comment.header);
		}

		final boolean hideLinkButtons = comment.src.author.equalsIgnoreCase("autowikibot");

		mBodyHolder.removeAllViews();
		final ViewGroup commentBody = comment.getBody(activity, 13.0f * mFontScale, mBodyCol, mShowLinkButtons && !hideLinkButtons);

		mBodyHolder.addView(commentBody);
		commentBody.getLayoutParams().width = ViewGroup.LayoutParams.MATCH_PARENT;
		((MarginLayoutParams)commentBody.getLayoutParams()).topMargin = General.dpToPixels(activity, 1);

		updateVisibility(activity);
	}

	private void updateVisibility(final Context context) {

		if(mComment.isCollapsed()) {

			mBodyHolder.setVisibility(GONE);

			if(mComment.replyCount() == 1) {
				mHeader.setText(String.format("[ + ] %s (1 %s)", mComment.header, context.getString(R.string.subtitle_reply)));
			} else {
				mHeader.setText(String.format("[ + ] %s (%d %s)", mComment.header, mComment.replyCount(), context.getString(R.string.subtitle_replies)));
			}

		} else {
			mBodyHolder.setVisibility(VISIBLE);
			mHeader.setText(mComment.header);
		}
	}

	public boolean handleVisibilityToggle() {
		mComment.toggleVisibility();
		updateVisibility(getContext());
		return mComment.isCollapsed();
	}

	public RedditPreparedComment getComment() {
		return mComment;
	}

	public void updateAppearance() {
		mHeader.setText(mComment.header);
	}
}
