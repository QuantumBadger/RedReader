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
import android.graphics.Color;
import android.view.View;
import android.view.ViewGroup;
import com.laurencedawson.activetextview.ActiveTextView;
import org.holoeverywhere.preference.PreferenceManager;
import org.holoeverywhere.widget.FrameLayout;
import org.holoeverywhere.widget.LinearLayout;
import org.holoeverywhere.widget.TextView;
import org.quantumbadger.redreader.R;
import org.quantumbadger.redreader.common.General;
import org.quantumbadger.redreader.common.PrefsUtility;
import org.quantumbadger.redreader.fragments.CommentListingFragment;
import org.quantumbadger.redreader.reddit.prepared.RedditPreparedComment;

public class RedditCommentView extends LinearLayout {

	private RedditPreparedComment comment;

	private final TextView header;
	private final FrameLayout bodyHolder;

	private final LinearLayout main;

	private final View leftIndent, leftDividerLine;

	private final int bodyCol;
	private final float fontScale;

	public RedditCommentView(final Context context, final int headerCol, final int bodyCol) {

		super(context);
		this.bodyCol = bodyCol;

		setOrientation(HORIZONTAL);

		main = new LinearLayout(context);
		main.setOrientation(VERTICAL);

		fontScale = PrefsUtility.appearance_fontscale_comments(context, PreferenceManager.getDefaultSharedPreferences(context));

		header = new TextView(context);
		header.setTextSize(11.0f * fontScale);
		header.setTextColor(headerCol);
		main.addView(header);

		bodyHolder = new FrameLayout(context);
		bodyHolder.setPadding(0, General.dpToPixels(context, 2), 0, 0);
		main.addView(bodyHolder);

		final int paddingPixelsVertical = General.dpToPixels(context, 8.0f);
		final int paddingPixelsHorizontal = General.dpToPixels(context, 12.0f);
		main.setPadding(paddingPixelsHorizontal, paddingPixelsVertical, paddingPixelsHorizontal, paddingPixelsVertical);

		setDescendantFocusability(FOCUS_BLOCK_DESCENDANTS);

		leftIndent = new View(context);
		addView(leftIndent);

		leftIndent.getLayoutParams().height = ViewGroup.LayoutParams.MATCH_PARENT;
		leftIndent.setBackgroundColor(Color.argb(20, 128, 128, 128));

		leftDividerLine = new View(context);
		addView(leftDividerLine);

		leftDividerLine.getLayoutParams().width = General.dpToPixels(context, 2);
		leftDividerLine.getLayoutParams().height = ViewGroup.LayoutParams.MATCH_PARENT;
		leftDividerLine.setBackgroundColor(Color.argb(75, 128, 128, 128));

		addView(main);
	}

	public void reset(final Context context, final CommentListingFragment fragment, final RedditPreparedComment comment, final ActiveTextView.OnLinkClickedListener listener) {

		if(this.comment != null) this.comment.unbind(this);

		this.comment = comment;
		comment.bind(this);

		final int paddingPixelsPerIndent = General.dpToPixels(context, 10.0f); // TODO Add in vertical lines?
		leftIndent.getLayoutParams().width = paddingPixelsPerIndent * comment.indentation;
		leftDividerLine.setVisibility(comment.indentation == 0 ? GONE : VISIBLE);

		if(!comment.isCollapsed()) {
			header.setText(comment.header);
		} else {
			header.setText("[ + ]  " + comment.header);
		}

		bodyHolder.removeAllViews();
		bodyHolder.addView(comment.getBody(context, 13.0f * fontScale, bodyCol, new ActiveTextView.OnLinkClickedListener() {

			public void onClickUrl(String url) {
				listener.onClickUrl(url);
			}

			public void onClickText(Object attachment) {

				// TODO separate preference for comment body click?

				switch(PrefsUtility.pref_behaviour_actions_comment_tap(context, PreferenceManager.getDefaultSharedPreferences(context))) {
					case COLLAPSE:
						fragment.handleCommentVisibilityToggle(RedditCommentView.this);
						break;
					case ACTION_MENU:
						fragment.openContextMenu(RedditCommentView.this);
						break;
				}

			}
		}));

		updateVisibility(context);
	}

	private void updateVisibility(final Context context) {

		if(comment.isCollapsed()) {

			bodyHolder.setVisibility(GONE);

			if(comment.replyCount() == 1) {
				header.setText(String.format("[ + ] %s (1 %s)", comment.header, context.getString(R.string.subtitle_reply)));
			} else {
				header.setText(String.format("[ + ] %s (%d %s)", comment.header, comment.replyCount(), context.getString(R.string.subtitle_replies)));
			}

		} else {
			bodyHolder.setVisibility(VISIBLE);
			header.setText(comment.header);
		}
	}

	public boolean handleVisibilityToggle() {
		comment.toggleVisibility();
		updateVisibility(getContext());
		return comment.isCollapsed();
	}

	public RedditPreparedComment getComment() {
		return comment;
	}

	public void updateAppearance() {
		header.setText(comment.header);
	}
}
